use gospel::write::{Label, Le as _};
use snafu::{ensure, OptionExt as _, ResultExt as _};

use crate::func::Expr;
use crate::{WriterExt, VWriter};

use super::spec::Part;
use super::{Arg, Case, Op, OpMeta, Stmt, SPEC};

// TODO unhardcode control flow ops

#[extend::ext]
impl VWriter {
	fn goto(&mut self, meta: &OpMeta, label: Label, width: u8) {
		self.u8(0x03);
		self.fake_meta(meta, || width);
		self.label(label)
	}

	fn fake_meta(&mut self, meta: &OpMeta, width: impl FnOnce() -> u8) {
		self.u16(meta.line);
		self.u8(0);
		self.u8(if meta.width == 0xFF { 0xFF } else { width() });
	}

	#[track_caller]
	fn meta(&mut self, meta: &OpMeta) -> Label {
		let meta = *meta;
		let label = self.here();
		let label2 = Label::new();
		self.u16(meta.line);
		self.u8(0);
		if meta.width != 0xFF {
			self.delay(move |x| {
				let v = (x.get(label, label2) + 1).min(255) as u8;
				if v != meta.width {
					tracing::warn!("expected width {}, got {} on {}", meta.width, v, meta.line);
				}
				Some([v])
			});
		} else {
			self.u8(0xFF);
		}
		label2
	}

	#[track_caller]
	fn label(&mut self, label: Label) {
		let start = self.start;
		self.label32(start, label);
	}
}

#[derive(Debug, snafu::Snafu)]
pub enum WriteError {
	#[snafu(display("break outside of loop/switch at {meta:?}"))]
	UnexpectedBreak { meta: OpMeta },
	#[snafu(display("continue outside of loop at {meta:?}"))]
	UnexpectedContinue { meta: OpMeta },
	#[snafu(display("duplicate default case at {meta:?}"))]
	DuplicateDefault { meta: OpMeta },

	#[snafu(transparent, context(false))]
	Expr { source: super::expr::WriteError },
	#[snafu(transparent, context(false))]
	Op { source: OpWriteError },
}

pub(crate) fn write(f: &mut VWriter, code: &[Stmt]) -> Result<(), WriteError> {
	block(f, code, None, None)
}

fn expr_width(expr: &Expr) -> u8 {
	fn inner(expr: &Expr) -> u8 {
		match expr {
			Expr::Int(_) => 5,
			Expr::Op(_) => 1,
			Expr::Flag(_) => 8, // ???
			Expr::Bin(_, a, b) => 1u8.saturating_add(inner(a)).saturating_add(inner(b)),
			Expr::Un(_, a) => 1u8.saturating_add(inner(a)),
			_ => {
				tracing::warn!("don't know width of {expr:?}");
				0
			}
		}
	}
	1u8.saturating_add(inner(expr))
}

fn block(f: &mut VWriter, code: &[Stmt], brk: Option<Label>, cont: Option<Label>) -> Result<(), WriteError> {
	for stmt in code {
		match stmt {
			Stmt::Op(op) => write_raw_op(f, op)?,
			Stmt::If(meta, expr, thn, els) => {
				let l = Label::new();
				f.u8(0x05);
				f.fake_meta(meta, || expr_width(expr));
				expr.write(f)?;
				f.label(l);

				block(f, thn, brk, cont)?;
				if let Some((em, els)) = els {
					let l2 = Label::new();
					f.goto(em, l2, 5);
					f.place(l);
					block(f, els, brk, cont)?;
					f.place(l2);
				} else {
					f.place(l);
				}
			}

			Stmt::While(meta, expr, body, meta2) => {
				let [cont, brk] = Label::many();
				f.place(cont);
				f.u8(0x05);
				f.fake_meta(meta, || expr_width(expr));
				expr.write(f)?;
				f.label(brk);

				block(f, body, Some(brk), Some(cont))?;

				f.goto(meta2, cont, 5);
				f.place(brk);
			},
			Stmt::Break(meta) => {
				if let Some(label) = brk {
					f.goto(meta, label, 4);
				} else {
					return UnexpectedBreakSnafu { meta: *meta }.fail();
				}
			}
			Stmt::Continue(meta) => {
				if let Some(label) = cont {
					f.goto(meta, label, 4);
				} else {
					return UnexpectedContinueSnafu { meta: *meta }.fail();
				}
			}
			Stmt::Switch(meta, expr, arms) => {
				let mut cases = Vec::new();
				let mut default = None;
				let mut bodies = Vec::new();

				for (case, body) in arms {
					let l = Label::new();
					bodies.push((l, body));
					match case {
						Case::Default => {
							ensure!(default.is_none(), DuplicateDefaultSnafu { meta: *meta });
							default = Some(l);
						}
						Case::Case(i) => {
							cases.push((*i, l));
						}
						Case::None => {}
					}
				}

				let brk = Label::new();
				f.u8(0x06);
				f.fake_meta(meta, || expr_width(expr));
				expr.write(f)?;
				f.u8(cases.len() as u8);
				for (case, l) in cases {
					f.i32(case);
					f.label(l);
				}
				f.label(default.unwrap_or(brk));

				for (l, body) in bodies {
					f.place(l);
					block(f, body, Some(brk), cont)?;
				}
				f.place(brk);
			},

			Stmt::ForkLambda(..) => panic!("can't write ForkLambda, it should have been desugared"),
		}
	}
	Ok(())
}

#[derive(Debug, snafu::Snafu)]
#[snafu(display("couldn't write op: {op:?}"))]
pub struct OpWriteError {
	pub op: Op,
	pub source: OpWriteErrorKind,
}

#[derive(Debug, snafu::Snafu)]
pub enum OpWriteErrorKind {
	#[snafu(transparent, context(false))]
	Value { source: crate::ValueError },
	#[snafu(display("unknown opcode"))]
	UnknownOp,
	#[snafu(display("too many arguments: {remaining} remaining"))]
	TooManyArgs { remaining: usize },
	#[snafu(display("expected {what}, got {got:?}"))]
	ExpectedArg { what: &'static str, got: Option<Arg> },

	#[snafu(transparent, context(false))]
	Dialogue { source: super::dial::WriteError },
	#[snafu(transparent, context(false))]
	Expr { source: super::expr::WriteError },
}

pub(crate) fn write_raw_op(f: &mut VWriter, op: &Op) -> Result<(), OpWriteError> {
	write_raw_op_inner(f, op).with_context(|_| OpWriteSnafu { op: op.clone() })
}

fn write_raw_op_inner(f: &mut VWriter, op: &Op) -> Result<(), OpWriteErrorKind> {
	let code = SPEC.by_name.get(op.name).context(UnknownOpSnafu)?;
	let mut args = op.args.iter();
	let mut n = 0;
	let mut spec = SPEC.ops[code[n] as usize].as_ref().context(UnknownOpSnafu)?;
	f.u8(code[n]);
	let mut end = if matches!(code[n], 0x01 | 0x04) {
		Label::new()
	} else {
		f.meta(&op.meta)
	};
	loop {
		write_parts(op, &mut end, f, &spec.parts, &mut args)?;
		n += 1;
		if n == code.len() {
			break;
		}
		f.u8(code[n]);
		spec = spec.child(code[n]).context(UnknownOpSnafu)?;
	}
	f.place(end);
	snafu::ensure!(!spec.has_children(), UnknownOpSnafu);
	snafu::ensure!(args.as_slice().is_empty(), TooManyArgsSnafu { remaining: args.as_slice().len() });
	Ok(())
}

macro_rules! next {
	($args:expr, $ty:ident) => {
		*match $args.next() {
			Some(Arg::$ty(arg)) => arg,
			v => return ExpectedArgSnafu { what: stringify!($ty), got: v.cloned() }.fail(),
		}
	};
	($args:expr, $n:expr, $ty:ident) => {
		match $args.args.get($n) {
			Some(Arg::$ty(arg)) => *arg,
			v => return ExpectedArgSnafu { what: stringify!($ty), got: v.cloned() }.fail(),
		}
	};
}

fn write_parts(op: &Op, end: &mut Label, f: &mut VWriter, parts: &[Part], args: &mut std::slice::Iter<Arg>) -> Result<(), OpWriteErrorKind> {
	use super::spec::Part::*;
	for part in parts {
		match part {
			U8 => f.u8(next!(args, U8)),
			U16 => f.u16(next!(args, U16)),
			U32 => f.u32(next!(args, U32)),
			I8 => f.i8(next!(args, I8)),
			I16 => f.i16(next!(args, I16)),
			I32 => f.i32(next!(args, I32)),
			F32 => {
				if let Some(Arg::F32Munged(v)) = args.as_slice().first() {
					args.next();
					f.f32(f32::from_bits(*v as u32));
				} else {
					f.f32(next!(args, F32));
				}
			}
			Str => f.str(&next!(args, Str))?,

			Char => f.u16(next!(args, Char).0),
			Item => f.u16(next!(args, Item).0),
			Magic => f.u16(next!(args, Magic).0),
			Flag => f.u16(next!(args, Flag).0),
			Global => f.u8(next!(args, Global).0),
			Var => f.u8(next!(args, Var).0),
			FuncArg => f.u8(next!(args, FuncArg).0),
			NumReg => f.u8(next!(args, NumReg).0),
			StrReg => f.u8(next!(args, StrReg).0),
			Attr => f.u8(next!(args, Attr).0),
			CharAttr => { let c = next!(args, CharAttr); f.u16(c.0.0); f.u8(c.1); }
			Flags8 => f.u8(next!(args, Flags8).0),
			Flags16 => f.u16(next!(args, Flags16).0),
			Flags32 => f.u32(next!(args, Flags32).0),

			Text => next!(args, Dialogue).write(f)?,
			Expr => {
				f.place(*end);
				*end = Label::new();
				next!(args, Expr).write(f)?
			},
			Dyn => write_dyn(f, args.next())?,
			Ndyn => {
				let n = args.as_slice().len();
				snafu::ensure!(n < 256, TooManyArgsSnafu { remaining: n - 256 });
				f.u8(n as u8);
				for _ in 0..n {
					write_dyn(f, args.next())?;
				}
			}

			Part::F32Opt => {
				if args.as_slice().first() == Some(&Arg::I32(-1)) {
					args.next();
					f.i32(-1);
				} else {
					write_parts(op, end, f, &[F32], args)?;
				}
			}

			Part::_40 => write_parts(op, end, f, super::spec::op_40(next!(op, 1, Char)), args)?,
			Part::_98 => write_parts(op, end, f, super::spec::op_98(next!(op, 0, U16)), args)?,
			Part::_C0 => write_parts(op, end, f, super::spec::op_c0(next!(op, 0, U16)), args)?,
			Part::_D2 => write_parts(op, end, f, super::spec::op_d2(next!(op, 0, I16)), args)?,

			Part::_3E => {
				let a = next!(op, 1, Char).0;
				if a == 0xFE12 {
					write_parts(op, end, f, &[U8], args)?;
				} else if a == 0xFE13 {
					write_parts(op, end, f, &[F32], args)?;
				}

				if op.args[1..4] == [Arg::Char(0xFFFF.into()), Arg::F32(5.0), Arg::U8(0)] {
					f.slice(&[0, 0, 0]);
				}
			}
			Part::_3F => {
				if f.version != 2 {
					write_parts(op, end, f, &[U32], args)?;
				}
			}
			Part::_4E => {
				if f.version != 2 {
					write_parts(op, end, f, &[U8], args)?;
				}
			}
			Part::_6C => {
				if f.version != 2 {
					write_parts(op, end, f, &[I32], args)?;
				}
			}
			Part::_79 => {
				let a = next!(op, 0, U8);
				if a == 7 {
					write_parts(op, end, f, &[U8, U8], args)?;
				}
			}
			Part::_AB00 => {
				let mut slice = [0; 50];
				for v in slice.iter_mut().take(args.as_slice().len()) {
					*v = next!(args, U8);
				}
				f.slice(&slice);
			}
			Part::_AB02 => {
				let n = args.as_slice().len();
				snafu::ensure!(n <= 50, TooManyArgsSnafu { remaining: n - 50 });
				f.u8(n as u8);
				for _ in 0..n {
					f.u16(next!(args, U16));
				}
				for _ in n..50 {
					f.u16(0);
				}
			}
		}
	}
	Ok(())
}

fn write_dyn(f: &mut VWriter, arg: Option<&Arg>) -> Result<(), OpWriteErrorKind> {
	match arg {
		Some(Arg::Var(v)) => { f.u8(0x11); f.u8(v.0); f.u32(0); }
		Some(Arg::NumReg(v)) => { f.u8(0x33); f.u8(v.0); f.u32(0); }
		Some(Arg::StrReg(v)) => { f.u8(0x44); f.u8(v.0); f.u32(0); }
		Some(Arg::Global(v)) => { f.u8(0x55); f.u8(v.0); f.u32(0); }
		Some(Arg::Str(v)) => { f.u8(0xDD); f.str(v)?; }
		Some(Arg::F32(v)) => { f.u8(0xEE); f.f32(*v); f.u8(0); }
		Some(Arg::I32(v)) => { f.u8(0xFF); f.i32(*v); f.u8(0); }
		Some(Arg::I32Munged(v)) => { f.u8(0xFF); f.f32(*v); f.u8(0); }
		v => return ExpectedArgSnafu { what: "dyn", got: v.cloned() }.fail(),
	}
	Ok(())
}

use gospel::write::{Label, Le as _};
use snafu::{ensure, OptionExt as _, ResultExt as _};

use crate::{WriterExt, VWriter};

use super::spec::Part;
use super::{Arg, Dyn, Case, Op, OpMeta, Stmt, SPEC};

// TODO unhardcode control flow ops

#[extend::ext]
impl VWriter {
	fn goto(&mut self, meta: &OpMeta, label: Label) {
		self.u8(0x03);
		self.meta(meta);
		self.label(label)
	}

	fn meta(&mut self, meta: &OpMeta) {
		self.u16(meta.line);
		self.u8(0);
		self.u8(meta.width); // TODO compute width automatically
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

fn block(f: &mut VWriter, code: &[Stmt], brk: Option<Label>, cont: Option<Label>) -> Result<(), WriteError> {
	for stmt in code {
		match stmt {
			Stmt::Op(op) => write_raw_op(f, op)?,
			Stmt::If(meta, expr, thn, els) => {
				let l = Label::new();
				f.u8(0x05);
				f.meta(meta);
				expr.write(f)?;
				f.label(l);

				block(f, thn, brk, cont)?;
				if let Some((em, els)) = els {
					let l2 = Label::new();
					f.goto(em, l2);
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
				f.meta(meta);
				expr.write(f)?;
				f.label(brk);

				block(f, body, Some(brk), Some(cont))?;

				f.goto(meta2, cont);
				f.place(brk);
			},
			Stmt::Break(meta) => {
				if let Some(label) = brk {
					f.goto(meta, label);
				} else {
					return UnexpectedBreakSnafu { meta: *meta }.fail();
				}
			}
			Stmt::Continue(meta) => {
				if let Some(label) = cont {
					f.goto(meta, label);
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
				f.meta(meta);
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
	#[snafu(display("too few arguments"))]
	ExpectedArg { what: &'static str },

	#[snafu(transparent, context(false))]
	Dialogue { source: super::dial::WriteError },
	#[snafu(transparent, context(false))]
	Expr { source: super::expr::WriteError },
}

pub(crate) fn write_raw_op(f: &mut VWriter, op: &Op) -> Result<(), OpWriteError> {
	write_raw_op_inner(f, op).with_context(|_| OpWriteSnafu { op: op.clone() })
}

fn write_raw_op_inner(f: &mut VWriter, op: &Op) -> Result<(), OpWriteErrorKind> {
	let mut args = op.args.iter();
	assert!(!op.code.is_empty());
	let mut n = 0;
	let mut spec = SPEC.ops[op.code[n] as usize].as_ref().context(UnknownOpSnafu)?;
	f.u8(op.code[n]);
	if !matches!(op.code[n], 0x01 | 0x04) {
		f.meta(&op.meta);
	}
	loop {
		write_parts(op, f, &spec.parts, &mut args)?;
		n += 1;
		if n == op.code.len() {
			break;
		}
		f.u8(op.code[n]);
		spec = spec.child(op.code[n]).context(UnknownOpSnafu)?;
	}
	snafu::ensure!(!spec.has_children(), UnknownOpSnafu);
	snafu::ensure!(args.as_slice().is_empty(), TooManyArgsSnafu { remaining: args.as_slice().len() });
	Ok(())
}

macro_rules! next {
	($args:expr, $ty:ident) => {
		*match $args.next() {
			Some(Arg::$ty(arg)) => arg,
			_ => return ExpectedArgSnafu { what: stringify!($ty) }.fail(),
		}
	};
	($args:expr, $n:expr, $ty:ident) => {
		match $args.args.get($n) {
			Some(Arg::$ty(arg)) => *arg,
			_ => return ExpectedArgSnafu { what: stringify!($ty) }.fail(),
		}
	};
}

fn write_parts(op: &Op, f: &mut VWriter, parts: &[Part], args: &mut std::slice::Iter<Arg>) -> Result<(), OpWriteErrorKind> {
	use super::spec::Part::*;
	for part in parts {
		match part {
			U8 => f.u8(next!(args, U8)),
			U16 => f.u16(next!(args, U16)),
			U32 => f.u32(next!(args, U32)),
			I8 => f.i8(next!(args, I8)),
			I16 => f.i16(next!(args, I16)),
			I32 => f.i32(next!(args, I32)),
			F32 => f.f32(next!(args, F32)),
			Str => f.str(&next!(args, Str))?,

			Char => f.u16(next!(args, Char).0),
			Item => f.u16(next!(args, Item).0),
			Flag => f.u16(next!(args, Flag).0),
			Global => f.u8(next!(args, Global).0),
			Var => f.u8(next!(args, Var).0),
			Attr => f.u8(next!(args, Attr).0),
			CharAttr => { let c = next!(args, CharAttr); f.u16(c.0.0); f.u8(c.1); }
			Flags16 => f.u16(next!(args, Flags16).0),
			Flags32 => f.u32(next!(args, Flags32).0),

			Text => next!(args, Dialogue).write(f)?,
			Expr => next!(args, Expr).write(f)?,
			Dyn => write_dyn(f, &next!(args, Dyn))?,
			Dyn2 => write_dyn(f, &next!(args, Dyn))?,
			Ndyn => {
				let n = args.as_slice().len();
				snafu::ensure!(n < 256, TooManyArgsSnafu { remaining: n - 256 });
				f.u8(n as u8);
				for _ in 0..n {
					write_dyn(f, &next!(args, Dyn))?;
				}
			}

			Part::_40 => write_parts(op, f, super::spec::op_40(next!(op, 1, Char)), args)?,
			Part::_98 => write_parts(op, f, super::spec::op_98(next!(op, 0, U16)), args)?,
			Part::_C0 => write_parts(op, f, super::spec::op_c0(next!(op, 0, U16)), args)?,
			Part::_D2 => write_parts(op, f, super::spec::op_d2(next!(op, 0, I16)), args)?,

			Part::_3E => {
				let a = next!(op, 1, Char).0;
				if a == 0xFE12 {
					write_parts(op, f, &[U8], args)?;
				} else if a == 0xFE13 {
					write_parts(op, f, &[F32], args)?;
				}

				if op.args[1..4] == [Arg::Char(0xFFFF.into()), Arg::F32(5.0), Arg::U8(0)] {
					f.slice(&[0, 0, 0]);
				}
			}
			Part::_3F => {
				if f.version != 2 {
					write_parts(op, f, &[U32], args)?;
				}
			}
			Part::_4E => {
				if f.version != 2 {
					write_parts(op, f, &[U8], args)?;
				}
			}
			Part::_6C => {
				if f.version != 2 {
					write_parts(op, f, &[I32], args)?;
				}
			}
			Part::_79 => {
				let a = next!(op, 0, U8);
				if a == 7 {
					write_parts(op, f, &[U8, U8], args)?;
				}
			}
			Part::_AB01 => {
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

fn write_dyn(f: &mut VWriter, arg: &Dyn) -> Result<(), OpWriteErrorKind> {
	match *arg {
		Dyn::_11(v) => { f.u8(0x11); f.u32(v); f.u8(0); }
		Dyn::_33(v) => { f.u8(0x33); f.u32(v); f.u8(0); }
		Dyn::_44(v) => { f.u8(0x44); f.u32(v); f.u8(0); }
		Dyn::_55(v) => { f.u8(0x55); f.u32(v); f.u8(0); }
		Dyn::_DD(ref v) => { f.u8(0xDD); f.str(v)?; }
		Dyn::_EE(v) => { f.u8(0xEE); f.f32(v); f.u8(0); }
		Dyn::_FF(v) => { f.u8(0xFF); f.i32(v); f.u8(0); }
	}
	Ok(())
}

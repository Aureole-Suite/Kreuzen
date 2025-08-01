use std::collections::BTreeSet;

use gospel::read::Le as _;
use snafu::ResultExt;

use crate::{ReaderaExt, VReader};

use super::spec::{Opcode, Part};
use super::{Arg, Dyn, Case, Dialogue, Expr, Op, OpMeta, Stmt, SPEC};

mod decompile;

#[derive(Debug, snafu::Snafu)]
pub enum ReadError {
	#[snafu(display("invalid read (at {location})"), context(false))]
	Read {
		source: gospel::read::Error,
		#[snafu(implicit)]
		location: snafu::Location,
	},
	#[snafu(display("failed to read op at {pos}"))]
	Op {
		pos: Label,
		source: OpReadError,
	},
	#[snafu(display("missing labels: {labels:?}"))]
	MissingLabels { labels: BTreeSet<Label> },
	#[snafu(display("failed to decompile function:\n{code:#?}"))]
	Decompile { source: decompile::DecompileError, code: Vec<FlatOp> },
}

pub(crate) fn read(f: &mut VReader) -> Result<Vec<Stmt>, ReadError> {
	let mut ops = Vec::new();
	while !at_end(f) {
		let pos = Label(f.pos() as u32);
		let op = read_op(f).context(OpSnafu { pos })?;
		ops.push((pos, op))
	}
	
	let mut labels = BTreeSet::new();
	for (_, op) in &ops {
		match op {
			FlatOp::Op(_) => {}
			FlatOp::Label(_) => unreachable!(),
			FlatOp::Goto(_, l) | FlatOp::If(_, _, l) => {
				labels.insert(*l);
			}
			FlatOp::Switch(_, _, ls, l) => {
				for (_, l) in ls {
					labels.insert(*l);
				}
				labels.insert(*l);
			}
		}
	}
	let mut ops2 = Vec::with_capacity(ops.len() + labels.len());
	for (pos, op) in ops {
		if labels.remove(&pos) {
			ops2.push(FlatOp::Label(pos));
		}
		ops2.push(op);
	}
	let endl = Label(f.pos() as u32);
	if labels.remove(&endl) {
		ops2.push(FlatOp::Label(endl));
	}
	snafu::ensure!(labels.is_empty(), MissingLabelsSnafu { labels });
	let decomp = decompile::decompile(&ops2).context(DecompileSnafu { code: ops2 })?;
	Ok(decomp)
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Label(u32);

impl std::fmt::Debug for Label {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "@{:X}", self.0)
	}
}

impl std::fmt::Display for Label {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "@{:X}", self.0)
	}
}

#[derive(Clone, PartialEq)]
pub enum FlatOp {
	Op(Op),
	Label(Label),
	Goto(OpMeta, Label),
	If(OpMeta, Expr, Label),
	Switch(OpMeta, Expr, Vec<(i32, Label)>, Label),
}

impl std::fmt::Debug for FlatOp {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Op(arg0) => arg0.fmt(f),
			Self::Label(arg0) => f.debug_tuple("Label").field(arg0).finish(),
			Self::Goto(arg0, arg1) => arg0.fmt(f)?.debug_tuple("Goto").field(arg1).finish(),
			Self::If(arg0, arg1, arg2) => arg0.fmt(f)?.debug_tuple("If").field(arg1).field(arg2).finish(),
			Self::Switch(arg0, arg1, arg2, arg3) => arg0.fmt(f)?.debug_tuple("Switch").field(arg1).field(arg2).field(arg3).finish(),
		}
	}
}

#[derive(Debug, snafu::Snafu)]
pub enum OpReadError {
	#[snafu(display("invalid read (at {location})"), context(false))]
	Read {
		source: gospel::read::Error,
		#[snafu(implicit)]
		location: snafu::Location,
	},
	#[snafu(display("unknown op {} ({} bytes)", opcode, meta.width))]
	UnknownOp { opcode: Opcode, meta: OpMeta },
	#[snafu(display("failed to read expr"), context(false))]
	Expr { source: super::expr::ReadError },
	#[snafu(display("failed to read dialogue"), context(false))]
	Dialogue { source: super::dial::ReadError },
	#[snafu(display("control flow nested inside expr"))]
	BadNesting,
	#[snafu(display("unknown dyn code {code:02X}"))]
	BadDyn { code: u8 },
}

fn read_op(f: &mut VReader) -> Result<FlatOp, OpReadError> {
	let pos = f.pos();
	let op = read_op2(f)?;
	if let FlatOp::Op(op) = &op
		&& op.meta.width != 0xFF
		&& pos + op.meta.width as usize != f.pos()
		&& !op.args.iter().any(|a| matches!(a, Arg::Expr(_)))
	{
		tracing::warn!("expected length {}, got {} on {op:?}", op.meta.width, f.pos() - pos);
	}
	Ok(op)
}

fn read_op2(f: &mut VReader) -> Result<FlatOp, OpReadError> {
	let mut code = f.u8()?;
	let mut opcode = Opcode::new(&[code]);

	let name = SPEC.names.get(&opcode).map(|s| s.as_str());

	let meta = if !matches!(code, 0x01 | 0x04) {
		let line = f.u16()?;
		f.check_u8(0)?;
		let width = f.u8()?;
		OpMeta { line, width }
	} else {
		OpMeta::default()
	};

	match name {
		Some("if") => {
			let expr = Expr::read(f)?;
			let label = Label(f.u32()?);
			return Ok(FlatOp::If(meta, expr, label));
		}
		Some("goto") => {
			let label = Label(f.u32()?);
			return Ok(FlatOp::Goto(meta, label));
		}
		Some("switch") => {
			let expr = Expr::read(f)?;
			let mut cases = Vec::new();
			for _ in 0..f.u8()? {
				cases.push((f.i32()?, Label(f.u32()?)));
			}
			let default = Label(f.u32()?);
			return Ok(FlatOp::Switch(meta, expr, cases, default));
		}
		_ => {}
	}

	let Some(mut spec) = SPEC.ops[code as usize].as_ref() else {
		return UnknownOpSnafu { opcode, meta }.fail();
	};

	let mut args = Vec::new();

	loop {
		read_parts(&mut args, f, &spec.parts)?; // TODO better error
		if spec.has_children() {
			code = f.u8()?;
			opcode.push(code);
			if let Some(next) = spec.child(code) {
				spec = next;
			} else {
				return UnknownOpSnafu { opcode, meta }.fail();
			}
		} else {
			break;
		}
	}

	let mut op = Op {
		name: SPEC.names.get(&opcode).unwrap(),
		meta,
		args
	};
	munge(&mut op);

	Ok(FlatOp::Op(op))
}

pub(crate) fn read_raw_op(f: &mut VReader) -> Result<Op, OpReadError> {
	match read_op2(f)? {
		FlatOp::Op(op) => Ok(op),
		_ => BadNestingSnafu.fail(),
	}
}

fn read_parts(args: &mut Vec<Arg>, f: &mut VReader, part: &[Part]) -> Result<(), OpReadError> {
	for p in part {
		read_part(args, f, p)?;
	}
	Ok(())
}

fn munge(op: &mut Op) {
	let mut changed = false;
	for arg in &mut op.args {
		if let Arg::F32(v) = arg
			&& *v != 0.0 && !(1e-5..=1e7).contains(&v.abs())
		{
			changed = true;
			*arg = Arg::F32Munged(v.to_bits() as i32);
		}
	}
	if changed {
		tracing::warn!("munged f32 values in {op:?}");
	}
}

fn read_part(args: &mut Vec<Arg>, f: &mut VReader, part: &Part) -> Result<(), OpReadError> {
	macro_rules! next {
		($n:expr, $ty:ident) => {
			match args.get($n) {
				Some(Arg::$ty(arg)) => *arg,
				_ => panic!("expected arg {n} to be type {ty}", n=$n, ty=stringify!($ty)),
			}
		};
	}
	use super::spec::Part::*;
	match part {
		U8 => args.push(f.u8()?.into()),
		U16 => args.push(f.u16()?.into()),
		U32 => args.push(f.u32()?.into()),
		I8 => args.push(f.i8()?.into()),
		I16 => args.push(f.i16()?.into()),
		I32 => args.push(f.i32()?.into()),
		F32 => args.push(f.f32()?.into()),
		Str => args.push(f.str()?.into()),

		Char => args.push(Arg::Char(f.u16()?.into())),
		Item => args.push(Arg::Item(f.u16()?.into())),
		Magic => args.push(Arg::Magic(f.u16()?.into())),
		Flag => args.push(Arg::Flag(f.u16()?.into())),
		Global => args.push(Arg::Global(f.u8()?.into())),
		Var => args.push(Arg::Var(f.u8()?.into())),
		FuncArg => args.push(Arg::FuncArg(f.u8()?.into())),
		NumReg => args.push(Arg::NumReg(f.u8()?.into())),
		StrReg => args.push(Arg::StrReg(f.u8()?.into())),
		Attr => args.push(Arg::Attr(f.u8()?.into())),
		CharAttr => args.push(Arg::CharAttr((f.u16()?.into(), f.u8()?).into())),

		Flags8 => args.push(Arg::Flags8(f.u8()?.into())),
		Flags16 => args.push(Arg::Flags16(f.u16()?.into())),
		Flags32 => args.push(Arg::Flags32(f.u32()?.into())),

		Expr => args.push(self::Expr::read(f)?.into()),
		Text => args.push(self::Dialogue::read(f)?.into()),
		Dyn => args.push(read_dyn(f)?.into()),
		Ndyn => {
			for _ in 0..f.u8()? {
				args.push(read_dyn(f)?.into());
			}
		}

		Part::F32Opt => {
			if f.check_i32(-1).is_ok() {
				args.push((-1i32).into());
			} else {
				read_parts(args, f, &[F32])?;
			}
		}

		Part::_40 => read_parts(args, f, super::spec::op_40(next!(1, Char)))?,
		Part::_98 => read_parts(args, f, super::spec::op_98(next!(0, U16)))?,
		Part::_C0 => read_parts(args, f, super::spec::op_c0(next!(0, U16)))?,
		Part::_D2 => read_parts(args, f, super::spec::op_d2(next!(0, I16)))?,

		Part::_3E => {
			let a = next!(1, Char).0;
			if a == 0xFE12 {
				read_parts(args, f, &[U8])?;
			} else if a == 0xFE13 {
				read_parts(args, f, &[F32])?;
			}
			if args[1..] == [Arg::Char(0xFFFF.into()), Arg::F32(5.0), Arg::U8(0)] {
				f.check(&[0, 0, 0])?;
			}
		}
		Part::_3F => {
			if f.version != 2 {
				read_parts(args, f, &[U32])?;
			}
		}
		Part::_4E => {
			if f.version != 2 {
				read_parts(args, f, &[U8])?;
			}
		}
		Part::_6C => {
			if f.version != 2 {
				read_parts(args, f, &[I32])?;
			}
		}
		Part::_79 => {
			let a = next!(0, U8);
			if a == 7 {
				read_parts(args, f, &[U8, U8])?;
			}
		}
		Part::_AB00 => {
			let slice = f.slice(50)?;
			let nonzero = slice.iter().rposition(|&b| b != 0).map_or(0, |i| i + 1);
			for &i in &slice[..nonzero] {
				args.push(i.into());
			}
		}
		Part::_AB02 => {
			let n = f.u8()?;
			for _ in 0..n {
				args.push(f.u16()?.into());
			}
			for _ in n..50 {
				f.check_u16(0)?;
			}
		}
	}
	Ok(())
}

fn at_end(f: &VReader<'_>) -> bool {
	f.remaining().len() <= 3 && f.remaining().iter().all(|&b| b == 0)
}

fn read_dyn(f: &mut VReader) -> Result<Dyn, OpReadError> {
	Ok(match f.u8()? {
		0x11 => { let v = f.u8()?; f.check_u32(0)?; Dyn::Var(v.into()) }
		0x33 => { let v = f.u8()?; f.check_u32(0)?; Dyn::NumReg(v.into()) }
		0x44 => { let v = f.u8()?; f.check_u32(0)?; Dyn::StrReg(v.into()) }
		0x55 => { let v = f.u8()?; f.check_u32(0)?; Dyn::Global(v.into()) }
		0xDD => { let v = f.str()?; Dyn::Str(v) }
		0xEE => { let v = f.f32()?; f.check_u8(0)?; Dyn::F32(v) }
		0xFF => {
			let v = f.i32()?;
			f.check_u8(0)?;
			if v.abs() > 0x1000000 {
				Dyn::I32lol(f32::from_bits(v as u32))
			} else {
				Dyn::I32(v)
			}
		}
		code => return BadDynSnafu { code }.fail(),
	})
}

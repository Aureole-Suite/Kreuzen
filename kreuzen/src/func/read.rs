use std::collections::BTreeSet;

use arrayvec::ArrayVec;
use gospel::read::Le as _;
use snafu::ResultExt;

use crate::{ReaderaExt, VReader};

use super::spec::Part;
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
	#[snafu(display("unknown op {} ({} bytes)", hex::encode_upper(&op.code), op.meta.width))]
	UnknownOp { op: Op },
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
	let mut op = Op {
		code: ArrayVec::new(),
		meta: OpMeta::default(),
		args: Vec::new(),
	};
	op.code.push(code);

	let name = SPEC.names.get(op.code.as_slice()).map(|s| s.as_str());

	if !matches!(code, 0x01 | 0x04) {
		op.meta.line = f.u16()?;
		f.check_u8(0)?;
		op.meta.width = f.u8()?;
	}

	match name {
		Some("if") => {
			let expr = Expr::read(f)?;
			let label = Label(f.u32()?);
			return Ok(FlatOp::If(op.meta, expr, label));
		}
		Some("goto") => {
			let label = Label(f.u32()?);
			return Ok(FlatOp::Goto(op.meta, label));
		}
		Some("switch") => {
			let expr = Expr::read(f)?;
			let mut cases = Vec::new();
			for _ in 0..f.u8()? {
				cases.push((f.i32()?, Label(f.u32()?)));
			}
			let default = Label(f.u32()?);
			return Ok(FlatOp::Switch(op.meta, expr, cases, default));
		}
		_ => {}
	}

	let Some(mut spec) = SPEC.ops[code as usize].as_ref() else {
		return UnknownOpSnafu { op }.fail();
	};

	loop {
		read_parts(&mut op, f, &spec.parts)?;
		if spec.has_children() {
			code = f.u8()?;
			op.code.push(code);
			if let Some(next) = spec.child(code) {
				spec = next;
			} else {
				return UnknownOpSnafu { op }.fail();
			}
		} else {
			break;
		}
	}

	Ok(FlatOp::Op(op))
}

pub(crate) fn read_raw_op(f: &mut VReader) -> Result<Op, OpReadError> {
	match read_op2(f)? {
		FlatOp::Op(op) => Ok(op),
		_ => BadNestingSnafu.fail(),
	}
}

fn read_parts(op: &mut Op, f: &mut VReader, part: &[Part]) -> Result<(), OpReadError> {
	for p in part {
		read_part(op, f, p)?;
	}
	Ok(())
}

macro_rules! next {
	($op:expr, $n:expr, $ty:ident) => {
		match $op.args.get($n) {
			Some(Arg::$ty(arg)) => *arg,
			_ => panic!("{op:?}: expected arg {n} to be type {ty}", op=$op, n=$n, ty=stringify!($ty)),
		}
	};
}

fn read_part(op: &mut Op, f: &mut VReader, part: &Part) -> Result<(), OpReadError> {
	use super::spec::Part::*;
	match part {
		U8 => op.push(f.u8()?),
		U16 => op.push(f.u16()?),
		U32 => op.push(f.u32()?),
		I8 => op.push(f.i8()?),
		I16 => op.push(f.i16()?),
		I32 => op.push(f.i32()?),
		F32 => op.push(f.f32()?),
		Str => op.push(f.str()?),

		Char => op.push(Arg::Char(f.u16()?.into())),
		Item => op.push(Arg::Item(f.u16()?.into())),
		Magic => op.push(Arg::Magic(f.u16()?.into())),
		Flag => op.push(Arg::Flag(f.u16()?.into())),
		Global => op.push(Arg::Global(f.u8()?.into())),
		Var => op.push(Arg::Var(f.u8()?.into())),
		NumReg => op.push(Arg::NumReg(f.u8()?.into())),
		StrReg => op.push(Arg::StrReg(f.u8()?.into())),
		Attr => op.push(Arg::Attr(f.u8()?.into())),
		CharAttr => op.push(Arg::CharAttr((f.u16()?.into(), f.u8()?).into())),
		Flags16 => op.push(Arg::Flags16(f.u16()?.into())),
		Flags32 => op.push(Arg::Flags32(f.u32()?.into())),

		Expr => op.push(self::Expr::read(f)?),
		Text => op.push(self::Dialogue::read(f)?),
		Dyn => op.push(read_dyn(f)?),
		Ndyn => {
			for _ in 0..f.u8()? {
				op.push(read_dyn(f)?);
			}
		}

		Part::_40 => read_parts(op, f, super::spec::op_40(next!(op, 1, Char)))?,
		Part::_98 => read_parts(op, f, super::spec::op_98(next!(op, 0, U16)))?,
		Part::_C0 => read_parts(op, f, super::spec::op_c0(next!(op, 0, U16)))?,
		Part::_D2 => read_parts(op, f, super::spec::op_d2(next!(op, 0, I16)))?,

		Part::_3E => {
			let a = next!(op, 1, Char).0;
			if a == 0xFE12 {
				read_parts(op, f, &[U8])?;
			} else if a == 0xFE13 {
				read_parts(op, f, &[F32])?;
			}
			if op.args[1..] == [Arg::Char(0xFFFF.into()), Arg::F32(5.0), Arg::U8(0)] {
				f.check(&[0, 0, 0])?;
			}
		}
		Part::_3F => {
			if f.version != 2 {
				read_parts(op, f, &[U32])?;
			}
		}
		Part::_4E => {
			if f.version != 2 {
				read_parts(op, f, &[U8])?;
			}
		}
		Part::_6C => {
			if f.version != 2 {
				read_parts(op, f, &[I32])?;
			}
		}
		Part::_79 => {
			let a = next!(op, 0, U8);
			if a == 7 {
				read_parts(op, f, &[U8, U8])?;
			}
		}
		Part::_AB00 => {
			let slice = f.slice(50)?;
			let nonzero = slice.iter().rposition(|&b| b != 0).map_or(0, |i| i + 1);
			for &i in &slice[..nonzero] {
				op.push(i);
			}
		}
		Part::_AB02 => {
			let n = f.u8()?;
			for _ in 0..n {
				op.push(f.u16()?);
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

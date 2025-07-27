use std::collections::BTreeSet;

use arrayvec::ArrayVec;
use gospel::read::Le as _;
use snafu::ResultExt;

use crate::{ReaderaExt, VReader};

use super::spec::Part;
use super::{Arg, CallArg, Case, Dialogue, Expr, Op, OpMeta, Stmt, SPEC};

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

pub fn read(f: &mut VReader) -> Result<Vec<Stmt>, ReadError> {
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
pub struct Label(pub u32);

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

impl FlatOp {
	pub fn meta(&self) -> OpMeta {
		match self {
			FlatOp::Op(op) => op.meta,
			FlatOp::Label(_) => OpMeta::default(),
			FlatOp::Goto(meta, _) => *meta,
			FlatOp::If(meta, _, _) => *meta,
			FlatOp::Switch(meta, _, _, _) => *meta,
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

	match name {
		Some("return") => {
			return Ok(FlatOp::Op(op));
		}
		Some("fastcall") => {
			op.push(f.u8()?);
			op.push(f.str()?);
			return Ok(FlatOp::Op(op));
		}
		_ => {}
	}

	op.meta.line = f.u16()?;
	f.check_u8(0)?;
	op.meta.width = f.u8()?;

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
		Char => op.push(crate::Char(f.u16()?)),
		Expr => op.push(self::Expr::read(f)?),
		Text => op.push(self::Dialogue::read(f)?),
		Dyn => op.push(call_arg(f)?),
		Dyn2 => op.push(call_arg2(f)?),
		Ndyn => {
			for _ in 0..f.u8()? {
				op.push(call_arg(f)?);
			}
		}

		_3E => {
			let Some(&Arg::Char(crate::Char(a))) = op.args.get(1) else {
				panic!("3E must have a char arg");
			};
			if a == 0xFE12 {
				read_parts(op, f, &[U8])?;
			} else if a == 0xFE13 {
				read_parts(op, f, &[F32])?;
			}

			if op.args[1..] == [Arg::Char(crate::Char(65535)), Arg::F32(5.0), Arg::U8(0)] {
				f.check(&[0, 0, 0])?;
			}
		}

		_3F => {
			if f.version != 2 {
				read_parts(op, f, &[U32])?;
			}
		}

		_40 => {
			let Some(&Arg::Char(crate::Char(a))) = op.args.get(1) else {
				panic!("40 must have a char arg");
			};
			if a == 0xFE15 {
				read_parts(op, f, &[Dyn, Dyn, Dyn, Dyn])?;
			} else {
				read_parts(op, f, &[F32, F32, F32, F32])?;
				if matches!(a, 0xFE02 | 0xFE03 | 0xFE04) {
					read_parts(op, f, &[F32])?;
				}
			}
			read_parts(op, f, &[U8, U16, F32, F32, U8])?;
			if a == 0xFE05 {
				read_parts(op, f, &[Str])?;
			}
		}

		_4E => {
			if f.version != 2 {
				read_parts(op, f, &[U8])?;
			}
		}

		_6C => {
			if f.version != 2 {
				read_parts(op, f, &[I32])?;
			}
		}

		_79 => {
			let Some(&Arg::U8(a)) = op.args.get(0) else {
				panic!("79 must have a U8 arg");
			};
			if a == 7 {
				read_parts(op, f, &[U8, U8])?;
			}
		}

		_98 => {
			let Some(&Arg::U16(a)) = op.args.get(0) else {
				panic!("98 must have a U16 arg");
			};
			match a {
				1 => read_parts(op, f, &[F32])?,
				2 => read_parts(op, f, &[F32])?,
				6 => read_parts(op, f, &[F32])?,
				7 => read_parts(op, f, &[F32])?,
				3 => read_parts(op, f, &[U16, U8])?,
				1000 => read_parts(op, f, &[F32, U8])?,
				1001 => read_parts(op, f, &[F32, U8])?,
				2000 => read_parts(op, f, &[U8, F32, U8])?,
				3000 => read_parts(op, f, &[F32, F32, U16, F32])?,
				4000 => read_parts(op, f, &[Char, F32, U16, U8])?,
				4001 => read_parts(op, f, &[Str, F32, U16, U8])?,
				4002 => read_parts(op, f, &[U16])?,
				5000 => read_parts(op, f, &[F32])?,
				5001 => read_parts(op, f, &[F32])?,
				5002 => read_parts(op, f, &[F32])?,
				6000 => read_parts(op, f, &[F32])?,
				6001 => read_parts(op, f, &[F32])?,
				6500 => read_parts(op, f, &[F32])?,
				7000 => read_parts(op, f, &[U8])?,
				7001 => read_parts(op, f, &[U8])?,
				8000 => read_parts(op, f, &[Str, U8])?,
				9000 => read_parts(op, f, &[F32])?,
				10000 => read_parts(op, f, &[F32, F32, F32, F32, F32, F32, F32, F32])?,
				_ => {}
			}
		}

		_AB01 => {
			let slice = f.slice(50)?;
			let nonzero = slice.iter().rposition(|&b| b != 0).map_or(0, |i| i + 1);
			for &i in &slice[..nonzero] {
				op.push(i);
			}
		}

		_AB02 => {
			let n = f.u8()?;
			for i in 0..50 {
				if i < n {
					op.push(f.u16()?);
				} else {
					f.check_u16(0)?;
				}
			}
		}

		_C0 => {
			let Some(&Arg::U16(a)) = op.args.get(0) else {
				panic!("C0 must have a U16 arg");
			};
			match a {
				1 => read_parts(op, f, &[F32])?,
				3 => read_parts(op, f, &[Str, F32, F32, F32, F32, F32, F32])?,
				4 => read_parts(op, f, &[Str, U8])?,
				1000 | 1001 | 1003 => read_parts(op, f, &[U16, U16])?,
				_ => {}
			}
		}

		_D2 => {
			let Some(&Arg::I16(a)) = op.args.get(0) else {
				panic!("C0 must have a I16 arg");
			};
			match a {
				0 => read_parts(op, f, &[U8])?,
				3 => read_parts(op, f, &[U8, U8, U32])?,
				-2 | -1 => read_parts(op, f, &[Dyn])?,
				_ => {}
			}
		}
	}
	Ok(())
}

fn at_end(f: &VReader<'_>) -> bool {
	f.remaining().len() <= 3 && f.remaining().iter().all(|&b| b == 0)
}

fn call_arg(f: &mut VReader) -> Result<CallArg, OpReadError> {
	Ok(match f.u8()? {
		0x11 => CallArg::_11(f.u32()?, f.u8()?),
		0x22 => CallArg::_22(f.f32()?, f.f32()?),
		0x33 => CallArg::_33(f.f32()?, f.u8()?),
		0x44 => CallArg::_44(f.f32()?, f.u8()?, String::new()),
		0x55 => CallArg::_55(f.u32()?, f.u8()?),
		0xDD => CallArg::_DD(String::new(), f.str()?),
		0xEE => CallArg::_EE(f.f32()?, f.u8()?),
		0xFF => CallArg::_FF(f.i32()?, f.u8()?),
		v => CallArg::Unknown(v),
	})
}

fn call_arg2(f: &mut VReader) -> Result<CallArg, OpReadError> {
	Ok(match f.u8()? {
		0x11 => CallArg::_11(f.u32()?, f.u8()?),
		0x22 => CallArg::_22(f.f32()?, f.f32()?),
		0x33 => CallArg::_33(f.f32()?, f.u8()?),
		0x44 => CallArg::_44(f.f32()?, f.u8()?, f.str()?),
		0x55 => CallArg::_55(f.u32()?, f.u8()?),
		0xDD => CallArg::_DD(f.str()?, f.str()?),
		0xEE => CallArg::_EE(f.f32()?, f.u8()?),
		0xFF => CallArg::_FF(f.i32()?, f.u8()?),
		v => CallArg::Unknown(v),
	})
}

use std::sync::LazyLock;
use gospel::read::Le as _;
use arrayvec::ArrayVec;
use snafu::ResultExt as _;

use crate::{ReaderaExt as _, VReader};

#[derive(Debug, snafu::Snafu)]
pub enum FunctionError {
	#[snafu(display("invalid read (at {location})"), context(false))]
	Read {
		source: gospel::read::Error,
		#[snafu(implicit)]
		location: snafu::Location,
	},
	#[snafu(display("failed to read op at {pos:X}"))]
	Op {
		pos: usize,
		source: OpError,
	}
}

pub mod expr;
pub mod dial;

mod spec;
use spec::Spec;

pub static SPEC: LazyLock<Spec> = LazyLock::new(|| Spec::parse(include_str!("../../ed85.txt")));

pub fn read_func(f: &mut VReader) -> Result<(), FunctionError> {
	let mut ops = Vec::new();
	while !at_end(f) {
		let pos = f.pos();
		let op = read_op(f).context(OpSnafu { pos })?;
		ops.push((pos, op))
	}
	Ok(())
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Label(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct OpMeta {
	pub line: u16,
	pub width: u8,
}

impl Default for OpMeta {
	fn default() -> Self {
		OpMeta { line: 0, width: 0xFF }
	}
}

#[derive(Debug, Clone, PartialEq)]
pub enum FlatOp {
	Op(Op),
	Goto(OpMeta, Label),
	If(OpMeta, expr::Expr, Label),
	Switch(OpMeta, expr::Expr, Vec<(i32, Label)>, Label),
}

impl FlatOp {
	pub fn meta(&self) -> OpMeta {
		match self {
			FlatOp::Op(op) => op.meta,
			FlatOp::Goto(meta, _) => *meta,
			FlatOp::If(meta, _, _) => *meta,
			FlatOp::Switch(meta, _, _, _) => *meta,
		}
	}
}

#[derive(Clone, PartialEq)]
pub struct Op {
	pub code: ArrayVec<u8, 4>,
	pub meta: OpMeta,
	pub args: Vec<Arg>
}

#[derive(Clone, PartialEq, derive_more::From)]
pub enum Arg {
	Str(String),
	U8(u8),
	U16(u16),
	U32(u32),
	I8(i8),
	I16(i16),
	I32(i32),
	F32(f32),
	Expr(expr::Expr),
	CallArg(CallArg),
	Dialogue(dial::Dialogue),
}

impl Op {
	pub fn push(&mut self, arg: impl Into<Arg>) {
		self.args.push(arg.into());
	}
}

pub struct OpName<'a> {
	code: &'a [u8],
}

impl std::fmt::Display for OpName<'_> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		let mut prefix = 0;
		for i in 0..=self.code.len() {
			if let Some(s) = SPEC.names.get(&self.code[..i]) {
				f.write_str(s)?;
				prefix = i;
			}
		}
		if prefix == 0 {
			write!(f, "op")?;
		}
		for byte in &self.code[prefix..] {
			write!(f, "{byte:02X}")?;
		}
		Ok(())
	}
}

impl std::fmt::Debug for Op {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", OpName { code: &self.code })?;
		write!(f, "(")?;
		for (i, arg) in self.args.iter().enumerate() {
			if i > 0 {
				write!(f, ", ")?;
			}
			arg.fmt(f)?;
		}
		write!(f, ")")
	}
}

impl std::fmt::Debug for Arg {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Arg::Str(s) => write!(f, "{s:?}"),
			Arg::U8(v) => write!(f, "{v}"),
			Arg::U16(v) => write!(f, "{v}"),
			Arg::U32(v) => write!(f, "{v}"),
			Arg::I8(v) => write!(f, "{v}"),
			Arg::I16(v) => write!(f, "{v}"),
			Arg::I32(v) => write!(f, "{v}"),
			Arg::F32(v) => write!(f, "{v:?}"),
			Arg::Expr(e) => write!(f, "{e:?}"),
			Arg::CallArg(ca) => write!(f, "{ca:?}"),
			Arg::Dialogue(d) => write!(f, "{d:?}"),
		}
	}
}

#[derive(Debug, snafu::Snafu)]
pub enum OpError {
	#[snafu(display("invalid read (at {location})"), context(false))]
	Read {
		source: gospel::read::Error,
		#[snafu(implicit)]
		location: snafu::Location,
	},
	#[snafu(display("unknown op {} ({} bytes)", hex::encode_upper(&op.code), op.meta.width))]
	UnknownOp { op: Op },
	#[snafu(display("failed to read expr"), context(false))]
	Expr { source: expr::ExprError },
	#[snafu(display("failed to read dialogue"), context(false))]
	Dialogue { source: dial::DialogueError },
	#[snafu(display("control flow nested inside expr"))]
	BadNesting,
}

fn read_op(f: &mut VReader) -> Result<FlatOp, OpError> {
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

fn read_op2(f: &mut VReader) -> Result<FlatOp, OpError> {
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
			let expr = expr::Expr::read(f)?;
			let label = Label(f.u32()?);
			return Ok(FlatOp::If(op.meta, expr, label));
		}
		Some("goto") => {
			let label = Label(f.u32()?);
			return Ok(FlatOp::Goto(op.meta, label));
		}
		Some("switch") => {
			let expr = expr::Expr::read(f)?;
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

fn read_raw_op(f: &mut VReader) -> Result<Op, OpError> {
	match read_op2(f)? {
		FlatOp::Op(op) => Ok(op),
		_ => BadNestingSnafu.fail(),
	}
}

fn read_parts(op: &mut Op, f: &mut VReader, part: &[spec::Part]) -> Result<(), OpError> {
	for p in part {
		read_part(op, f, p)?;
	}
	Ok(())
}

fn read_part(op: &mut Op, f: &mut VReader, part: &spec::Part) -> Result<(), OpError> {
	use spec::Part::*;
	match part {
		U8 => op.push(f.u8()?),
		U16 => op.push(f.u16()?),
		U32 => op.push(f.u32()?),
		I8 => op.push(f.i8()?),
		I16 => op.push(f.i16()?),
		I32 => op.push(f.i32()?),
		F32 => op.push(f.f32()?),
		Str => op.push(f.str()?),
		Expr => op.push(expr::Expr::read(f)?),
		Text => op.push(dial::Dialogue::read(f)?),
		Dyn => op.push(call_arg(f)?),
		Dyn2 => op.push(call_arg2(f)?),
		Ndyn => {
			for _ in 0..f.u8()? {
				op.push(call_arg(f)?);
			}
		}

		_3E => {
			let Some(&Arg::U16(a)) = op.args.get(1) else {
				panic!("3E must have a U16 arg");
			};
			if a == 0xFE12 {
				read_parts(op, f, &[U8])?;
			} else if a == 0xFE13 {
				read_parts(op, f, &[F32])?;
			}

			if op.args[1..] == [Arg::U16(65535), Arg::F32(5.0), Arg::U8(0)] {
				f.check(&[0, 0, 0])?;
			}
		}

		_3F => {
			if f.version != 2 {
				read_parts(op, f, &[U32])?;
			}
		}

		_40 => {
			let Some(&Arg::U16(a)) = op.args.get(1) else {
				panic!("40 must have a U16 arg");
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
				4000 => read_parts(op, f, &[U16, F32, U16, U8])?,
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
			let Some(&Arg::U16(a)) = op.args.get(0) else {
				panic!("C0 must have a U16 arg");
			};
			match a {
				0 => read_parts(op, f, &[U8])?,
				3 => read_parts(op, f, &[U8, U8, U32])?,
				0xFFFE | 0xFFFF => read_parts(op, f, &[Dyn])?,
				_ => {}
			}
		}
	}
	Ok(())
}

fn at_end(f: &VReader<'_>) -> bool {
	f.remaining().len() <= 3 && f.remaining().iter().all(|&b| b == 0)
}

#[derive(Debug, Clone, PartialEq)]
pub enum CallArg {
	_11(u32, u8),
	_22(f32, f32),
	_33(f32, u8),
	_44(f32, u8, String),
	_55(u32, u8),
	_DD(String, String),
	_EE(f32, u8),
	_FF(i32, u8),
	Unknown(u8),
}

fn call_arg(f: &mut VReader) -> Result<CallArg, OpError> {
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

fn call_arg2(f: &mut VReader) -> Result<CallArg, OpError> {
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

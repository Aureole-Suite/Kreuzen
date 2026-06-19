use std::collections::{BTreeSet, HashMap};

use gospel::read::Le as _;
use gospel::write::{Le as _, Writer, Label as WLabel};
use rootcause::option_ext::OptionExt as _;
use rootcause::prelude::ResultExt as _;

use crate::io::{CReader, OData, WriterExt as _};
use crate::spec::{Opcode, Part};
use crate::expr::Expr;
use crate::text::Text;
use crate::{Game, types::*};

mod to_dot;
pub use to_dot::to_dot;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Label(u32);

impl std::fmt::Debug for Label {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "@{:04X}", self.0)
	}
}

impl std::fmt::Display for Label {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "@{:04X}", self.0)
	}
}

#[derive(Debug, Clone)]
pub struct OpContext(pub Vec<(Label, FlatOp)>);
impl std::fmt::Display for OpContext {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		const MAX: usize = 5;
		if let Some(more) = self.0.len().checked_sub(MAX) {
			writeln!(f, "Context: (omitting {more} ops)")?;
			for (Label(l), op) in self.0.iter().rev().take(MAX).rev() {
				writeln!(f, "  {l:04X} {op:?}")?;
			}
		} else if self.0.is_empty() {
			writeln!(f, "Context: (empty)")?;
		} else {
			writeln!(f, "Context:")?;
		}
		Ok(())
	}
}

#[derive(Clone)]
struct Hexdump(String);
impl std::fmt::Display for Hexdump {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		self.0.fmt(f)
	}
}
impl std::fmt::Debug for Hexdump {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "Hexdump(..)")
	}
}

#[derive(Debug, Clone)]
pub struct Code {
	pub ops: Vec<FlatOp>,
}

pub fn read(f: &mut CReader, end: usize) -> rootcause::Result<Code> {
	let mut ops = Vec::new();
	while f.pos() < end {
		let pos = f.pos();
		let op = read_op(f)
			.context_with(|| format!("Failed to read op at {pos:04X}"))
			.attach_with(|| OpContext(std::mem::take(&mut ops)))
			.attach_with(|| Hexdump(format!("{:#2.48X}", f.dump().start(pos).mark(f.pos()))))
			?;
		ops.push((Label(pos as u32), op))
	}

	let wtf = (f.game, f.scena) == (Game::Cs3, "system");
	let mut ops = insert_labels(ops, wtf)?;
	remap_labels(&mut ops);

	Ok(Code { ops })
}

fn insert_labels(ops: Vec<(Label, FlatOp)>, wtf: bool) -> rootcause::Result<Vec<FlatOp>> {
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

	const WEIRD_LABEL: Label = Label(10651);
	if wtf
	&& labels.len() == 1
	&& let Some(if_loc) = ops2.iter().position(|op| matches!(op, FlatOp::If(_, _, WEIRD_LABEL)))
	&& labels.remove(&WEIRD_LABEL)
	{
		tracing::warn!("Fixing up broken label");
		ops2.insert(if_loc + 2, FlatOp::Label(WEIRD_LABEL));
	}

	crate::ensure!(labels.is_empty(), "Some labels were not used: {labels:?}");
	Ok(ops2)
}

fn remap_labels(ops2: &mut [FlatOp]) {
	let mut order = HashMap::new();
	for op in ops2.iter_mut() {
		if let FlatOp::Label(l) = op {
			order.insert(*l, Label(order.len() as _));
		}
	}
	let remap = |l: &mut Label| *l = order[l];
	for op in ops2.iter_mut() {
		match op {
			FlatOp::Op(_) => {}
			FlatOp::Label(l) => remap(l),
			FlatOp::Goto(_, l) | FlatOp::If(_, _, l) => remap(l),
			FlatOp::Switch(_, _, ls, l) => {
				for (_, l) in ls {
					remap(l);
				}
				remap(l);
			}
		}
	}
}

#[derive(Clone, Copy, PartialEq, Eq, Default)]
pub struct OpMeta {
	pub line: u16,
	pub has_width: bool, // width != 0xFF
}

impl OpMeta {
	pub(crate) fn fmt<'a, 'b>(&self, f: &'a mut std::fmt::Formatter<'b>) -> Result<&'a mut std::fmt::Formatter<'b>, std::fmt::Error> {
		write!(f, "{self:?}:")?;
		Ok(f)
	}
}

impl std::fmt::Debug for OpMeta {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", self.line)?;
		if !self.has_width {
			write!(f, "~")?;
		}
		Ok(())
	}
}

#[derive(Clone, PartialEq)]
pub struct Op {
	pub name: &'static str,
	pub meta: OpMeta,
	pub args: Vec<Arg>
}

impl std::fmt::Debug for Op {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		self.meta.fmt(f)?;
		write!(f, "{}(", self.name)?;
		for (i, arg) in self.args.iter().enumerate() {
			if i > 0 {
				write!(f, ", ")?;
			}
			arg.fmt(f)?;
		}
		write!(f, ")")
	}
}

#[derive(Clone, PartialEq, derive_more::From, derive_more::Debug)]
pub enum Arg {
	#[debug("{_0:?}")] Str(String),
	#[from(skip)] #[debug("{_0:?}")] Int(i64), // i64 becase it can contain both i32 and u32
	#[debug("{_0:?}")] F32(f32),

	// There's way too many cases where the data should be f32 but the data is a i32.
	#[from(ignore)] #[debug("{_0:?}'")] F32Munged(i32),
	// And some cases, especially in dyn, where it should be i32 but the data is a f32.
	#[from(ignore)] #[debug("{_0:?}'")] I32Munged(f32),

	#[debug("{_0:?}")] Char(crate::types::Char),
	#[debug("{_0:?}")] Item(crate::types::Item),
	#[debug("{_0:?}")] Magic(crate::types::Magic),
	#[debug("{_0:?}")] Flag(crate::types::Flag),
	#[debug("{_0:?}")] Global(crate::types::Global),
	#[debug("{_0:?}")] Var(crate::types::Var),
	#[debug("{_0:?}")] FuncArg(crate::types::FuncArg),
	#[debug("{_0:?}")] NumReg(crate::types::NumReg),
	#[debug("{_0:?}")] StrReg(crate::types::StrReg),
	#[debug("{_0:?}")] Attr(crate::types::Attr),
	#[debug("{_0:?}")] CharAttr(crate::types::CharAttr),
	#[debug("{_0:?}")] Flags8(crate::types::Flags8),
	#[debug("{_0:?}")] Flags16(crate::types::Flags16),
	#[debug("{_0:?}")] Flags32(crate::types::Flags32),

	#[debug("{_0:?}")] Expr(Expr),
	#[debug("{_0:?}")] Text(Text),
}

macro_rules! from_int {
	($($t:ty),*) => {
		$(
			impl From<$t> for Arg {
				fn from(value: $t) -> Self {
					Self::Int(value as i64)
				}
			}
		)*
	};
}
from_int!(u8, u16, u32, i8, i16, i32);

#[derive(Clone, PartialEq, Debug)]
pub enum FlatOp {
	Op(Op),
	Label(Label),
	Goto(OpMeta, Label),
	If(OpMeta, Expr, Label),
	Switch(OpMeta, Expr, Vec<(i32, Label)>, Label),
}

pub(crate) fn read_op(f: &mut CReader) -> rootcause::Result<FlatOp> {
	let mut code = f.u8()?;
	let mut opcode = Opcode::new(&[code]);

	let spec = crate::spec::for_game(f.game, f.variant);
	let mut op_spec = match spec.ops[code as usize].as_ref() {
		Some(it) => it,
		None => {
			rootcause::bail!("_Unknown opcode {opcode}")
		}
	};

	let mut line = 0;
	let mut width = 0xFF;
	if !matches!(code, 0x01 | 0x04) && f.game == crate::Game::Reverie {
		line = f.u16()?;
		f.check_u8(0)?;
		width = f.u8()?;
	};

	let meta = OpMeta {
		line,
		has_width: width != 0xFF,
	};

	match op_spec.name.as_str() {
		"if" => {
			let expr = Expr::read(f)?;
			let label = Label(f.u32()?);
			return Ok(FlatOp::If(meta, expr, label));
		}
		"goto" => {
			let label = Label(f.u32()?);
			return Ok(FlatOp::Goto(meta, label));
		}
		"switch" => {
			let expr = Expr::read(f)?;
			let mut cases = Vec::new();
			for _ in 0..f.u8()? {
				let value = match f.game {
					Game::Cs1 | Game::Cs2 | Game::Tx => f.i16()? as i32,
					_ => f.i32()?,
				};
				cases.push((value, Label(f.u32()?)));
			}
			let default = Label(f.u32()?);
			return Ok(FlatOp::Switch(meta, expr, cases, default));
		}
		_ => {}
	}

	let mut op = Op {
		name: op_spec.name.as_str(),
		meta,
		args: Vec::new(),
	};

	loop {
		read_parts(&mut op, f, &op_spec.parts)?;
		if op_spec.has_children() {
			code = f.u8()?;
			opcode.push(code);

			op_spec = match op_spec.child(code) {
				Some(it) => it,
				None => rootcause::bail!("_Unknown opcode {opcode}"),
			};
			op.name = op_spec.name.as_str();
		} else {
			break;
		}
	}

	Ok(FlatOp::Op(op))
}

fn read_parts(op: &mut Op, f: &mut CReader, parts: &[Part]) -> rootcause::Result<()> {
	op.args.reserve(parts.len());
	use Part as P;
	for p in parts {
		match p {
			P::U8 => op.args.push(f.u8()?.into()),
			P::U16 => op.args.push(f.u16()?.into()),
			P::U32 => op.args.push(f.u32()?.into()),
			P::I8 => op.args.push(f.i8()?.into()),
			P::I16 => op.args.push(f.i16()?.into()),
			P::I32 => op.args.push(f.i32()?.into()),
			P::F32 => op.args.push(f.f32()?.into()),
			P::Str => op.args.push(f.str()?.into()),

			P::Char => op.args.push(Char(f.u16()?).into()),
			P::Item => op.args.push(Item(f.u16()?).into()),
			P::Magic => op.args.push(Magic(f.u16()?).into()),
			P::Flag => op.args.push(Flag(f.u16()?).into()),
			P::Global => op.args.push(Global(f.u8()?).into()),
			P::Var => op.args.push(Var(f.u8()?).into()),
			P::FuncArg => op.args.push(FuncArg(f.u8()?).into()),
			P::NumReg => op.args.push(NumReg(f.u8()?).into()),
			P::StrReg => op.args.push(StrReg(f.u8()?).into()),
			P::Attr => op.args.push(Attr(f.u8()?).into()),
			P::CharAttr => op.args.push(CharAttr(Char(f.u16()?), f.u8()?).into()),

			P::Flags8 => op.args.push(Arg::Flags8(f.u8()?.into())),
			P::Flags16 => op.args.push(Arg::Flags16(f.u16()?.into())),
			P::Flags32 => op.args.push(Arg::Flags32(f.u32()?.into())),

			P::Expr => op.args.push(self::Expr::read(f)?.into()),
			P::Text => op.args.push(self::Text::read(f)?.into()),
			P::Dyn => op.args.push(read_dyn(f)?),
			P::Ndyn => {
				for _ in 0..f.u8()? {
					op.args.push(read_dyn(f)?);
				}
			}
			P::Dync => op.args.push(match read_dyn(f)? {
				Arg::Int(v) => Arg::Char(Char(v.try_into()?)),
				a => a,
			}),

			P::Cs1_36 => {
				if matches!(op.args[1], Arg::Char(Char(0xFE02..=0xFE03))) {
					read_parts(op, f, &[P::F32])?;
				}
			}
			P::Cs1_3C => {
				if matches!(op.args[1], Arg::Char(Char(0xFFFF))) {
					read_parts(op, f, &[P::U32, P::U32, P::U32])?;
				}
			}

			P::Cs2_37 => {
				if matches!(op.args[1], Arg::Char(Char(0xFE04))) {
					read_parts(op, f, &[P::Str])?;
				}
			}

			P::Tx_3C => {
				if matches!(op.args[0], Arg::Int(1)) {
					read_parts(op, f, &[P::U32, P::U32, P::U32])?;
				}
			}
			P::Tx_isforceload => {
				if f.scena == "a0005" && f.check(b"isforceload").is_ok() {
					// for some reason this one put the name of the flag rather than value
					op.args.push(Arg::Str("isforceload".to_string()));
				} else {
					read_parts(op, f, &[P::U8])?;
				}
			}
			P::Tx_2F => {
				if f.scena == "t5110" && f.check(b"AniWvWait\xFF").is_ok() {
					op.args.push(Arg::Str("AniWvWait".to_string()));
					op.args.push(Arg::Int(0xFF));
					read_parts(op, f, &[P::U8, P::F32, P::F32, P::F32])?;
					break;
				}
			}

			P::Cs3_98 => {
				let Arg::Int(v) = op.args[0] else {
					rootcause::bail!("Expected U16 for Cs3_c0 part");
				};
				read_parts(op, f, op_98(v as u16, f.game))?;
			}
			P::Cs3_c0 => {
				let Arg::Int(v) = op.args[0] else {
					rootcause::bail!("Expected U16 for Cs3_c0 part");
				};
				read_parts(op, f, op_c0(v as u16))?;
			}

			P::Cs4_40 => {
				let Arg::Char(v) = op.args[1] else {
					rootcause::bail!("Expected Char");
				};
				read_parts(op, f, op_40(v))?;
			}
			P::Cs4_wtf_are_you_doing => {
				if f.scena == "mg11" && f.check_u32(0).is_ok() {
					op.args.push(Arg::Int(0)); // This one is only there in the japanese version
				}
			}
			
			P::Rev_3E => {
				match op.args[1] {
					Arg::Char(Char(0xFE12)) => read_parts(op, f, &[P::U8])?,
					Arg::Char(Char(0xFE13)) => read_parts(op, f, &[P::F32])?,
					Arg::Char(Char(0xFFFF)) if f.scena == "btlcom" => {} // why
					Arg::Char(Char(0xFFFF)) => read_parts(op, f, &[P::U8, P::U8, P::U8])?,
					_ => {}
				}
			}
			P::Rev_D2 => {
				let Arg::Int(v) = op.args[0] else {
					rootcause::bail!("Expected I16");
				};
				read_parts(op, f, op_d2(v as i16))?;
			}
			P::Rev_79 => {
				if matches!(op.args[0], Arg::Int(7)) {
					read_parts(op, f, &[P::U8])?;
				}
			}
			P::Rev_E002 => {
				match op.args[1] {
					Arg::Char(Char(0xFFFF)) => read_parts(op, f, &[P::I32])?,
					_ => read_parts(op, f, &[P::F32])?,
				}
			}

			P::Print => println!("{op:?}"),
			P::Fail => rootcause::bail!("Fail"),
		}
	}
	Ok(())
}

#[rustfmt::skip]
fn op_40(a: crate::types::Char) -> &'static [Part] {
	use Part::*;
	match a.0 {
		0xFE02..= 0xFE04 => &[F32, F32, F32, F32, F32, U8, Flags16, F32, F32, U8],
		0xFE05           => &[F32, F32, F32, F32,      U8, Flags16, F32, F32, U8, Str],
		0xFE15           => &[Dyn, Dyn, Dyn, Dyn,      U8, Flags16, F32, F32, U8],
		_                => &[F32, F32, F32, F32,      U8, Flags16, F32, F32, U8],
	}
}

fn op_98(a: u16, game: Game) -> &'static [Part] {
	use Part::*;
	match a {
		1 => &[F32],
		2 => &[F32],
		6 => &[F32],
		7 => &[F32],
		3 => &[U16, U8],
		1000 => &[F32, U8], // a0100:TK_Enter2 says this is rotation
		1001 => &[F32, U8],
		2000 => &[U8, F32, U8],
		3000 => &[F32, F32, U16, F32],
		4000 => &[Char, F32, U16, U8],
		4001 => &[Str, F32, U16, U8],
		4002 => &[U16],
		5000 => &[F32],
		5001 => &[F32],
		5002 => &[F32],
		6000 => &[U32],
		6001 => &[U32],
		6500 => &[U32],
		7000 => &[U8],
		7001 if game == Game::Reverie => &[Global],
		8000 => &[Str, U8],
		9000 => &[F32],
		10000 => &[F32, F32, F32, F32, F32, F32, F32, F32],
		_ => &[],
	}
}

fn op_c0(a: u16) -> &'static [Part] {
	use Part::*;
	match a {
		1 => &[F32],
		2 => &[F32],
		3 => &[Str, F32, F32, F32, F32, F32, F32],
		4 => &[Str, U8],
		1000 | 1001 | 1003 => &[U16, U16],
		_ => &[],
	}
}

fn op_d2(a: i16) -> &'static [Part] {
	use Part::*;
	match a {
		0 => &[U8],
		3 => &[U8, U8, U32],
		-2 | -1 => &[Dyn],
		_ => &[],
	}
}

#[rustfmt::skip]
fn read_dyn(f: &mut CReader) -> rootcause::Result<Arg> {
	Ok(match f.u8()? {
		0x11 => { let v = f.u8()?; f.check_u32(0)?; Var(v).into() }
		0x33 => { let v = f.u8()?; f.check_u32(0)?; NumReg(v).into() }
		0x44 => { let v = f.u8()?; f.check_u32(0)?; StrReg(v).into() }
		0x55 => { let v = f.u8()?; f.check_u32(0)?; Global(v).into() }
		0xDD => { let v = f.str()?; Arg::Str(v) }
		0xEE => { let v = f.f32()?; f.check_u8(0)?; Arg::F32(v) }
		0xFF => {
			let v = f.i32()?;
			f.check_u8(0)?;
			if v.abs() > 0x1000000 {
				Arg::I32Munged(f32::from_bits(v as u32))
			} else {
				Arg::Int(v as i64)
			}
		}
		code => rootcause::bail!("Unknown dyn code: {code:02X}"),
	})
}

pub fn write(d: &OData, code: &Code) -> rootcause::Result<Writer> {
	let mut f = Writer::new();
	let labels: HashMap<Label, WLabel> = code.ops.iter()
		.filter_map(|op| if let FlatOp::Label(l) = op { Some((*l, WLabel::new())) } else { None })
		.collect();
	for op in &code.ops {
		write_flatop(d, &mut f, op, &labels)?;
	}
	Ok(f)
}

fn write_flatop(d: &OData, f: &mut Writer, op: &FlatOp, labels: &HashMap<Label, WLabel>) -> rootcause::Result<()> {
	let spec = crate::spec::for_game(d.game, d.variant);
	match op {
		FlatOp::Label(l) => {
			f.place(labels[l]);
		}
		FlatOp::Goto(meta, l) => {
			let opcode = spec.by_name.get("goto").copied().context("no opcode for goto")?;
			f.u8(opcode[0]);
			write_meta(d, f, opcode[0], *meta);
			f.label32(d.start, labels[l]);
		}
		FlatOp::If(meta, expr, l) => {
			let opcode = spec.by_name.get("if").copied().context("no opcode for if")?;
			f.u8(opcode[0]);
			write_meta(d, f, opcode[0], *meta);
			expr.write(d, f)?;
			f.label32(d.start, labels[l]);
		}
		FlatOp::Switch(meta, expr, cases, default) => {
			let opcode = spec.by_name.get("switch").copied().context("no opcode for switch")?;
			f.u8(opcode[0]);
			write_meta(d, f, opcode[0], *meta);
			expr.write(d, f)?;
			f.u8(cases.len() as u8);
			for (v, l) in cases {
				match d.game {
					Game::Cs1 | Game::Cs2 | Game::Tx => f.i16(*v as i16),
					_ => f.i32(*v),
				}
				f.label32(d.start, labels[l]);
			}
			f.label32(d.start, labels[default]);
		}
		FlatOp::Op(op) => write_op(d, f, op)?,
	}
	Ok(())
}

fn write_meta(d: &OData, f: &mut Writer, code: u8, meta: OpMeta) {
	if !matches!(code, 0x01 | 0x04) && d.game == Game::Reverie {
		f.u16(meta.line);
		f.u8(0);
		f.u8(if meta.has_width { 0 } else { 0xFF });
	}
}

pub(crate) fn write_op(d: &OData, f: &mut Writer, op: &Op) -> rootcause::Result<()> {
	let spec = crate::spec::for_game(d.game, d.variant);
	let opcode = spec.by_name.get(op.name).copied()
		.context_with(|| format!("no opcode for {:?}", op.name))?;
	crate::ensure!(!opcode.is_empty(), "empty opcode for {:?}", op.name);

	f.u8(opcode[0]);
	write_meta(d, f, opcode[0], op.meta);

	let mut op_spec = spec.ops[opcode[0] as usize].as_ref()
		.context_with(|| format!("opcode {opcode} not in spec"))?;
	let mut cursor = 0usize;
	write_parts(d, f, op, &mut cursor, &op_spec.parts)?;
	for &byte in &opcode[1..] {
		f.u8(byte);
		op_spec = op_spec.child(byte).context_with(|| format!("opcode {opcode} not in spec"))?;
		write_parts(d, f, op, &mut cursor, &op_spec.parts)?;
	}
	crate::ensure!(cursor == op.args.len(), "wrote {} args but op has {}: {op:?}", cursor, op.args.len());
	Ok(())
}

fn take_arg<'a>(op: &'a Op, cursor: &mut usize) -> rootcause::Result<&'a Arg> {
	let arg = op.args.get(*cursor).context_with(|| format!("not enough args in {op:?} at {}", *cursor))?;
	*cursor += 1;
	Ok(arg)
}

fn write_parts(d: &OData, f: &mut Writer, op: &Op, cursor: &mut usize, parts: &[Part]) -> rootcause::Result<()> {
	use Part as P;
	for p in parts {
		match p {
			P::U8 => match take_arg(op, cursor)? { Arg::Int(v) => f.u8(*v as u8), a => rootcause::bail!("expected Int for U8, got {a:?}") },
			P::U16 => match take_arg(op, cursor)? { Arg::Int(v) => f.u16(*v as u16), a => rootcause::bail!("expected Int for U16, got {a:?}") },
			P::U32 => match take_arg(op, cursor)? { Arg::Int(v) => f.u32(*v as u32), a => rootcause::bail!("expected Int for U32, got {a:?}") },
			P::I8 => match take_arg(op, cursor)? { Arg::Int(v) => f.i8(*v as i8), a => rootcause::bail!("expected Int for I8, got {a:?}") },
			P::I16 => match take_arg(op, cursor)? { Arg::Int(v) => f.i16(*v as i16), a => rootcause::bail!("expected Int for I16, got {a:?}") },
			P::I32 => match take_arg(op, cursor)? { Arg::Int(v) => f.i32(*v as i32), a => rootcause::bail!("expected Int for I32, got {a:?}") },
			P::F32 => match take_arg(op, cursor)? { Arg::F32(v) => f.f32(*v), a => rootcause::bail!("expected F32, got {a:?}") },
			P::Str => match take_arg(op, cursor)? { Arg::Str(s) => f.str(d.enc, s)?, a => rootcause::bail!("expected Str, got {a:?}") },

			P::Char => match take_arg(op, cursor)? { Arg::Char(Char(v)) => f.u16(*v), a => rootcause::bail!("expected Char, got {a:?}") },
			P::Item => match take_arg(op, cursor)? { Arg::Item(Item(v)) => f.u16(*v), a => rootcause::bail!("expected Item, got {a:?}") },
			P::Magic => match take_arg(op, cursor)? { Arg::Magic(Magic(v)) => f.u16(*v), a => rootcause::bail!("expected Magic, got {a:?}") },
			P::Flag => match take_arg(op, cursor)? { Arg::Flag(Flag(v)) => f.u16(*v), a => rootcause::bail!("expected Flag, got {a:?}") },
			P::Global => match take_arg(op, cursor)? { Arg::Global(Global(v)) => f.u8(*v), a => rootcause::bail!("expected Global, got {a:?}") },
			P::Var => match take_arg(op, cursor)? { Arg::Var(Var(v)) => f.u8(*v), a => rootcause::bail!("expected Var, got {a:?}") },
			P::FuncArg => match take_arg(op, cursor)? { Arg::FuncArg(FuncArg(v)) => f.u8(*v), a => rootcause::bail!("expected FuncArg, got {a:?}") },
			P::NumReg => match take_arg(op, cursor)? { Arg::NumReg(NumReg(v)) => f.u8(*v), a => rootcause::bail!("expected NumReg, got {a:?}") },
			P::StrReg => match take_arg(op, cursor)? { Arg::StrReg(StrReg(v)) => f.u8(*v), a => rootcause::bail!("expected StrReg, got {a:?}") },
			P::Attr => match take_arg(op, cursor)? { Arg::Attr(Attr(v)) => f.u8(*v), a => rootcause::bail!("expected Attr, got {a:?}") },
			P::CharAttr => match take_arg(op, cursor)? { Arg::CharAttr(CharAttr(Char(c), a)) => { f.u16(*c); f.u8(*a); }, a => rootcause::bail!("expected CharAttr, got {a:?}") },

			P::Flags8 => match take_arg(op, cursor)? { Arg::Flags8(Flags8(v)) => f.u8(*v), a => rootcause::bail!("expected Flags8, got {a:?}") },
			P::Flags16 => match take_arg(op, cursor)? { Arg::Flags16(Flags16(v)) => f.u16(*v), a => rootcause::bail!("expected Flags16, got {a:?}") },
			P::Flags32 => match take_arg(op, cursor)? { Arg::Flags32(Flags32(v)) => f.u32(*v), a => rootcause::bail!("expected Flags32, got {a:?}") },

			P::Expr => match take_arg(op, cursor)? { Arg::Expr(e) => e.write(d, f)?, a => rootcause::bail!("expected Expr, got {a:?}") },
			P::Text => match take_arg(op, cursor)? { Arg::Text(t) => t.write(d.enc, f)?, a => rootcause::bail!("expected Text, got {a:?}") },
			P::Dyn => write_dyn(f, d, take_arg(op, cursor)?)?,
			P::Ndyn => {
				let n = op.args.len() - *cursor;
				f.u8(n as u8);
				for _ in 0..n {
					write_dyn(f, d, take_arg(op, cursor)?)?;
				}
			}
			P::Dync => {
				let arg = take_arg(op, cursor)?;
				match arg {
					Arg::Char(Char(v)) => write_dyn(f, d, &Arg::Int(*v as i64))?,
					_ => write_dyn(f, d, arg)?,
				}
			}

			P::Cs1_36 => {
				if matches!(op.args[1], Arg::Char(Char(0xFE02..=0xFE03))) {
					write_parts(d, f, op, cursor, &[P::F32])?;
				}
			}
			P::Cs1_3C => {
				if matches!(op.args[1], Arg::Char(Char(0xFFFF))) {
					write_parts(d, f, op, cursor, &[P::U32, P::U32, P::U32])?;
				}
			}

			P::Cs2_37 => {
				if matches!(op.args[1], Arg::Char(Char(0xFE04))) {
					write_parts(d, f, op, cursor, &[P::Str])?;
				}
			}

			P::Tx_3C => {
				if matches!(op.args[0], Arg::Int(1)) {
					write_parts(d, f, op, cursor, &[P::U32, P::U32, P::U32])?;
				}
			}
			P::Tx_isforceload => {
				if matches!(op.args.get(*cursor), Some(Arg::Str(s)) if s == "isforceload") {
					f.slice(b"isforceload");
					*cursor += 1;
				} else {
					write_parts(d, f, op, cursor, &[P::U8])?;
				}
			}
			P::Tx_2F => {
				if matches!(op.args.get(*cursor), Some(Arg::Str(s)) if s == "AniWvWait") {
					f.slice(b"AniWvWait\xFF");
					*cursor += 2;
					write_parts(d, f, op, cursor, &[P::U8, P::F32, P::F32, P::F32])?;
					break;
				}
			}

			P::Cs3_98 => {
				let Arg::Int(v) = op.args[0] else {
					rootcause::bail!("Expected Int for Cs3_98 dispatch");
				};
				write_parts(d, f, op, cursor, op_98(v as u16, d.game))?;
			}
			P::Cs3_c0 => {
				let Arg::Int(v) = op.args[0] else {
					rootcause::bail!("Expected Int for Cs3_c0 dispatch");
				};
				write_parts(d, f, op, cursor, op_c0(v as u16))?;
			}

			P::Cs4_40 => {
				let Arg::Char(v) = op.args[1] else {
					rootcause::bail!("Expected Char for Cs4_40 dispatch");
				};
				write_parts(d, f, op, cursor, op_40(v))?;
			}
			P::Cs4_wtf_are_you_doing => {
				if *cursor < op.args.len() {
					match op.args[*cursor] {
						Arg::Int(0) => { f.u32(0); *cursor += 1; }
						_ => {}
					}
				}
			}

			P::Rev_3E => {
				match op.args[1] {
					Arg::Char(Char(0xFE12)) => write_parts(d, f, op, cursor, &[P::U8])?,
					Arg::Char(Char(0xFE13)) => write_parts(d, f, op, cursor, &[P::F32])?,
					Arg::Char(Char(0xFFFF)) => {
						if *cursor < op.args.len() {
							write_parts(d, f, op, cursor, &[P::U8, P::U8, P::U8])?;
						}
					}
					_ => {}
				}
			}
			P::Rev_D2 => {
				let Arg::Int(v) = op.args[0] else {
					rootcause::bail!("Expected Int for Rev_D2 dispatch");
				};
				write_parts(d, f, op, cursor, op_d2(v as i16))?;
			}
			P::Rev_79 => {
				if matches!(op.args[0], Arg::Int(7)) {
					write_parts(d, f, op, cursor, &[P::U8])?;
				}
			}
			P::Rev_E002 => {
				match op.args[1] {
					Arg::Char(Char(0xFFFF)) => write_parts(d, f, op, cursor, &[P::I32])?,
					_ => write_parts(d, f, op, cursor, &[P::F32])?,
				}
			}

			P::Print | P::Fail => {}
		}
	}
	Ok(())
}

fn write_dyn(f: &mut Writer, _d: &OData, arg: &Arg) -> rootcause::Result<()> {
	match arg {
		Arg::Var(Var(v)) => { f.u8(0x11); f.u8(*v); f.u32(0); }
		Arg::NumReg(NumReg(v)) => { f.u8(0x33); f.u8(*v); f.u32(0); }
		Arg::StrReg(StrReg(v)) => { f.u8(0x44); f.u8(*v); f.u32(0); }
		Arg::Global(Global(v)) => { f.u8(0x55); f.u8(*v); f.u32(0); }
		Arg::Str(s) => {
			f.u8(0xDD);
			f.slice(s.as_bytes());
			f.u8(0);
		}
		Arg::F32(v) => { f.u8(0xEE); f.f32(*v); f.u8(0); }
		Arg::Int(v) => { f.u8(0xFF); f.i32(*v as i32); f.u8(0); }
		Arg::I32Munged(v) => { f.u8(0xFF); f.i32(v.to_bits() as i32); f.u8(0); }
		other => rootcause::bail!("can't write {other:?} as dyn"),
	}
	Ok(())
}

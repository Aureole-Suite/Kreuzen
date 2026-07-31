use std::cell::Cell;
use std::collections::{BTreeSet, HashMap};
use std::ops::Range;
use std::rc::Rc;

use gospel::read::{Le as _, Reader};
use gospel::write::{Label as WLabel, Le as _, Writer};
use rootcause::option_ext::OptionExt as _;
use rootcause::prelude::ResultExt as _;

use crate::Game;
use crate::expr::Expr;
use crate::io::{CReader, OData, WriterExt as _};
use crate::spec::{Opcode, Part};
use crate::text::Text;
use crate::types::*;

pub mod preload;
pub mod shadow;
mod to_dot;
pub use to_dot::to_dot;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Label(pub u32);

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
		writeln!(f, "Context:")?;
		for (Label(l), op) in self.0.iter() {
			writeln!(f, "  {l:04X} {op:?}")?;
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

fn read_one_op(f: &mut CReader, ops: &mut Vec<(Label, FlatOp)>) -> rootcause::Result<()> {
	let pos = f.pos();
	let op = read_op(f)
		.context_with(|| format!("Failed to read op at {pos:04X}"))
		.attach_with(|| OpContext(std::mem::take(ops)))
		.attach_with(|| Hexdump(format!("{:#2.48X}", f.dump().start(pos).mark(f.pos()))))?;
	ops.push((Label(pos as u32), op));
	Ok(())
}

pub(crate) fn read_code_chunk(f: &mut CReader, s: (usize, usize)) -> rootcause::Result<Vec<FlatOp>> {
	let (start, end) = s;
	let end = end.min(f.outline_start);
	let d = f.data();
	crate::ensure!(start <= end && end <= d.len());
	let mut g = CReader { reader: Reader::new(&d[..end]).at(start)?, ..*f };

	let mut ops = Vec::new();
	while !g.remaining().is_empty() {
		if g.check_u8(0).is_ok() {
			break;
		}
		read_one_op(&mut g, &mut ops)?;
	}

	let s = Label(s.0 as _)..Label(s.1 as _);

	if (f.game, f.scena) == (Game::Cs3, "system") {
		fix_broken_label(&mut ops, s.clone());
	}
	resolve_outlines(f, &mut ops, s)
		.context("resolving outlines")
		.attach_with(|| OpContext(std::mem::take(&mut ops)))?;
	let mut ops = insert_labels(&ops)
		.context("could not resolve labels")
		.attach_with(|| OpContext(std::mem::take(&mut ops)))?;
	remap_labels(&mut ops);

	if !g.remaining().iter().all(|x| *x == 0) {
		tracing::warn!("Data remaining after function: {:02X?}", g.remaining());
	}
	f.seek(end)?;
	Ok(ops)
}

/// Fixes up a label in cs3's `system` that points into the middle of an unrelated function.
///
/// It should point two ops past the branch that uses it.
fn fix_broken_label(ops: &mut [(Label, FlatOp)], s: Range<Label>) {
	const WEIRD_LABEL: Label = Label(10651);

	if !s.contains(&WEIRD_LABEL)
		&& let Some(i) = ops.iter().position(|(_, op)| matches!(op, FlatOp::If(_, _, WEIRD_LABEL)))
		&& let Some(&(target, _)) = ops.get(i + 2)
	{
		tracing::warn!("Fixing up broken label");
		let (_, FlatOp::If(_, _, l)) = &mut ops[i] else { unreachable!() };
		*l = target;
	}
}

/// Resolves labels that point outside the function.
///
/// Some scripts have been edited with hex editors, which cannot resize a function. To make room,
/// those replace a `goto` and a number of `nop`s with a jump to unused space elsewhere in the file,
/// where the new code sits followed by a jump back. This can't be written back out the way it came
/// in, but the inlined code is equivalent.
fn resolve_outlines(f: &mut CReader, ops: &mut Vec<(Label, FlatOp)>, s: Range<Label>) -> rootcause::Result<()> {
	let mut index = 0;
	while index < ops.len() {
		if let (l, FlatOp::Goto(meta, l1)) = ops[index]
			&& !s.contains(&l1)
		{
			tracing::warn!("restoring outlined code from {l1} to {l}");
			f.seek(l1.0 as usize)?;
			f.outline_start = (f.outline_start).min(l1.0 as usize);
			let mut body = Vec::new();
			// Add in nops instead of the gotos, both as a visual marker and to keep the label/meta
			body.push((l, FlatOp::Op(Op { name: "nop", meta, args: Vec::new() })));
			let l2 = loop {
				read_one_op(f, &mut body)?;
				if let (l, FlatOp::Goto(meta, l2)) = *body.last().unwrap()
					&& s.contains(&l2)
				{
					body.pop();
					body.push((l, FlatOp::Op(Op { name: "nop", meta, args: Vec::new() })));
					break l2;
				}
			};

			let end_index = ops.iter().position(|o| o.0 == l2).context_with(|| format!("could not find {l2}"))?;
			crate::ensure!(end_index > index);
			let other = ops.splice(index..end_index, body);
			for (_, op) in other {
				if !matches!(&op, FlatOp::Op(op) if op.name == "nop") {
					tracing::warn!("outline replaces non-nop op {op:?}");
				}
			}
		} else {
			index += 1;
		}
	}
	Ok(())
}

fn insert_labels(ops: &[(Label, FlatOp)]) -> rootcause::Result<Vec<FlatOp>> {
	let mut labels = BTreeSet::new();
	for (_, op) in ops {
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
		if labels.remove(pos) {
			ops2.push(FlatOp::Label(*pos));
		}
		ops2.push(op.clone());
	}

	crate::ensure!(labels.is_empty(), "Some labels were not used: {labels:?}");
	Ok(ops2)
}

pub(crate) fn remap_labels(ops2: &mut Vec<FlatOp>) {
	use std::collections::HashSet;

	let mut used = HashSet::new();
	for op in ops2.iter() {
		match op {
			FlatOp::Op(_) | FlatOp::Label(_) => {}
			FlatOp::Goto(_, l) | FlatOp::If(_, _, l) => {
				used.insert(*l);
			}
			FlatOp::Switch(_, _, ls, l) => {
				for (_, l) in ls {
					used.insert(*l);
				}
				used.insert(*l);
			}
		}
	}

	let mut order = HashMap::<Label, Label>::new();
	let mut n = 0;
	let mut current = None;
	ops2.retain_mut(|op| match op {
		FlatOp::Label(l) => {
			if !used.contains(l) {
				false
			} else if let Some(current) = current {
				order.insert(*l, current);
				false
			} else {
				let new = Label(n);
				n += 1;
				order.insert(*l, new);
				current = Some(new);
				true
			}
		}
		_ => {
			current = None;
			true
		}
	});

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
	pub width: u8, // means different things for different ops; most just have 0 meaning computed
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
		match self.width {
			0 => {}
			1 => write!(f, "~")?,
			n => write!(f, "~{n}")?,
		}
		Ok(())
	}
}

#[derive(Clone, PartialEq)]
pub struct Op {
	pub name: &'static str,
	pub meta: OpMeta,
	pub args: Vec<Arg>,
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
#[rustfmt::skip]
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
	Battle(i32, crate::types::Battle),
	#[debug("{_0:?}")] Magic(crate::types::Magic),
	#[debug("{_0:?}")] Sound(crate::types::Sound),
	#[debug("{_0:?}")] Music(crate::types::Music),
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
	#[debug("{_0:?}")] SystemFlags(crate::types::SystemFlags),

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
	let op_start = f.pos();
	let mut code = f.u8()?;
	let mut opcode = Opcode::new(&[code]);

	let spec = crate::spec::for_game(f.game, f.variant);
	let mut op_spec = match spec.ops[code as usize].as_ref() {
		Some(it) => it,
		None => {
			rootcause::bail!("_Unknown opcode {opcode}")
		}
	};

	let has_meta = has_meta(f.game, code);
	let mut meta = OpMeta::default();
	let mut raw_width = 0;
	if has_meta {
		meta.line = f.u16()?;
		f.check_u8(0)?;
		raw_width = f.u8()?;
	}

	match op_spec.name.as_str() {
		"if" => {
			let expr = Expr::read(f)?;
			let label = Label(f.u32()?);
			meta.width = if raw_width == 0xFF { 0 } else { raw_width };
			return Ok(FlatOp::If(meta, expr, label));
		}
		"goto" => {
			let label = Label(f.u32()?);
			meta.width = if raw_width == 0xFF { 0 } else { raw_width };
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

	if has_meta && has_ambiguous_width(op.name) {
		let actual = f.pos() - op_start;
		let actual_b = actual.min(0xFF) as u8;
		op.meta.width = if raw_width == actual_b { 0 } else { 1 };
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
			P::F32 => {
				let v = f.f32()?;
				op.args.push(if v == 0.0 || (1e-5..=1e8).contains(&v.abs()) {
					Arg::F32(v)
				} else {
					Arg::F32Munged(v.to_bits().cast_signed())
				});
			}
			P::Pos => read_parts(op, f, &[P::F32, P::F32, P::F32])?,
			P::Str => op.args.push(f.str()?.into()),

			P::Char => op.args.push(Char(f.u16()?).into()),
			P::Item => op.args.push(Item(f.u16()?).into()),
			P::Battle => op.args.push(Arg::Battle(f.i32()?, Battle(f.u8()? as u32))),
			P::Magic => op.args.push(Magic(f.u16()?).into()),
			P::Sound => op.args.push(Sound(f.u16()?).into()),
			P::Music => op.args.push(Music(f.u16()?).into()),
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
			P::SystemFlags => op.args.push(Arg::SystemFlags(f.u32()?.into())),

			P::Expr => op.args.push(self::Expr::read(f)?.into()),
			P::Text => op.args.push(self::Text::read(f)?.into()),
			P::Dyn => op.args.push(read_dyn(f)?),
			P::Ndyn => {
				for _ in 0..f.u8()? {
					op.args.push(read_dyn(f)?);
				}
			}
			P::Dyn_Char => op.args.push(match read_dyn(f)? {
				Arg::Int(v) => Arg::Char(Char(v.try_into()?)),
				a => a,
			}),
			P::Dyn_Sound => op.args.push(match read_dyn(f)? {
				Arg::Int(v) => Arg::Sound(Sound(v.try_into()?)),
				a => a,
			}),

			P::CharMoveTo | P::Cs1_3C | P::Cs3_98 | P::Cs3_c0 | P::Rev_79 | P::Rev_D2 | P::Rev_E002 => {
				read_parts(op, f, extra_parts(p, &op.args, f.game)?)?;
			}

			P::Tx_isforceload => {
				if f.scena == "a0005" && f.check(b"isforceload").is_ok() {
					// for some reason this one put the name of the flag rather than value
					op.args.push(Arg::Str("isforceload".to_string()));
				} else {
					read_parts(op, f, &[P::U8])?;
				}
			}
			P::Tx_CharAniLoop => {
				if f.scena == "t5110" && f.check(b"AniWvWait\xFF").is_ok() {
					op.args.push(Arg::Str("AniWvWait".to_string()));
					op.args.push(Arg::Int(0xFF));
					read_parts(op, f, &[P::U8, P::F32, P::F32, P::F32])?;
					break;
				}
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
			P::Print => println!("{op:?}"),
			P::Fail => rootcause::bail!("Fail"),
		}
	}
	Ok(())
}

/// Get the extra parts implied by a conditional part.
///
/// Errors if the given part is not a conditional one.
/// Should perhaps refactor that into a separate enum someday. Who cares.
pub fn extra_parts(p: &Part, args: &[Arg], game: Game) -> rootcause::Result<&'static [Part]> {
	use Part as P;
	let arg = |i: usize| args.get(i).context_with(|| format!("no arg {i} for conditional part {p:?}"));
	Ok(match p {
		P::Cs1_3C => match arg(1)? {
			Arg::Char(Char(0xFFFF)) => &[P::F32, P::F32, P::F32],
			_ => &[],
		},
		P::Cs3_98 => {
			let Arg::Int(v) = arg(0)? else {
				rootcause::bail!("Expected U16 for Cs3_98 part");
			};
			op_98(*v as u16, game)
		}
		P::Cs3_c0 => {
			let Arg::Int(v) = arg(0)? else {
				rootcause::bail!("Expected U16 for Cs3_c0 part");
			};
			op_c0(*v as u16)
		}
		P::CharMoveTo => {
			let Arg::Char(v) = arg(1)? else {
				rootcause::bail!("Expected Char for CharMoveTo part");
			};
			op_char_move_to(*v, game)
		}
		P::Rev_79 => match arg(0)? {
			Arg::Int(7) => &[P::U8],
			_ => &[],
		},
		P::Rev_D2 => {
			let Arg::Int(v) = arg(0)? else {
				rootcause::bail!("Expected I16 for Rev_D2 part");
			};
			op_d2(*v as i16)
		}
		P::Rev_E002 => match arg(1)? {
			Arg::Char(Char(0xFFFF)) => &[P::I32],
			_ => &[P::F32],
		},
		_ => rootcause::bail!("{p:?} is not an arg-driven conditional part"),
	})
}

#[rustfmt::skip]
fn op_char_move_to(a: crate::types::Char, game: Game) -> &'static [Part] {
	use Part::*;
	match game {
		Game::Cs4 | Game::Reverie => match a.0 {
			0xFE02..=0xFE04 => &[F32, F32, F32, F32, F32, U8, Flags16, F32, F32, U8],
			0xFE05          => &[F32, F32, F32, F32,      U8, Flags16, F32, F32, U8, Str],
			0xFE15          => &[Dyn, Dyn, Dyn, Dyn,      U8, Flags16, F32, F32, U8],
			_               => &[F32, F32, F32, F32,      U8, Flags16, F32, F32, U8],
		},
		_ => match a.0 {
			0xFE02..=0xFE03 => &[F32, F32, F32, F32, F32, U8, Flags16],
			0xFE04          => &[F32, F32, F32, F32,      U8, Flags16, Str],
			_               => &[F32, F32, F32, F32,      U8, Flags16],
		},
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
		6500 => &[F32],
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

pub fn write(d: &OData, code: &[FlatOp]) -> rootcause::Result<Writer> {
	let mut f = Writer::new();
	let labels: HashMap<Label, WLabel> = code
		.iter()
		.filter_map(|op| if let FlatOp::Label(l) = op { Some((*l, WLabel::new())) } else { None })
		.collect();
	for op in code {
		write_flatop(d, &mut f, op, &labels)?;
	}
	Ok(f)
}

fn write_flatop(d: &OData, f: &mut Writer, op: &FlatOp, labels: &HashMap<Label, WLabel>) -> rootcause::Result<()> {
	match op {
		FlatOp::Label(l) => {
			f.place(labels[l]);
		}
		FlatOp::Goto(meta, l) => {
			let width = if meta.width == 0 { 0xFF } else { meta.width };
			write_branch_meta(d, f, "goto", meta.line, width)?;
			f.label32(d.start, labels[l]);
		}
		FlatOp::If(meta, expr, l) => {
			let width = if meta.width == 0 { 0xFF } else { meta.width };
			write_branch_meta(d, f, "if", meta.line, width)?;
			expr.write(d, f)?;
			f.label32(d.start, labels[l]);
		}
		FlatOp::Switch(meta, expr, cases, default) => {
			write_branch_meta(d, f, "switch", meta.line, 255)?;
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

fn write_branch_meta(d: &OData, f: &mut Writer, name: &'static str, line: u16, width: u8) -> rootcause::Result<()> {
	let spec = crate::spec::for_game(d.game, d.variant);
	let opcode = spec.by_name.get(name).copied().context_with(|| format!("no opcode for {name:?}"))?;
	crate::ensure!(!opcode.is_empty(), "empty opcode for {:?}", name);

	f.u8(opcode[0]);
	if has_meta(d.game, opcode[0]) {
		f.u16(line);
		f.u8(0);
		f.u8(width);
	}

	Ok(())
}

pub(crate) fn write_op(d: &OData, f: &mut Writer, op: &Op) -> rootcause::Result<()> {
	let spec = crate::spec::for_game(d.game, d.variant);
	let opcode = spec
		.by_name
		.get(op.name)
		.copied()
		.context_with(|| format!("no opcode for {:?}", op.name))?;
	crate::ensure!(!opcode.is_empty(), "empty opcode for {:?}", op.name);

	let op_start = f.len();
	let width = Rc::new(Cell::new(0u8));
	f.u8(opcode[0]);
	if has_meta(d.game, opcode[0]) {
		f.u16(op.meta.line);
		f.u8(0);
		let cell = width.clone();
		f.delay(move |_| Some([cell.get()]));
	}

	let mut op_spec = spec.ops[opcode[0] as usize]
		.as_ref()
		.context_with(|| format!("opcode {opcode} not in spec"))?;
	let mut cursor = 0usize;
	let mut op_end = 0usize;
	write_parts(d, f, op, &mut cursor, &op_spec.parts, &mut op_end)?;
	for &byte in &opcode[1..] {
		f.u8(byte);
		op_spec = op_spec.child(byte).context_with(|| format!("opcode {opcode} not in spec"))?;
		write_parts(d, f, op, &mut cursor, &op_spec.parts, &mut op_end)?;
	}
	crate::ensure!(cursor == op.args.len(), "wrote {} args but op has {}: {op:?}", cursor, op.args.len());

	if has_ambiguous_width(op.name) && op.meta.width == 1 {
		width.set(0xFF)
	} else {
		if op_end == 0 {
			op_end = f.len();
		}
		width.set(u8::try_from(op_end - op_start).unwrap_or(0xFF))
	};
	Ok(())
}

fn take_arg<'a>(op: &'a Op, cursor: &mut usize) -> rootcause::Result<&'a Arg> {
	let arg = op
		.args
		.get(*cursor)
		.context_with(|| format!("not enough args in {op:?} at {}", *cursor))?;
	*cursor += 1;
	Ok(arg)
}

fn write_parts(d: &OData, f: &mut Writer, op: &Op, cursor: &mut usize, parts: &[Part], op_end: &mut usize) -> rootcause::Result<()> {
	macro_rules! arg {
		($variant:ident) => {
			match take_arg(op, cursor)? {
				Arg::$variant(v) => v,
				a => {
					rootcause::bail!("expected {} in {}, got {a:?}", stringify!($variant), op.name)
				}
			}
		};
	}
	macro_rules! int {
		($ty:ty) => {{
			let v = *arg!(Int);
			<$ty>::try_from(v).context_with(|| format!("{v} out of range for {} in {}", stringify!($ty), op.name))?
		}};
	}

	use Part as P;
	for p in parts {
		match p {
			P::U8 => f.u8(int!(u8)),
			P::U16 => f.u16(int!(u16)),
			P::U32 => f.u32(int!(u32)),
			P::I8 => f.i8(int!(i8)),
			P::I16 => f.i16(int!(i16)),
			P::I32 => f.i32(int!(i32)),
			P::F32 => match take_arg(op, cursor)? {
				Arg::F32(v) => f.f32(*v),
				Arg::F32Munged(v) => f.i32(*v),
				a => rootcause::bail!("expected F32 in {}, got {a:?}", op.name),
			},
			P::Str => f.str(d.enc, arg!(Str))?,

			P::Char => f.u16(arg!(Char).0),
			P::Item => f.u16(arg!(Item).0),
			P::Battle => match take_arg(op, cursor)? {
				Arg::Battle(a, v) => {
					f.i32(*a);
					f.u8(v.0.try_into()?);
				}
				a => {
					rootcause::bail!("expected {} in {}, got {a:?}", stringify!(Battle), op.name)
				}
			},
			P::Magic => f.u16(arg!(Magic).0),
			P::Sound => f.u16(arg!(Sound).0),
			P::Music => f.u16(arg!(Music).0),
			P::Flag => f.u16(arg!(Flag).0),
			P::Global => f.u8(arg!(Global).0),
			P::Var => f.u8(arg!(Var).0),
			P::FuncArg => f.u8(arg!(FuncArg).0),
			P::NumReg => f.u8(arg!(NumReg).0),
			P::StrReg => f.u8(arg!(StrReg).0),
			P::Attr => f.u8(arg!(Attr).0),
			P::CharAttr => {
				let v = arg!(CharAttr);
				f.u16(v.0.0);
				f.u8(v.1);
			}

			P::Flags8 => f.u8(arg!(Flags8).0),
			P::Flags16 => f.u16(arg!(Flags16).0),
			P::Flags32 => f.u32(arg!(Flags32).0),
			P::SystemFlags => f.u32(arg!(SystemFlags).0.0),

			P::Expr => {
				*op_end = f.len();
				arg!(Expr).write(d, f)?
			}
			P::Text => arg!(Text).write(d.enc, f)?,
			P::Dyn => write_dyn(f, d, take_arg(op, cursor)?)?,
			P::Ndyn => {
				let n = op.args.len() - *cursor;
				f.u8(n as u8);
				for _ in 0..n {
					write_dyn(f, d, take_arg(op, cursor)?)?;
				}
			}
			P::Dyn_Char => match take_arg(op, cursor)? {
				Arg::Char(Char(v)) => write_dyn(f, d, &Arg::Int(*v as i64))?,
				arg => write_dyn(f, d, arg)?,
			},
			P::Dyn_Sound => match take_arg(op, cursor)? {
				Arg::Sound(Sound(v)) => write_dyn(f, d, &Arg::Int(*v as i64))?,
				arg => write_dyn(f, d, arg)?,
			},

			P::CharMoveTo | P::Cs1_3C | P::Cs3_98 | P::Cs3_c0 | P::Rev_79 | P::Rev_D2 | P::Rev_E002 => {
				write_parts(d, f, op, cursor, extra_parts(p, &op.args, d.game)?, op_end)?;
			}

			P::Tx_isforceload => {
				if matches!(op.args.get(*cursor), Some(Arg::Str(s)) if s == "isforceload") {
					f.slice(b"isforceload");
					*cursor += 1;
				} else {
					write_parts(d, f, op, cursor, &[P::U8], op_end)?;
				}
			}
			P::Tx_CharAniLoop => {
				if matches!(op.args.get(*cursor), Some(Arg::Str(s)) if s == "AniWvWait") {
					f.slice(b"AniWvWait\xFF");
					*cursor += 2;
					write_parts(d, f, op, cursor, &[P::U8, P::F32, P::F32, P::F32], op_end)?;
					break;
				}
			}

			#[expect(clippy::single_match)]
			P::Cs4_wtf_are_you_doing => {
				if *cursor < op.args.len() {
					match op.args[*cursor] {
						Arg::Int(0) => {
							f.u32(0);
							*cursor += 1;
						}
						_ => {}
					}
				}
			}

			#[expect(clippy::collapsible_match)]
			P::Rev_3E => match op.args[1] {
				Arg::Char(Char(0xFE12)) => write_parts(d, f, op, cursor, &[P::U8], op_end)?,
				Arg::Char(Char(0xFE13)) => write_parts(d, f, op, cursor, &[P::F32], op_end)?,
				Arg::Char(Char(0xFFFF)) => {
					if *cursor < op.args.len() {
						write_parts(d, f, op, cursor, &[P::U8, P::U8, P::U8], op_end)?;
					}
				}
				_ => {}
			},
			P::Pos => write_parts(d, f, op, cursor, &[P::F32, P::F32, P::F32], op_end)?,

			P::Print | P::Fail => {}
		}
	}
	Ok(())
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

#[rustfmt::skip]
fn write_dyn(f: &mut Writer, _d: &OData, arg: &Arg) -> rootcause::Result<()> {
	match arg {
		Arg::Var(v) => { f.u8(0x11); f.u8(v.0); f.u32(0); }
		Arg::NumReg(v) => { f.u8(0x33); f.u8(v.0); f.u32(0); }
		Arg::StrReg(v) => { f.u8(0x44); f.u8(v.0); f.u32(0); }
		Arg::Global(v) => { f.u8(0x55); f.u8(v.0); f.u32(0); }
		Arg::Str(s) => { f.u8(0xDD); f.slice(s.as_bytes()); f.u8(0); }
		Arg::F32(v) => { f.u8(0xEE); f.f32(*v); f.u8(0); }
		Arg::Int(v) => { f.u8(0xFF); f.i32(*v as i32); f.u8(0); }
		Arg::I32Munged(v) => { f.u8(0xFF); f.i32(v.to_bits() as i32); f.u8(0); }
		other => rootcause::bail!("can't write {other:?} as dyn"),
	}
	Ok(())
}

fn has_meta(game: Game, code: u8) -> bool {
	game == Game::Reverie && !matches!(code, 0x01 | 0x04)
}

fn has_ambiguous_width(op: &str) -> bool {
	matches!(op, "call" | "Fork" | "TextTalk" | "TextShow")
}

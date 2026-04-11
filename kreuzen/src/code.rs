use std::collections::BTreeSet;

use eyre::Context as _;
use gospel::read::Le as _;

use crate::io::CReader;
use crate::spec::{Opcode, Part};
pub mod expr;
pub mod text;
pub use expr::Expr;
pub use text::Text;
use crate::{Game, types::*};

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

pub fn decompile(f: &mut CReader, mut end: usize) -> eyre::Result<()> {
	let orig_end = end;
	while end > 0 && f.data()[end - 1] == 0 {
		end -= 1;
	}
	let mut ops = Vec::new();
	while f.pos() < end {
		let pos = Label(f.pos() as u32);
		let op = read_op(f).with_context(|| format!("Failed to read op at {pos:?}"))?;
		tracing::trace!("Read op at {pos:?}: {op:?}");
		ops.push((pos, op))
	}
	if f.pos() != end {
		tracing::warn!("Expected to end at {end:X} but ended at {:X}", f.pos());
	}
	f.seek(orig_end)?;

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

	const WEIRD_LABEL: Label = Label(0x299B);
	if labels.len() == 1
	&& f.game == Game::Cs3
	&& f.scena == "system"
	&& let Some(if_loc) = ops2.iter().position(|op| matches!(op, FlatOp::If(_, _, WEIRD_LABEL)))
	&& labels.remove(&WEIRD_LABEL)
	{
		tracing::warn!("Fixing up broken label");
		ops2.insert(if_loc + 2, FlatOp::Label(WEIRD_LABEL));
	}

	eyre::ensure!(labels.is_empty(), "Some labels were not used: {labels:?}");
	// let decomp = decompile::decompile(&ops2).context(DecompileSnafu { code: ops2 })?;
	// Ok(decomp)

	Ok(())
}

#[derive(Clone, Copy, PartialEq, Eq, Default)]
pub struct OpMeta {
	pub line: u16,
	pub has_width: bool, // width != 0xFF
}

impl OpMeta {
	fn fmt<'a, 'b>(&self, f: &'a mut std::fmt::Formatter<'b>) -> Result<&'a mut std::fmt::Formatter<'b>, std::fmt::Error> {
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
	#[debug("{_0:?}")] U8(u8),
	#[debug("{_0:?}")] U16(u16),
	#[debug("{_0:?}")] U32(u32),
	#[debug("{_0:?}")] I8(i8),
	#[debug("{_0:?}")] I16(i16),
	#[debug("{_0:?}")] I32(i32),
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

#[derive(Clone, PartialEq, Debug)]
pub enum FlatOp {
	Op(Op),
	Label(Label),
	Goto(OpMeta, Label),
	If(OpMeta, Expr, Label),
	Switch(OpMeta, Expr, Vec<(i32, Label)>, Label),
}

fn read_op(f: &mut CReader) -> eyre::Result<FlatOp> {
	let mut code = f.u8()?;
	let mut opcode = Opcode::new(&[code]);

	let spec = crate::spec::for_game(f.game, f.variant);
	let name = spec.names.get(&opcode).map(|s| s.as_str());

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

	let mut op_spec = match spec.ops[code as usize].as_ref() {
		Some(it) => it,
		None => {
			eyre::bail!("_Unknown opcode {opcode}")
		}
	};

	let mut op = Op {
		name: spec.names.get(&opcode).unwrap(),
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
				None => eyre::bail!("_Unknown opcode {opcode}"),
			};
			op.name = spec.names.get(&opcode).unwrap();
		} else {
			break;
		}
	}

	Ok(FlatOp::Op(op))
}

fn read_parts(op: &mut Op, f: &mut CReader, parts: &[Part]) -> eyre::Result<()> {
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
				if matches!(op.args[0], Arg::U16(1)) {
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
					op.args.push(Arg::U8(0xFF));
					read_parts(op, f, &[P::U8, P::F32, P::F32, P::F32])?;
					break;
				}
			}

			P::Cs3_98 => {
				let Arg::U16(v) = op.args[0] else {
					eyre::bail!("Expected U16 for Cs3_c0 part");
				};
				read_parts(op, f, op_98(v, f.game))?;
			}
			P::Cs3_c0 => {
				let Arg::U16(v) = op.args[0] else {
					eyre::bail!("Expected U16 for Cs3_c0 part");
				};
				read_parts(op, f, op_c0(v))?;
			}

			P::Cs4_40 => {
				let Arg::Char(v) = op.args[1] else {
					eyre::bail!("Expected Char");
				};
				read_parts(op, f, op_40(v))?;
			}
			P::Cs4_wtf_are_you_doing => {
				if f.scena == "mg11" && f.check_u32(0).is_ok() {
					op.args.push(Arg::U32(0)); // This one is only there in the japanese version
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
				let Arg::I16(v) = op.args[0] else {
					eyre::bail!("Expected I16");
				};
				read_parts(op, f, op_d2(v))?;
			}
			P::Rev_79 => {
				if matches!(op.args[0], Arg::U8(7)) {
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
			P::Fail => eyre::bail!("Fail"),
		}
	}
	Ok(())
}

fn cs4_weird_sound_play() -> &'static [Part] {
	use Part::*;
	&[
		Dyn, F32, Dyn, F32,
		F32, U16, Char, F32,
		F32, F32, F32, Str,
		U16, U16, U16, U16,
		U16, I16, I16, U16,
		F32,
	]
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
fn read_dyn(f: &mut CReader) -> eyre::Result<Arg> {
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
				Arg::I32(v)
			}
		}
		code => eyre::bail!("Unknown dyn code: {code:02X}"),
	})
}

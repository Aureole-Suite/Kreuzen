use arrayvec::ArrayVec;
use gospel::read::Le as _;
use snafu::ensure;

use crate::{ReaderaExt as _, VReader};

#[derive(Debug, snafu::Snafu)]
pub enum EffectError {
	#[snafu(display("invalid read (at {location})"), context(false))]
	Read {
		source: gospel::read::Error,
		#[snafu(implicit)]
		location: snafu::Location,
	},
	#[snafu(display("mismatch between kind and params: {effect:?}"))]
	BadEffect { effect: RawEffect },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Effect {
	_02(String), // C_CHRX10
	_03(String), // battle/atk000_2.eff
	_04(u32),
	_05(u32),
	_07(u32),
	_09(u16, String), // charid, BTL_CRAFT00_01
	_0A(String), // BTL_CRAFT01_02_GS
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RawEffect {
	pub kind: u16,
	pub charid: u16,
	pub u32: u32,
	pub str: String,
}

pub fn read_effect(f: &mut VReader) -> Result<Vec<Effect>, EffectError> {
	let mut table = Vec::new();
	while !f.remaining().is_empty() {
		let effect = RawEffect {
			kind: f.u16()?,
			charid: f.u16()?,
			u32: f.u32()?,
			str: f.sstr(32)?,
		};
		table.push(match effect.kind {
			0 => {
				ensure!(effect.charid == 0xFFFF, BadEffectSnafu { effect });
				ensure!(effect.str.is_empty(), BadEffectSnafu { effect });
				ensure!(effect.u32 == 0, BadEffectSnafu { effect });
				break;
			}
			2 => {
				ensure!(effect.charid == 0xFFFF, BadEffectSnafu { effect });
				ensure!(effect.u32 == 0, BadEffectSnafu { effect });
				Effect::_02(effect.str)
			}
			3 => {
				ensure!(effect.charid == 0xFFFF, BadEffectSnafu { effect });
				ensure!(effect.u32 == 0, BadEffectSnafu { effect });
				Effect::_03(effect.str)
			}
			4 => {
				ensure!(effect.charid == 0xFFFF, BadEffectSnafu { effect });
				ensure!(effect.str.is_empty(), BadEffectSnafu { effect });
				Effect::_04(effect.u32)
			}
			5 => {
				ensure!(effect.charid == 0xFFFF, BadEffectSnafu { effect });
				ensure!(effect.str.is_empty(), BadEffectSnafu { effect });
				Effect::_05(effect.u32)
			}
			7 => {
				ensure!(effect.charid == 0xFFFF, BadEffectSnafu { effect });
				ensure!(effect.str.is_empty(), BadEffectSnafu { effect });
				Effect::_07(effect.u32)
			}
			9 => {
				ensure!(effect.u32 == 0, BadEffectSnafu { effect });
				Effect::_09(effect.charid, effect.str)
			}
			10 => {
				ensure!(effect.charid == 0xFFFF, BadEffectSnafu { effect });
				ensure!(effect.u32 == 0, BadEffectSnafu { effect });
				Effect::_0A(effect.str)
			}
			_ => return BadEffectSnafu { effect }.fail(),
		})
	}
	Ok(table)
}

pub fn read_fc(f: &mut VReader) -> Result<String, gospel::read::Error> {
	let s = f.str()?;
	Ok(s)
}


#[derive(Debug, snafu::Snafu)]
pub enum BookError {
	#[snafu(display("invalid read (at {location})"), context(false))]
	Read {
		source: gospel::read::Error,
		#[snafu(implicit)]
		location: snafu::Location,
	},
	#[snafu(display("bad book kind: {kind:04X}"))]
	BadBook { kind: u16 },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Book {
	TitlePage(String, [u16; 10], String),
	Page(String),
}

pub fn read_book(f: &mut VReader) -> Result<Book, BookError> {
	let b = match f.u16()? {
		0 => {
			let s = f.str()?;
			Book::Page(s)
		}
		1 => {
			f.check_u16(0)?;
			let title = f.sstr(16)?;
			let data = [f.u16()?, f.u16()?, f.u16()?, f.u16()?, f.u16()?, f.u16()?, f.u16()?, f.u16()?, f.u16()?, f.u16()?];
			let text = f.str()?;
			Book::TitlePage(title, data, text)
		}
		kind => return BadBookSnafu { kind }.fail(),
	};
	Ok(b)
}

pub fn read_book99(f: &mut VReader) -> Result<u16, BookError> {
	let n = f.u16()?;
	f.check_u16(1)?;
	Ok(n)
}

#[derive(Debug, snafu::Snafu)]
pub enum BtlsetError {
	#[snafu(display("invalid read (at {location})"), context(false))]
	Read {
		source: gospel::read::Error,
		#[snafu(implicit)]
		location: snafu::Location,
	},
}

#[derive(Debug, Clone, PartialEq)]
pub struct Btlset {
	pub field: String,
	pub bounds: [f32; 6],
	pub unk1: (u32, u32, u16, u16),
	pub unk2: u32,
	pub unk3: String,
	pub variants: Vec<BtlVariant>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BtlVariant {
	pub num: u32,
	pub monsters: ArrayVec<(String, u8), 8>,
}

pub fn read_btlset(f: &mut VReader) -> Result<Btlset, BtlsetError> {
	let field = f.sstr(16)?;
	let bounds = [f.f32()?, f.f32()?, f.f32()?, f.f32()?, f.f32()?, f.f32()?];
	let unk1 = (f.u32()?, f.u32()?, f.u16()?, f.u16()?);
	f.check_u32(0)?;
	let unk2 = f.u32()?;
	let unk3 = f.sstr(32)?;
	let mut variants = Vec::new();
	loop {
		let num = f.u32()?;
		if num == 0xFFFFFFFF {
			break;
		}
		let names = [
			f.sstr(16)?,
			f.sstr(16)?,
			f.sstr(16)?,
			f.sstr(16)?,
			f.sstr(16)?,
			f.sstr(16)?,
			f.sstr(16)?,
			f.sstr(16)?,
		];
		let probs = f.array::<8>()?;
		f.check(&[0; 8])?;
		let mut monsters = names.into_iter().zip(probs).collect::<ArrayVec<_, 8>>();
		while monsters.last().is_some_and(|(m, p)| m.is_empty() && *p == 0) {
			monsters.pop();
		}
		variants.push(BtlVariant { num, monsters });
	}
	f.check(&[0; 0x18])?;
	Ok(Btlset { field, bounds, unk1, unk2, unk3, variants })
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Collision {
	pub a: u32,
	pub b: [f32; 5],
}

pub fn read_add_collision(f: &mut VReader) -> Result<Vec<Collision>, gospel::read::Error> {
	let mut out = Vec::new();
	for _ in 0..f.u8()? {
		out.push(Collision {
			a: f.u32()?,
			b: [f.f32()?, f.f32()?, f.f32()?, f.f32()?, f.f32()?],
		});
	}
	Ok(out)
}

#[derive(Debug, snafu::Snafu)]
pub enum StyleNameError {
	#[snafu(display("invalid read (at {location})"), context(false))]
	Read {
		source: gospel::read::Error,
		#[snafu(implicit)]
		location: snafu::Location,
	},
	#[snafu(display("style name mismatch: {a:?} != {b:?}"))]
	StyleMismatch { a: String, b: String },
}

pub fn read_style_name(f: &mut VReader) -> Result<String, StyleNameError> {
	let a = f.sstr(64)?;
	let b = f.sstr(64)?;
	ensure!(a == b, StyleMismatchSnafu { a, b });
	Ok(a)
}

// There's always an Action::dummy() in each table, but in one case it's not the last entry.
#[derive(Debug, Clone, PartialEq)]
pub struct Action {
	pub id: u16,
	pub u1: (u8, u8),
	pub target: (u8, u8, u16),
	pub u2: (f32, f32, f32),
	pub time: (u16, u16),
	pub effects: ArrayVec<(u16, u32, u32, u32), 5>,
	pub u3: (u16, u16),
	pub flags: String,
	pub ani: String,
	pub name: String,
}

impl Action {
	pub fn dummy() -> Self {
		Self {
			id: 0xFFFF,
			u1: (0, 0),
			target: (0, 0, 0),
			u2: (0.0, 0.0, 0.0),
			time: (0, 0),
			effects: ArrayVec::from([
				(1, 0, 0, 0),
				(2, 0, 0, 0),
				(3, 0, 0, 0),
				(4, 0, 0, 0),
				(5, 0, 0, 0),
			]),
			u3: (0, 0),
			flags: String::new(),
			ani: String::new(),
			name: String::new(),
		}
	}
}

pub fn read_action_table(f: &mut VReader) -> Result<Vec<Action>, gospel::read::Error> {
	let mut table = Vec::new();
	while !f.remaining().is_empty() {
		let id = f.u16()?;
		let u1 = (f.u8()?, f.u8()?);
		let target = (f.u8()?, f.u8()?, f.u16()?);
		let u2 = (f.f32()?, f.f32()?, f.f32()?);
		let time = (f.u16()?, f.u16()?);
		let u4 = (f.u16()?, f.u16()?, f.u16()?, f.u16()?, f.u16()?);
		f.check_u16(0)?;
		let mut effects = ArrayVec::<_, 5>::new();
		effects.push((u4.0, f.u32()?, f.u32()?, f.u32()?));
		effects.push((u4.1, f.u32()?, f.u32()?, f.u32()?));
		effects.push((u4.2, f.u32()?, f.u32()?, f.u32()?));
		effects.push((u4.3, f.u32()?, f.u32()?, f.u32()?));
		effects.push((u4.4, f.u32()?, f.u32()?, f.u32()?));
		while effects.last().is_some_and(|v| *v == (0, 0, 0, 0)) {
			effects.pop();
		}
		let u3 = (f.u16()?, f.u16()?);
		let flags = f.sstr(16)?;
		let ani = f.sstr(32)?;
		let name = f.sstr(64)?;
		table.push(Action { id, u1, target, u2, time, effects, u3, flags, ani, name });
	}
	Ok(table)
}

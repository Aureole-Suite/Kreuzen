use arrayvec::ArrayVec;
use gospel::read::Le as _;
use snafu::ensure;

use crate::{ReaderaExt as _, VReader};

pub mod effect {
	use super::*;

	#[derive(Debug, snafu::Snafu)]
	pub enum ReadError {
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

	pub fn read(f: &mut VReader) -> Result<Vec<Effect>, ReadError> {
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
}

pub mod fc_auto {
	use super::*;

	#[derive(Debug, snafu::Snafu)]
	pub enum ReadError {
		#[snafu(display("invalid read (at {location})"), context(false))]
		Read {
			source: gospel::read::Error,
			#[snafu(implicit)]
			location: snafu::Location,
		},
	}

	pub fn read(f: &mut VReader) -> Result<String, ReadError> {
		let s = f.str()?;
		Ok(s)
	}
}

pub mod book {
	use super::*;

	#[derive(Debug, snafu::Snafu)]
	pub enum ReadError {
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

	pub fn read(f: &mut VReader) -> Result<Book, ReadError> {
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
}

pub mod book99 {
	use super::*;

	#[derive(Debug, snafu::Snafu)]
	pub enum ReadError {
		#[snafu(display("invalid read (at {location})"), context(false))]
		Read {
			source: gospel::read::Error,
			#[snafu(implicit)]
			location: snafu::Location,
		},
	}

	pub fn read(f: &mut VReader) -> Result<u16, ReadError> {
		let n = f.u16()?;
		f.check_u16(1)?;
		Ok(n)
	}
}

pub mod btlset {
	use super::*;

	#[derive(Debug, snafu::Snafu)]
	pub enum ReadError {
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

	pub fn read(f: &mut VReader) -> Result<Btlset, ReadError> {
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
}

pub mod add_collision {
	use super::*;

	#[derive(Debug, snafu::Snafu)]
	pub enum ReadError {
		#[snafu(display("invalid read (at {location})"), context(false))]
		Read {
			source: gospel::read::Error,
			#[snafu(implicit)]
			location: snafu::Location,
		},
	}

	#[derive(Debug, Clone, Copy, PartialEq)]
	pub struct Collision {
		pub a: u32,
		pub b: [f32; 5],
	}

	pub fn read(f: &mut VReader) -> Result<Vec<Collision>, ReadError> {
		let mut out = Vec::new();
		for _ in 0..f.u8()? {
			out.push(Collision {
				a: f.u32()?,
				b: [f.f32()?, f.f32()?, f.f32()?, f.f32()?, f.f32()?],
			});
		}
		Ok(out)
	}
}

pub mod style_name {
	use super::*;

	#[derive(Debug, snafu::Snafu)]
	pub enum ReadError {
		#[snafu(display("invalid read (at {location})"), context(false))]
		Read {
			source: gospel::read::Error,
			#[snafu(implicit)]
			location: snafu::Location,
		},
		#[snafu(display("style name mismatch: {a:?} != {b:?}"))]
		StyleMismatch { a: String, b: String },
	}

	pub fn read(f: &mut VReader) -> Result<String, ReadError> {
		let a = f.sstr(64)?;
		let b = f.sstr(64)?;
		ensure!(a == b, StyleMismatchSnafu { a, b });
		Ok(a)
	}
}

pub mod action_table {
	use super::*;

	#[derive(Debug, snafu::Snafu)]
	pub enum ReadError {
		#[snafu(display("invalid read (at {location})"), context(false))]
		Read {
			source: gospel::read::Error,
			#[snafu(implicit)]
			location: snafu::Location,
		},
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

	pub fn read(f: &mut VReader) -> Result<Vec<Action>, ReadError> {
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
}

pub mod algo_table {
	use super::*;

	#[derive(Debug, snafu::Snafu)]
	pub enum ReadError {
		#[snafu(display("invalid read (at {location})"), context(false))]
		Read {
			source: gospel::read::Error,
			#[snafu(implicit)]
			location: snafu::Location,
		},
	}

#[derive(Debug, Clone, PartialEq)]
	pub struct Algo {
		pub id: u16,
		pub chance: u8,
		pub use_limit: u8,
		pub target_priority: u8,
		pub cond: (u8, u32, u32, u32),
		pub no_move: u32,
	}

	impl Algo {
		pub fn dummy() -> Self {
			Self {
				id: 0xFFFF,
				chance: 0,
				use_limit: 0,
				target_priority: 0,
				cond: (0, 1, 2, 3),
				no_move: 4,
			}
		}
	}

	pub fn read(f: &mut VReader) -> Result<Vec<Algo>, ReadError> {
		let mut table = Vec::new();
		while !f.remaining().is_empty() {
			let id = f.u16()?;
			let cond = f.u8()?;
			let chance = f.u8()?;
			let use_limit = f.u8()?;
			let target_priority = f.u8()?;
			f.check_u16(0)?;
			let cond = (cond, f.u32()?, f.u32()?, f.u32()?);
			let no_move = f.u32()?;
			f.check_u32(use_limit as u32)?;
			f.check_u32(0)?;
			table.push(Algo { id, chance, use_limit, target_priority, cond, no_move });
		}
		Ok(table)
	}
}

pub mod anime_clip_table {
	use super::*;

	#[derive(Debug, snafu::Snafu)]
	pub enum ReadError {
		#[snafu(display("invalid read (at {location})"), context(false))]
		Read {
			source: gospel::read::Error,
			#[snafu(implicit)]
			location: snafu::Location,
		},
		#[snafu(display("bad anime clip kind: {kind:X} for {a:?}, {b:?}"))]
		BadKind { kind: u32, a: String, b: String },
	}

	#[derive(Debug, Clone, PartialEq)]
	pub struct AnimeClip {
		pub kind: u32,
		pub a: String,
		pub b: String,
	}

	pub fn read(f: &mut VReader) -> Result<Vec<AnimeClip>, ReadError> {
		let mut table = Vec::new();
		loop {
			let kind = f.u32()?;
			if kind == 0 {
				break;
			}
			let a = f.sstr(32)?;
			let b = f.sstr(32)?;
			if kind.count_ones() != 1 {
				return BadKindSnafu { kind, a, b }.fail();
			}
			let kind = kind.trailing_zeros();
			table.push(AnimeClip { kind, a, b });
		}
		f.check_u16(0)?;
		Ok(table)
	}

}

pub mod break_table {
	use super::*;

	#[derive(Debug, snafu::Snafu)]
	pub enum ReadError {
		#[snafu(display("invalid read (at {location})"), context(false))]
		Read {
			source: gospel::read::Error,
			#[snafu(implicit)]
			location: snafu::Location,
		},
	}

	pub fn read(f: &mut VReader) -> Result<Vec<(u16, u16)>, ReadError> {
		let mut table = Vec::new();
		loop {
			let id = f.u16()?;
			if id == 0xFFFF {
				break;
			}
			let value = f.u16()?;
			table.push((id, value));
		}
		f.check_u16(1)?;
		Ok(table)
	}
}

pub use effect::Effect;
pub use book::Book;
pub use btlset::{Btlset, BtlVariant};
pub use action_table::Action;
pub use add_collision::Collision;
pub use algo_table::Algo;
pub use anime_clip_table::AnimeClip;

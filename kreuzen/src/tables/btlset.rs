use gospel::read::Le as _;
use gospel::write::{Le as _, Writer};

use crate::Game;
use crate::io::{CReader, OData, WriterExt as _};

#[derive(Debug, Clone, PartialEq)]
pub struct Btlset {
	pub field: String,
	pub bounds: [f32; 6],
	pub btl_id: u32,
	pub unk1: u32,
	pub bgm: (u16, u16),
	pub unk2: u32,
	pub script: String,
	pub variants: Vec<Variant>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Variant {
	pub num: u32,
	pub monsters: Vec<(String, u8)>, // up to 8 entries
}

pub(crate) fn read(f: &mut CReader) -> rootcause::Result<Btlset> {
	let field = f.sstr(16)?;
	let bounds = if f.game >= Game::Cs3 {
		[f.f32()?, f.f32()?, f.f32()?, f.f32()?, f.f32()?, f.f32()?]
	} else {
		[0.0; 6]
	};
	let btl_id = f.u32()?;
	let unk1 = if f.game >= Game::Cs3 {
		f.u32()?
	} else {
		0
	};
	let bgm = (f.u16()?, f.u16()?);
	f.check_u32(0)?;
	let unk2 = f.u32()?;
	let slen = match f.game {
		Game::Reverie => 32,
		Game::Cs3 | Game::Cs4 => 16,
		_ => 0,
	};
	let script = f.sstr(slen)?;

	let mut variants = Vec::new();
	loop {
		if f.remaining().is_empty() {
			tracing::warn!("unterminated btlset");
			break;
		}

		if f.check_u32(0xFFFFFFFE).is_ok() {
			// I don't know wtf this extra chunk is, it's only present in cs2 a0004.
			// Still, might as well keep it
			let num = f.u32()? + 1000000000;
			let names = [
				f.sstr(16)?, f.sstr(16)?, f.sstr(16)?, f.sstr(16)?,
			];
			let probs = [f.u16()? as u8, f.u16()? as u8, f.u16()? as u8, f.u16()? as u8];
			let mut monsters: Vec<_> = names.into_iter().zip(probs).collect();
			while monsters.last().is_some_and(|(m, p)| m.is_empty() && *p == 0) {
				monsters.pop();
			}
			variants.push(Variant { num, monsters });
			continue;
		}

		let num = f.u32()?;
		if num == 0xFFFFFFFF {
			f.check(&[0; 0x18])?;
			break;
		}
		let names = [
			f.sstr(16)?, f.sstr(16)?, f.sstr(16)?, f.sstr(16)?,
			f.sstr(16)?, f.sstr(16)?, f.sstr(16)?, f.sstr(16)?,
		];
		let probs = [
			f.u8()?, f.u8()?, f.u8()?, f.u8()?,
			f.u8()?, f.u8()?, f.u8()?, f.u8()?,
		];
		if f.check(b"mon029_0\0\0\0\0").is_ok() {
			tracing::warn!("spurious mon029_0 in btlset");
		} else {
			f.check(&[0; 8])?;
		}
		let mut monsters: Vec<_> = names.into_iter().zip(probs).collect();
		while monsters.last().is_some_and(|(m, p)| m.is_empty() && *p == 0) {
			monsters.pop();
		}
		variants.push(Variant { num, monsters });
	}

	Ok(Btlset {
		field,
		bounds,
		btl_id,
		unk1,
		bgm,
		unk2,
		script,
		variants,
	})
}

pub(crate) fn write(d: &OData, b: &Btlset) -> rootcause::Result<Writer> {
	todo!()
}

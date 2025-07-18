use arrayvec::ArrayVec;

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

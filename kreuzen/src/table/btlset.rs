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

#[derive(Debug, snafu::Snafu)]
pub enum WriteError {
	#[snafu(transparent, context(false))]
	Value { source: crate::ValueError },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Btlset {
	pub field: String,
	pub bounds: [f32; 6],
	pub btl_id: u32,
	pub unk1: u32,
	pub bgm: (u16, u16),
	pub unk2: u32,
	pub script: String,
	pub variants: Vec<BtlVariant>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BtlVariant {
	pub num: u32,
	pub monsters: ArrayVec<(String, u8), 8>,
}

pub(crate) fn read(f: &mut VReader) -> Result<Btlset, ReadError> {
	let field = f.sstr(16)?;
	let bounds = [f.f32()?, f.f32()?, f.f32()?, f.f32()?, f.f32()?, f.f32()?];
	let btl_id = f.u32()?;
	let unk1 = f.u32()?;
	let bgm = (f.u16()?, f.u16()?);
	f.check_u32(0)?;
	let unk2 = f.u32()?;
	let script = f.sstr(32)?;
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
	Ok(Btlset { field, bounds, btl_id, unk1, bgm, unk2, script, variants })
}

pub(crate) fn write(f: &mut VWriter, btlset: &Btlset) -> Result<(), WriteError> {
	f.sstr(16, &btlset.field)?;
	for &bound in &btlset.bounds {
		f.f32(bound);
	}
	f.u32(btlset.btl_id);
	f.u32(btlset.unk1);
	f.u16(btlset.bgm.0);
	f.u16(btlset.bgm.1);
	f.u32(0);
	f.u32(btlset.unk2);
	f.sstr(32, &btlset.script)?;
	for variant in &btlset.variants {
		f.u32(variant.num);
		for (name, _) in &variant.monsters {
			f.sstr(16, name)?;
		}
		for _ in variant.monsters.len()..8 {
			f.sstr(16, "")?;
		}
		for (_, prob) in &variant.monsters {
			f.u8(*prob);
		}
		for _ in variant.monsters.len()..8 {
			f.u8(0);
		}
		f.slice(&[0; 8]);
	}
	f.u32(0xFFFFFFFF); // end of variants
	f.slice(&[0; 0x18]);
	Ok(())
}

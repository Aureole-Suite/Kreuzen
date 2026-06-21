use gospel::read::Le as _;
use gospel::write::{Le as _, Writer};

use crate::{Enc, Game};
use crate::io::{OData, CReader};

#[derive(Debug, Clone, PartialEq)]
pub struct Cs1Action {
	pub id: u16,
	pub bytes: [u8; 10],
	pub words: [u32; 7],
	pub flags: String,
	pub ani: String,
	pub name: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Cs3Action {
	pub id: u16,
	pub u1: (u8, u8),
	pub target: (u8, u8, u16),
	pub u2: (f32, f32, f32),
	pub time: (u16, u16),
	pub effects: Vec<(u16, u32, u32, u32)>,
	pub u3: (u16, u16),
	pub flags: String,
	pub ani: String,
	pub name: String,
}

impl Cs3Action {
	// There's always an Action::dummy() in each table, but in one case it's not the last entry.
	pub fn dummy() -> Self {
		Self {
			id: 0xFFFF,
			u1: (0, 0),
			target: (0, 0, 0),
			u2: (0.0, 0.0, 0.0),
			time: (0, 0),
			effects: vec![
				(1, 0, 0, 0),
				(2, 0, 0, 0),
				(3, 0, 0, 0),
				(4, 0, 0, 0),
				(5, 0, 0, 0),
			],
			u3: (0, 0),
			flags: String::new(),
			ani: String::new(),
			name: String::new(),
		}
	}
}

pub(crate) fn read_cs1(f: &mut CReader) -> rootcause::Result<Vec<Cs1Action>> {
	let n = f.u8()? as usize;
	let namelen = match f.enc {
		Enc::Sjis => 32,
		Enc::Utf8 => 48,
	};

	let mut out = Vec::with_capacity(n);
	while !f.remaining().is_empty() {
		let id = f.u16()?;
		let mut bytes = [0u8; 10];
		for b in &mut bytes {
			*b = f.u8()?;
		}
		let mut words = [0u32; 7];
		for w in &mut words {
			*w = if f.game == Game::Cs1 { f.u16()? as u32 } else { f.u32()? };
		}
		let flags = f.sstr(16)?;
		let ani = f.sstr(32)?;
		let name = f.sstr(namelen)?;
		out.push(Cs1Action { id, bytes, words, flags, ani, name });
	}

	if out.len() != n {
		tracing::warn!("wrong ActionTable length: {} != {}", out.len(), n);
	}

	Ok(out)
}

pub(crate) fn read_cs3(f: &mut CReader) -> rootcause::Result<Vec<Cs3Action>> {
	let mut table = Vec::new();
	let mut has_sep = false;
	while !f.remaining().is_empty() {
		if has_sep {
			tracing::warn!("data after ActionTable terminator");
		}
		let id = f.u16()?;
		if id == 0xFFFF && f.game != Game::Reverie {
			has_sep = true;
			f.check(&[0; 193])?;
			continue;
		}
		let u1 = (f.u8()?, f.u8()?);
		let target = (f.u8()?, f.u8()?, f.u16()?);
		let u2 = (f.f32()?, f.f32()?, f.f32()?);
		let time = (f.u16()?, f.u16()?);
		let u4 = (f.u16()?, f.u16()?, f.u16()?, f.u16()?, f.u16()?);
		f.check_u16(0)?;
		let mut effects = vec![
			(u4.0, f.u32()?, f.u32()?, f.u32()?),
			(u4.1, f.u32()?, f.u32()?, f.u32()?),
			(u4.2, f.u32()?, f.u32()?, f.u32()?),
			(u4.3, f.u32()?, f.u32()?, f.u32()?),
			(u4.4, f.u32()?, f.u32()?, f.u32()?),
		];
		while effects.last().is_some_and(|v| *v == (0, 0, 0, 0)) {
			effects.pop();
		}
		let u3 = (f.u16()?, f.u16()?);
		let flags = f.sstr(16)?;
		let ani = f.sstr(32)?;
		let name = f.sstr(64)?;
		let act = Cs3Action { id, u1, target, u2, time, effects, u3, flags, ani, name };
		if id == 0xFFFF && f.game == Game::Reverie {
			if act != Cs3Action::dummy() {
				tracing::error!("malformed ActionTable terminator");
			}
			has_sep = true;
			continue;
		}
		table.push(act);
	}
	Ok(table)
}

pub(crate) fn write_cs1(d: &OData, table: &[Cs1Action]) -> rootcause::Result<Writer> {
	let mut f = Writer::new();
	f.u8(table.len() as u8);
	for action in table {
		todo!()
	}
	Ok(f)
}

pub(crate) fn write_cs3(d: &OData, table: &[Cs3Action]) -> rootcause::Result<Writer> {
	let mut f = Writer::new();
	for action in table {
		todo!()
	}
	Ok(f)
}

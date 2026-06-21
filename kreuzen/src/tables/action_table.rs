use gospel::read::Le as _;
use gospel::write::{Le as _, Writer};

use crate::{Enc, Game};
use crate::io::{OData, CReader};

#[derive(Debug, Clone, PartialEq)]
pub struct Action {
	pub id: u16,
	pub kind: (u8, u8),
	pub target: (u8, u8, u16),
	pub u2: (f32, f32, f32), // almost always (45.0, 100.0, -100.0); CS3+
	pub cast_time: u16,
	pub recovery_time: u16,
	pub effects: Vec<(u16, u32, u32, u32)>,
	pub cp_cost: u32,
	pub flags: String,
	pub ani: String,
	pub name: String,
}

impl Action {
	fn dummy() -> Self {
		Self {
			id: 0xFFFF,
			kind: (0, 0),
			target: (0, 0, 0),
			u2: (0.0, 0.0, 0.0),
			cast_time: 0,
			recovery_time: 0,
			effects: vec![
				(1, 0, 0, 0),
				(2, 0, 0, 0),
				(3, 0, 0, 0),
				(4, 0, 0, 0),
				(5, 0, 0, 0),
			],
			cp_cost: 0,
			flags: String::new(),
			ani: String::new(),
			name: String::new(),
		}
	}
}

pub(crate) fn read(f: &mut CReader) -> rootcause::Result<Vec<Action>> {
	match f.game {
		Game::Cs1 | Game::Cs2 => read_cs1(f),
		Game::Tx => rootcause::bail!("ActionTable in Tx"),
		Game::Cs3 | Game::Cs4 | Game::Reverie => read_cs3(f),
	}
}

pub(crate) fn write(d: &OData, table: &[Action]) -> rootcause::Result<Writer> {
	match d.game {
		Game::Cs1 | Game::Cs2 => write_cs1(d, table),
		Game::Tx => rootcause::bail!("ActionTable in Tx"),
		Game::Cs3 | Game::Cs4 | Game::Reverie => write_cs3(d, table),
	}
}

fn read_cs1(f: &mut CReader) -> rootcause::Result<Vec<Action>> {
	let n = f.u8()? as usize;
	let namelen = match f.enc {
		Enc::Sjis => 32,
		Enc::Utf8 => 48,
	};

	let mut out = Vec::with_capacity(n);
	while !f.remaining().is_empty() {
		let id = f.u16()?;
		let kind = (f.u8()?, f.u8()?);
		let target = (f.u8()?, f.u8()?, f.u8()? as u16);
		let cast_time = f.u8()? as u16;
		let recovery_time = f.u16()?;

		let u4 = (f.u8()? as u16, f.u8()? as u16);
		let mut w = || -> Result<u32, gospel::read::Error> {
			Ok(if f.game == Game::Cs1 { f.u16()? as u32 } else { f.u32()? })
		};
		let mut effects = vec![
			(u4.0, w()?, w()?, w()?),
			(u4.1, w()?, w()?, w()?),
		];
		while effects.last().is_some_and(|v| *v == (0, 0, 0, 0)) {
			effects.pop();
		}

		let cp_cost = w()?;
		let flags = f.sstr(16)?;
		let ani = f.sstr(32)?;
		let name = f.sstr(namelen)?;
		out.push(Action {
			id, kind, target,
			u2: (0.0, 0.0, 0.0),
			cast_time, recovery_time,
			effects,
			cp_cost,
			flags, ani, name,
		});
	}

	if out.len() != n {
		tracing::warn!("wrong ActionTable length: {} != {}", out.len(), n);
	}

	Ok(out)
}

fn read_cs3(f: &mut CReader) -> rootcause::Result<Vec<Action>> {
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
		let kind = (f.u8()?, f.u8()?);
		let target = (f.u8()?, f.u8()?, f.u16()?);
		let u2 = (f.f32()?, f.f32()?, f.f32()?);
		let cast_time = f.u16()?;
		let recovery_time = f.u16()?;

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

		let cp_cost = f.u32()?;
		let flags = f.sstr(16)?;
		let ani = f.sstr(32)?;
		let name = f.sstr(64)?;
		let act = Action { id, kind, target, u2, cast_time, recovery_time, effects, cp_cost, flags, ani, name };
		if id == 0xFFFF && f.game == Game::Reverie {
			if act != Action::dummy() {
				tracing::error!("malformed ActionTable terminator");
			}
			has_sep = true;
			continue;
		}
		table.push(act);
	}
	Ok(table)
}

fn write_cs1(d: &OData, table: &[Action]) -> rootcause::Result<Writer> {
	let mut f = Writer::new();
	f.u8(table.len() as u8);
	for _ in table {
		todo!()
	}
	Ok(f)
}

fn write_cs3(d: &OData, table: &[Action]) -> rootcause::Result<Writer> {
	let mut f = Writer::new();
	for _ in table {
		todo!()
	}
	Ok(f)
}

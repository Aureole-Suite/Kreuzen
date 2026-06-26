use gospel::read::Le as _;
use gospel::write::{Le as _, Writer};

use crate::Game;
use crate::io::{CReader, OData};

#[derive(Debug, Clone, PartialEq)]
pub struct Reaction {
	id: u16,
	a: u16,
	b: u16,
	c: u16,
	floats: Vec<f32>, // always length 12 except in cs1 where it's empty
	d: u32,
}

pub(crate) fn read(f: &mut CReader) -> rootcause::Result<Vec<Reaction>> {
	match f.game {
		Game::Cs1 => read_cs1(f, 0),
		Game::Cs2 => read_cs1(f, 12),
		Game::Tx => rootcause::bail!("ReactionTable in Tx"),
		Game::Cs3 | Game::Cs4 | Game::Reverie => read_cs3(f),
	}
}

pub(crate) fn write(d: &OData, table: &[Reaction]) -> rootcause::Result<Writer> {
	match d.game {
		Game::Cs1 => write_cs1(table, 0),
		Game::Cs2 => write_cs1(table, 12),
		Game::Tx => rootcause::bail!("ReactionTable in Tx"),
		Game::Cs3 | Game::Cs4 | Game::Reverie => write_cs3(d, table),
	}
}

fn read_cs1(f: &mut CReader, count: usize) -> rootcause::Result<Vec<Reaction>> {
	let n = f.u16()? as usize;
	let mut out = Vec::new();
	while !f.remaining().is_empty() {
		out.push(Reaction {
			id: f.u16()?,
			a: f.u16()?,
			b: f.u16()?,
			c: f.u16()?,
			floats: {
				let mut floats = Vec::new();
				for _ in 0..count {
					floats.push(f.f32()?);
				}
				floats
			},
			d: f.u32()?,
		});
	}
	if out.len() != n {
		tracing::warn!("wrong ReactionTable count: {} != {}", out.len(), n);
	}
	Ok(out)
}

fn read_cs3(f: &mut CReader) -> rootcause::Result<Vec<Reaction>> {
	let sentinel = if f.game == Game::Reverie { 0xFFFF } else { 0 };
	let mut table = Vec::new();
	let mut has_sep = false;
	while !f.remaining().is_empty() {
		if has_sep {
			tracing::warn!("data after ReactionTable terminator");
		}
		let id = f.u16()?;
		if id == sentinel {
			f.check(&[0; 58])?;
			has_sep = true;
			continue;
		}
		table.push(Reaction {
			id,
			a: f.u16()?,
			b: f.u16()?,
			c: f.u16()?,
			floats: {
				let mut floats = Vec::new();
				for _ in 0..12 {
					floats.push(f.f32()?);
				}
				floats
			},
			d: f.u32()?,
		});
	}
	if !has_sep {
		tracing::warn!("missing ReactionTable terminator");
	}
	Ok(table)
}

fn write_cs1(table: &[Reaction], count: usize) -> rootcause::Result<Writer> {
	let mut f = Writer::new();
	let n = u16::try_from(table.len())
		.map_err(|_| rootcause::report!("ReactionTable too large: {}", table.len()))?;
	f.u16(n);
	for r in table {
		crate::ensure!(r.floats.len() == count);
		f.u16(r.id);
		f.u16(r.a);
		f.u16(r.b);
		f.u16(r.c);
		for &v in &r.floats {
			f.f32(v);
		}
		f.u32(r.d);
	}
	Ok(f)
}

fn write_cs3(d: &OData, table: &[Reaction]) -> rootcause::Result<Writer> {
	let mut f = Writer::new();
	let sentinel = if d.game == Game::Reverie { 0xFFFF } else { 0 };
	for r in table {
		crate::ensure!(r.floats.len() == 12);
		f.u16(r.id);
		f.u16(r.a);
		f.u16(r.b);
		f.u16(r.c);
		for &v in &r.floats {
			f.f32(v);
		}
		f.u32(r.d);
	}
	f.u16(sentinel);
	f.slice(&[0; 58]);
	Ok(f)
}

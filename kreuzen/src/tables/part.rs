use gospel::read::Le as _;
use gospel::write::{Le as _, Writer};

use crate::Game;
use crate::io::{CReader, OData, WriterExt as _};

#[derive(Debug, Clone, PartialEq)]
pub struct Part {
	pub id: u32,
	pub a: String,
	pub b: String,
}

pub(crate) fn read(f: &mut CReader) -> rootcause::Result<Vec<Part>> {
	match f.game {
		Game::Cs1 | Game::Cs2 => read_cs1(f),
		Game::Tx => rootcause::bail!("PartTable in Tx"),
		Game::Cs3 | Game::Cs4 | Game::Reverie => read_cs3(f),
	}
}

pub(crate) fn write(d: &OData, table: &[Part]) -> rootcause::Result<Writer> {
	match d.game {
		Game::Cs1 | Game::Cs2 => write_cs1(d, table),
		Game::Tx => rootcause::bail!("PartTable in Tx"),
		Game::Cs3 | Game::Cs4 | Game::Reverie => write_cs3(d, table),
	}
}

fn read_part(f: &mut CReader) -> rootcause::Result<Part> {
	Ok(Part { id: f.u32()?, a: f.sstr(32)?, b: f.sstr(32)? })
}

fn write_part(f: &mut Writer, d: &OData, p: &Part) -> rootcause::Result<()> {
	f.u32(p.id);
	f.sstr(32, d.enc, &p.a)?;
	f.sstr(32, d.enc, &p.b)?;
	Ok(())
}

fn read_cs1(f: &mut CReader) -> rootcause::Result<Vec<Part>> {
	let n = f.u8()? as usize;
	let mut out = Vec::new();
	while !f.remaining().is_empty() {
		out.push(read_part(f)?);
	}
	if out.len() != n {
		tracing::warn!("wrong PartTable count: {} != {}", out.len(), n);
	}
	Ok(out)
}

fn read_cs3(f: &mut CReader) -> rootcause::Result<Vec<Part>> {
	let sentinel = if f.game == Game::Reverie { 0xFFFF } else { 0xFF };
	let mut table = Vec::new();
	let mut has_sep = false;
	while !f.remaining().is_empty() {
		if has_sep {
			tracing::warn!("data after PartTable terminator");
		}
		let id = f.u32()?;
		if id == sentinel {
			f.check(&[0; 64])?;
			has_sep = true;
			continue;
		}
		let a = f.sstr(32)?;
		let b = f.sstr(32)?;
		table.push(Part { id, a, b });
	}
	if !has_sep {
		tracing::warn!("missing PartTable terminator");
	}
	Ok(table)
}

fn write_cs1(d: &OData, table: &[Part]) -> rootcause::Result<Writer> {
	let mut f = Writer::new();
	let n = u8::try_from(table.len()).map_err(|_| rootcause::report!("PartTable too large: {}", table.len()))?;
	f.u8(n);
	for p in table {
		write_part(&mut f, d, p)?;
	}
	Ok(f)
}

fn write_cs3(d: &OData, table: &[Part]) -> rootcause::Result<Writer> {
	let mut f = Writer::new();
	for p in table {
		write_part(&mut f, d, p)?;
	}
	let sentinel = if d.game == Game::Reverie { 0xFFFF } else { 0xFF };
	f.u32(sentinel);
	f.slice(&[0; 64]);
	Ok(f)
}

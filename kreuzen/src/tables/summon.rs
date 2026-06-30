use gospel::read::Le as _;
use gospel::write::{Le as _, Writer};

use crate::Game;
use crate::io::{CReader, OData, WriterExt as _};

#[derive(Debug, Clone, PartialEq)]
pub struct Summon {
	pub kind: u16,
	pub a: u8,
	pub b: u8,
	pub name: String,
}

pub(crate) fn read(f: &mut CReader) -> rootcause::Result<Vec<Summon>> {
	match f.game {
		Game::Cs1 | Game::Cs2 => read_cs1(f),
		Game::Tx => rootcause::bail!("SummonTable in Tx"),
		Game::Cs3 | Game::Cs4 | Game::Reverie => read_cs3(f),
	}
}

pub(crate) fn write(d: &OData, table: &[Summon]) -> rootcause::Result<Writer> {
	match d.game {
		Game::Cs1 | Game::Cs2 => write_cs1(d, table),
		Game::Tx => rootcause::bail!("SummonTable in Tx"),
		Game::Cs3 | Game::Cs4 | Game::Reverie => write_cs3(d, table),
	}
}

fn read_summon(f: &mut CReader) -> rootcause::Result<Summon> {
	Ok(Summon {
		kind: f.u16()?,
		a: f.u8()?,
		b: f.u8()?,
		name: f.sstr(32)?,
	})
}

fn write_summon(f: &mut Writer, d: &OData, s: &Summon) -> rootcause::Result<()> {
	f.u16(s.kind);
	f.u8(s.a);
	f.u8(s.b);
	f.sstr(32, d.enc, &s.name)?;
	Ok(())
}

fn read_cs1(f: &mut CReader) -> rootcause::Result<Vec<Summon>> {
	let n = f.u8()? as usize;
	let mut out = Vec::new();
	while !f.remaining().is_empty() {
		out.push(read_summon(f)?);
	}
	if out.len() != n {
		tracing::warn!("wrong SummonTable count: {} != {}", out.len(), n);
	}
	Ok(out)
}

fn read_cs3(f: &mut CReader) -> rootcause::Result<Vec<Summon>> {
	let mut table = Vec::new();
	let mut has_sep = false;
	while !f.remaining().is_empty() {
		if has_sep {
			tracing::warn!("data after SummonTable terminator");
		}
		let kind = f.u16()?;
		if kind == 0xFFFF {
			f.check(&[0; 34])?;
			has_sep = true;
			continue;
		}
		let a = f.u8()?;
		let b = f.u8()?;
		let name = f.sstr(32)?;
		table.push(Summon { kind, a, b, name });
	}
	if !has_sep {
		tracing::warn!("missing SummonTable terminator");
	}
	Ok(table)
}

fn write_cs1(d: &OData, table: &[Summon]) -> rootcause::Result<Writer> {
	let mut f = Writer::new();
	let n = u8::try_from(table.len()).map_err(|_| rootcause::report!("SummonTable too large: {}", table.len()))?;
	f.u8(n);
	for s in table {
		write_summon(&mut f, d, s)?;
	}
	Ok(f)
}

fn write_cs3(d: &OData, table: &[Summon]) -> rootcause::Result<Writer> {
	let mut f = Writer::new();
	for s in table {
		write_summon(&mut f, d, s)?;
	}
	f.u16(0xFFFF);
	f.slice(&[0; 34]);
	Ok(f)
}

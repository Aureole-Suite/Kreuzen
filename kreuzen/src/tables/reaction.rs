use gospel::read::Le as _;
use gospel::write::{Le as _, Writer};

use crate::Game;
use crate::io::{CReader, OData};
use crate::types::Magic;

#[derive(Debug, Clone, PartialEq)]
pub struct PartReaction {
	pub rating: u16,
	pub unbalance: f32,
	pub hit: f32,
	pub miss: f32,
	pub counter: f32,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ReactionKind {
	Parts([PartReaction; 3]),
	Alias(Magic),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Reaction {
	pub id: Magic,
	pub kind: ReactionKind,
}

pub(crate) fn read(f: &mut CReader) -> rootcause::Result<Vec<Reaction>> {
	match f.game {
		Game::Cs1 => read_cs1(f, false),
		Game::Cs2 => read_cs1(f, true),
		Game::Tx => rootcause::bail!("ReactionTable in Tx"),
		Game::Cs3 | Game::Cs4 | Game::Reverie => read_cs3(f),
	}
}

pub(crate) fn write(d: &OData, table: &[Reaction]) -> rootcause::Result<Writer> {
	match d.game {
		Game::Cs1 => write_cs1(table, false),
		Game::Cs2 => write_cs1(table, true),
		Game::Tx => rootcause::bail!("ReactionTable in Tx"),
		Game::Cs3 | Game::Cs4 | Game::Reverie => write_cs3(d, table),
	}
}

fn read_entry(f: &mut CReader, has_floats: bool) -> rootcause::Result<Reaction> {
	let floats = |f: &mut CReader| -> rootcause::Result<_> {
		if has_floats {
			Ok([f.f32()?, f.f32()?, f.f32()?, f.f32()?])
		} else {
			Ok([-1.0; 4])
		}
	};
	fn part(rating: u16, [unbalance, hit, miss, counter]: [f32; 4]) -> PartReaction {
		PartReaction { rating, unbalance, hit, miss, counter }
	}
	let id = Magic(f.u16()?);
	let a = f.u16()?;
	let b = f.u16()?;
	let c = f.u16()?;
	let pa = floats(f)?;
	let pb = floats(f)?;
	let pc = floats(f)?;
	let d = f.u32()?;
	let kind = if d == 1 {
		ReactionKind::Alias(Magic(a))
	} else {
		ReactionKind::Parts([part(a, pa), part(b, pb), part(c, pc)])
	};
	Ok(Reaction { id, kind })
}

fn read_cs1(f: &mut CReader, has_floats: bool) -> rootcause::Result<Vec<Reaction>> {
	let n = f.u16()? as usize;
	let mut out = Vec::new();
	while !f.remaining().is_empty() {
		out.push(read_entry(f, has_floats)?);
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
		if f.clone().u16()? == sentinel {
			f.u16()?;
			f.check(&[0; 58])?;
			has_sep = true;
			continue;
		}
		table.push(read_entry(f, true)?);
	}
	if !has_sep {
		tracing::warn!("missing ReactionTable terminator");
	}
	Ok(table)
}

fn write_entry(f: &mut Writer, r: &Reaction, has_floats: bool) {
	f.u16(r.id.0);
	match &r.kind {
		ReactionKind::Parts(parts) => {
			for p in parts {
				f.u16(p.rating);
			}
			if has_floats {
				for p in parts {
					for &v in &[p.unbalance, p.hit, p.miss, p.counter] {
						f.f32(v);
					}
				}
			}
			f.u32(0);
		}
		ReactionKind::Alias(m) => {
			f.u16(m.0);
			f.u16(0);
			f.u16(0);
			if has_floats {
				for _ in 0..12 {
					f.f32(0.0);
				}
			}
			f.u32(1);
		}
	}
}

fn write_cs1(table: &[Reaction], has_floats: bool) -> rootcause::Result<Writer> {
	let mut f = Writer::new();
	let n = u16::try_from(table.len()).map_err(|_| rootcause::report!("ReactionTable too large: {}", table.len()))?;
	f.u16(n);
	for r in table {
		write_entry(&mut f, r, has_floats);
	}
	Ok(f)
}

fn write_cs3(d: &OData, table: &[Reaction]) -> rootcause::Result<Writer> {
	let mut f = Writer::new();
	let sentinel = if d.game == Game::Reverie { 0xFFFF } else { 0 };
	for r in table {
		write_entry(&mut f, r, true);
	}
	f.u16(sentinel);
	f.slice(&[0; 58]);
	Ok(f)
}

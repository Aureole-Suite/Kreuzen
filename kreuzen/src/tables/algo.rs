use gospel::read::Le as _;
use gospel::write::{Le as _, Writer};

use crate::Game;
use crate::io::{CReader, OData};

#[derive(Debug, Clone, PartialEq)]
pub struct Algo {
	pub id: u16,
	pub chance: u8,
	pub use_limit: u8,
	pub target_priority: u8,
	pub cond: (u8, u32, u32, u32, u32),
}

impl Algo {
	fn dummy() -> Self {
		Self {
			id: 0xFFFF,
			chance: 0,
			use_limit: 0,
			target_priority: 0,
			cond: (0, 1, 2, 3, 4),
		}
	}
}

pub(crate) fn read(f: &mut CReader) -> rootcause::Result<Vec<Algo>> {
	match f.game {
		Game::Cs1 | Game::Cs2 | Game::Tx => read_cs1(f),
		Game::Cs3 | Game::Cs4 | Game::Reverie => read_cs3(f),
	}
}

pub(crate) fn write(d: &OData, table: &[Algo]) -> rootcause::Result<Writer> {
	match d.game {
		Game::Cs1 | Game::Cs2 | Game::Tx => write_cs1(d, table),
		Game::Cs3 | Game::Cs4 | Game::Reverie => write_cs3(d, table),
	}
}

fn read_algo(f: &mut CReader) -> rootcause::Result<Algo> {
	let id = f.u16()?;
	let cond0 = f.u8()?;
	let chance = f.u8()?;
	let use_limit = f.u8()?;
	let target_priority = f.u8()?;
	f.check_u16(0)?;
	let cond = (cond0, f.u32()?, f.u32()?, f.u32()?, f.u32()?);
	if f.check_u64(use_limit as u64).is_err() {
		tracing::warn!("truncated algo entry");
	}
	Ok(Algo { id, chance, use_limit, target_priority, cond })
}

fn write_algo(f: &mut Writer, algo: &Algo) {
	f.u16(algo.id);
	f.u8(algo.cond.0);
	f.u8(algo.chance);
	f.u8(algo.use_limit);
	f.u8(algo.target_priority);
	f.u16(0);
	f.u32(algo.cond.1);
	f.u32(algo.cond.2);
	f.u32(algo.cond.3);
	f.u32(algo.cond.4);
	f.u64(algo.use_limit as u64);
}

fn read_cs1(f: &mut CReader) -> rootcause::Result<Vec<Algo>> {
	let n = f.u8()? as usize;
	let mut out = Vec::new();
	while !f.remaining().is_empty() {
		let algo = read_algo(f)?;
		// if f.game == Game::Cs1 && algo.id == 0xFFFF {
		// 	f.check(&[0; 8])?;
		// }
		out.push(algo);
	}
	if out.len() != n {
		tracing::warn!("wrong AlgoTable count: {} != {}", out.len(), n);
	}
	Ok(out)
}

fn read_cs3(f: &mut CReader) -> rootcause::Result<Vec<Algo>> {
	let terminator_id = if f.game == Game::Reverie { 0xFFFF } else { 0 };
	let mut table = Vec::new();
	let mut has_sep = false;
	while !f.remaining().is_empty() {
		if has_sep {
			tracing::warn!("data after AlgoTable terminator");
		}
		let algo = read_algo(f)?;
		if algo.id == terminator_id {
			has_sep = true;
			continue;
		}
		table.push(algo);
	}
	if !has_sep {
		tracing::warn!("missing AlgoTable terminator");
	}
	Ok(table)
}

fn write_cs1(d: &OData, table: &[Algo]) -> rootcause::Result<Writer> {
	let mut f = Writer::new();
	let n = u8::try_from(table.len()).map_err(|_| rootcause::report!("AlgoTable too large: {}", table.len()))?;
	f.u8(n);
	for algo in table {
		write_algo(&mut f, algo);
		if d.game == Game::Cs1 && algo.id == 0xFFFF {
			f.slice(&[0; 8]);
		}
	}
	Ok(f)
}

fn write_cs3(d: &OData, table: &[Algo]) -> rootcause::Result<Writer> {
	let mut f = Writer::new();
	for algo in table {
		write_algo(&mut f, algo);
	}
	if d.game == Game::Reverie {
		write_algo(&mut f, &Algo::dummy());
	} else {
		f.slice(&[0; 32]);
	}
	Ok(f)
}

use gospel::read::Le as _;
use gospel::write::{Le as _, Writer};

use crate::Game;
use crate::io::{CReader, OData};

#[derive(Debug, Clone, PartialEq)]
pub struct Condition {
	pub id: u16,
	pub entries: Vec<(u16, u32, u32, u32)>,
}

pub(crate) fn read(f: &mut CReader) -> rootcause::Result<Vec<Condition>> {
	match f.game {
		Game::Cs2 => read_cs2(f),
		g => rootcause::bail!("ConditionTable in {g:?}"),
	}
}

pub(crate) fn write(d: &OData, table: &[Condition]) -> rootcause::Result<Writer> {
	match d.game {
		Game::Cs2 => write_cs2(table),
		g => rootcause::bail!("ConditionTable in {g:?}"),
	}
}

fn read_cs2(f: &mut CReader) -> rootcause::Result<Vec<Condition>> {
	let n = f.u8()? as usize;
	let mut out = Vec::with_capacity(n);
	for _ in 0..n {
		let id = f.u16()?;
		let vs = [f.u16()?, f.u16()?, f.u16()?, f.u16()?, f.u16()?];
		let mut entries = Vec::with_capacity(vs.len());
		for v in vs {
			entries.push((v, f.u32()?, f.u32()?, f.u32()?));
		}
		while entries.last().is_some_and(|&e| e == (0, 0, 0, 0)) {
			entries.pop();
		}
		out.push(Condition { id, entries });
	}
	Ok(out)
}

fn write_cs2(table: &[Condition]) -> rootcause::Result<Writer> {
	let mut f = Writer::new();
	let n = u8::try_from(table.len())
		.map_err(|_| rootcause::report!("ConditionTable too large: {}", table.len()))?;
	f.u8(n);
	for c in table {
		crate::ensure!(c.entries.len() <= 5, "ConditionTable entry has more than 5 entries: {c:?}");
		f.u16(c.id);
		for i in 0..5 {
			f.u16(c.entries.get(i).map_or(0, |e| e.0));
		}
		for i in 0..5 {
			let e = c.entries.get(i).unwrap_or(&(0, 0, 0, 0));
			f.u32(e.1);
			f.u32(e.2);
			f.u32(e.3);
		}
	}
	Ok(f)
}

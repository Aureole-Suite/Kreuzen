use gospel::read::Le as _;
use gospel::write::{Le as _, Writer};

use crate::Game;
use crate::io::{CReader, OData};
use crate::types::Magic;

#[derive(Debug, Clone, PartialEq)]
pub struct Break {
	pub id: Magic,
	pub value: u16,
}

pub(crate) fn read(f: &mut CReader) -> rootcause::Result<Vec<Break>> {
	match f.game {
		Game::Cs1 | Game::Cs2 => {
			let n = f.u8()? as usize;
			let mut entries = Vec::with_capacity(n);
			for _ in 0..n {
				entries.push(Break { id: Magic(f.u16()?), value: f.u16()? });
			}
			Ok(entries)
		}
		Game::Tx => rootcause::bail!("BreakTable in tx"),
		Game::Cs3 | Game::Cs4 | Game::Reverie => {
			let (sentinel, tail) = if f.game == Game::Reverie { (0xFFFF, 1) } else { (0, 0) };
			let mut entries = Vec::new();
			loop {
				let id = Magic(f.u16()?);
				if id.0 == sentinel {
					f.check_u16(tail)?;
					break;
				}
				entries.push(Break { id, value: f.u16()? });
			}
			Ok(entries)
		}
	}
}

pub(crate) fn write(d: &OData, table: &[Break]) -> rootcause::Result<Writer> {
	let mut f = Writer::new();
	match d.game {
		Game::Cs1 | Game::Cs2 => {
			let n = u8::try_from(table.len()).map_err(|_| rootcause::report!("BreakTable too large: {}", table.len()))?;
			f.u8(n);
			for b in table {
				f.u16(b.id.0);
				f.u16(b.value);
			}
		}
		Game::Tx => rootcause::bail!("BreakTable in tx"),
		Game::Cs3 | Game::Cs4 | Game::Reverie => {
			let (sentinel, tail) = if d.game == Game::Reverie { (0xFFFF, 1) } else { (0, 0) };
			for b in table {
				f.u16(b.id.0);
				f.u16(b.value);
			}
			f.u16(sentinel);
			f.u16(tail);
		}
	}
	Ok(f)
}

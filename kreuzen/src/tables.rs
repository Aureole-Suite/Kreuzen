use gospel::write::Writer;

use crate::Game;
use crate::io::{CReader, OData};

pub mod preload;
pub mod action_table;

#[derive(Clone)]
pub struct Opaque {
	pub bytes: Vec<u8>,
}

impl std::fmt::Debug for Opaque {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "[{} bytes]", self.bytes.len())
	}
 }


// Preload tables are stored separately from others, so they are not in this enum
#[derive(Debug, Clone)]
pub enum Table {
	Cs1ActionTable(Vec<action_table::Cs1Action>),
	Cs3ActionTable(Vec<action_table::Cs3Action>),
	Unknown(Opaque),
}

// Returns None if the chunk is code
pub(crate) fn read(f: &mut CReader, name: &str) -> rootcause::Result<Option<Table>> {
	if name == "ActionTable" {
		return Ok(Some(match f.game {
			Game::Cs1 | Game::Cs2 | Game::Tx => Table::Cs1ActionTable(action_table::read_cs1(f)?),
			Game::Cs3 | Game::Cs4 | Game::Reverie => Table::Cs3ActionTable(action_table::read_cs3(f)?),
		}));
	}

	let tables = [
		"",
		"AddCollision",
		"AlgoTable",
		"AnimeClipTable",
		"FieldMonsterData",
		"PartTable",
		"ReactionTable",
		"SummonTable",
		"ConditionTable",
		"BreakTable",
		"WeaponAttTable",
		"FieldFollowData",
		"ShinigPomBtlset",
	];

	let is_table = tables.contains(&name)
		|| name.starts_with("FC_auto")
		|| name.starts_with("BookData")
		|| name.starts_with("BTLSET")
		|| name.starts_with("StyleName");
	if is_table {
		let n = f.remaining().len();
		let opaque = Opaque { bytes: f.slice(n)?.to_vec() };
		return Ok(Some(Table::Unknown(opaque)));
	}

	// This is called from read_chunk, so we need to seek to end to prevent errors being reported
	let len = f.len();
	f.seek(len)?;
	Ok(None)
}

pub(crate) fn write(d: &OData, name: &str, table: &Table) -> rootcause::Result<(usize, Writer)> {
	let align = match (name, d.game) {
		_ if name.starts_with("FC_auto") => 16,
		("ReactionTable", Game::Cs1 | Game::Cs2) => 16,
		("ShinigPomBtlset", Game::Cs2) => 16,
		_ => 4,
	};
	let f = match table {
		Table::Cs1ActionTable(actions) => action_table::write_cs1(d, actions)?,
		Table::Cs3ActionTable(actions) => action_table::write_cs3(d, actions)?,
		Table::Unknown(opaque) => {
			let mut f = Writer::new();
			f.slice(&opaque.bytes);
			f
		}
	};
	Ok((align, f))
}

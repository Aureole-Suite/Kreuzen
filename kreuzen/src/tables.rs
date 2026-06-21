use gospel::write::Writer;

use crate::Game;
use crate::io::{CReader, OData};

pub mod preload;
pub mod action;

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
	ActionTable(Vec<action::Action>),
	Unknown(Opaque),
}

// Returns None if the chunk is code
pub(crate) fn read(f: &mut CReader, name: &str) -> rootcause::Result<Option<Table>> {
	if name == "ActionTable" {
		return Ok(Some(Table::ActionTable(action::read(f)?)));
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
		Table::ActionTable(actions) => action::write(d, actions)?,
		Table::Unknown(opaque) => {
			let mut f = Writer::new();
			f.slice(&opaque.bytes);
			f
		}
	};
	Ok((align, f))
}

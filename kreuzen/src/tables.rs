use gospel::write::Writer;

use crate::Game;
use crate::io::{CReader, OData};

pub mod preload;
pub mod action;
pub mod algo;
pub mod anime_clip;
pub mod book;
pub mod btlset;
pub mod break_;
pub mod field_follow;
pub mod part;
pub mod summon;
pub mod field_monster;
pub mod weapon_att;

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
	AlgoTable(Vec<algo::Algo>),
	AnimeClipTable(Vec<anime_clip::AnimeClip>),
	Book(book::Book),
	BreakTable(Vec<break_::Break>),
	FieldFollowData(field_follow::FieldFollow),
	PartTable(Vec<part::Part>),
	SummonTable(Vec<summon::Summon>),
	FieldMonsterData(field_monster::FieldMonster),
	WeaponAttTable(weapon_att::WeaponAtt),
	Btlset(btlset::Btlset),
	Dummy(Dummy),
	Unknown(Opaque),
}

#[derive(Debug, Clone, Copy)]
pub enum Dummy {
	Empty,
	D12,
	Dff,
}

impl Dummy {
	const fn bytes(&self) -> &'static [u8] {
		match self {
			Dummy::Empty => b"",
			// I have no idea what these byte sequences mean.
			// They look like garbage, but might as well roundtrip them I guess.
			Dummy::D12 => &[
				0x12, 0x00, 0x00,
				0x63, 0x00, 0x00, 0x00,
				0x13, 0x01, 0x0A, 0x00, 0x00,
				0x02, 0x00, 0x00, 0x00,
				0x23, 0x00,
				0x10, 0x13, 0x01, 0x07,
				0x00, 0xEF, 0xCD, 0xAB,
				0x07, 0xC8, 0x00, 0x00, 0x00,
			],
			Dummy::Dff => &const {
				let mut x = [0; 28];
				x[0] = 0xFF;
				x[1] = 0xFF;
				x[2] = 0xFF;
				x[3] = 0xFF;
				x
			}
		}
	}
}

// Returns None if the chunk is code
pub(crate) fn read(f: &mut CReader, name: &str) -> rootcause::Result<Option<Table>> {
	if name == "ActionTable" {
		return Ok(Some(Table::ActionTable(action::read(f)?)));
	}
	if name == "AlgoTable" {
		return Ok(Some(Table::AlgoTable(algo::read(f)?)));
	}
	if name == "AnimeClipTable" {
		return Ok(Some(Table::AnimeClipTable(anime_clip::read(f)?)));
	}
	if name == "FieldMonsterData" {
		return Ok(Some(Table::FieldMonsterData(field_monster::read(f)?)));
	}
	if name.starts_with("BookData") {
		return Ok(Some(Table::Book(book::read(f, name)?)));
	}
	if name.is_empty() {
		let r = f.remaining();
		if r.is_empty() {
			return Ok(Some(Table::Dummy(Dummy::Empty)));
		}
		if matches!(f.game, Game::Cs1 | Game::Cs2) {
			if r == Dummy::D12.bytes() {
				f.slice(r.len())?;
				return Ok(Some(Table::Dummy(Dummy::D12)));
			}
			if r == Dummy::Dff.bytes() {
				f.slice(r.len())?;
				return Ok(Some(Table::Dummy(Dummy::Dff)));
			}
		}
		if f.game == Game::Cs1 && f.scena == "a1700" {
			f.slice(r.len())?;
			return Ok(None);
		}
	}
	if name.is_empty() || name == "ShinigPomBtlset" || name.starts_with("BTLSET") {
		return Ok(Some(Table::Btlset(btlset::read(f)?)));
	}

	if name == "BreakTable" {
		return Ok(Some(Table::BreakTable(break_::read(f)?)));
	}
	if name == "WeaponAttTable" {
		return Ok(Some(Table::WeaponAttTable(weapon_att::read(f)?)));
	}

	if name == "FieldFollowData" {
		return Ok(Some(Table::FieldFollowData(field_follow::read(f)?)));
	}

	if name == "SummonTable" {
		return Ok(Some(Table::SummonTable(summon::read(f)?)));
	}

	if name == "PartTable" {
		return Ok(Some(Table::PartTable(part::read(f)?)));
	}

	let tables = [
		"AddCollision",
		"ReactionTable",
		"ConditionTable",
	];

	let is_table = tables.contains(&name)
		|| name.starts_with("FC_auto")
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
		Table::AlgoTable(algos) => algo::write(d, algos)?,
		Table::AnimeClipTable(clips) => anime_clip::write(d, clips)?,
		Table::Book(b) => book::write(d, b)?,
		Table::BreakTable(t) => break_::write(d, t.as_slice())?,
		Table::Btlset(b) => btlset::write(d, b)?,
		Table::FieldFollowData(ffd) => field_follow::write(d, ffd)?,
		Table::PartTable(t) => part::write(d, t)?,
		Table::SummonTable(t) => summon::write(d, t)?,
		Table::FieldMonsterData(fmd) => field_monster::write(d, fmd)?,
		Table::WeaponAttTable(t) => weapon_att::write(d, t)?,
		Table::Dummy(d) => {
			let mut f = Writer::new();
			f.slice(d.bytes());
			f
		}
		Table::Unknown(opaque) => {
			let mut f = Writer::new();
			f.slice(&opaque.bytes);
			f
		}
	};
	Ok((align, f))
}

use gospel::write::Writer;

use crate::Game;
use crate::io::{CReader, OData};

pub mod preload;
pub mod add_collision;
pub mod action;
pub mod algo;
pub mod anime_clip;
pub mod book;
pub mod btlset;
pub mod break_;
pub mod condition;
pub mod fc_auto;
pub mod field_follow;
pub mod part;
pub mod reaction;
pub mod style_name;
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
	AddCollision(Vec<add_collision::Collision>),
	ActionTable(Vec<action::Action>),
	AlgoTable(Vec<algo::Algo>),
	AnimeClipTable(Vec<anime_clip::AnimeClip>),
	Book(book::Book),
	BreakTable(Vec<break_::Break>),
	ConditionTable(Vec<condition::Condition>),
	FcAuto(String),
	FieldFollowData(field_follow::FieldFollow),
	PartTable(Vec<part::Part>),
	ReactionTable(Vec<reaction::Reaction>),
	StyleName(style_name::StyleName),
	SummonTable(Vec<summon::Summon>),
	FieldMonsterData(field_monster::FieldMonster),
	WeaponAttTable(weapon_att::WeaponAtt),
	Btlset(btlset::Btlset),
	Dummy(Dummy),
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
	match name {
		"ActionTable"      => return Ok(Some(Table::ActionTable(action::read(f)?))),
		"AddCollision"     => return Ok(Some(Table::AddCollision(add_collision::read(f)?))),
		"AlgoTable"        => return Ok(Some(Table::AlgoTable(algo::read(f)?))),
		"AnimeClipTable"   => return Ok(Some(Table::AnimeClipTable(anime_clip::read(f)?))),
		"BreakTable"       => return Ok(Some(Table::BreakTable(break_::read(f)?))),
		"ConditionTable"   => return Ok(Some(Table::ConditionTable(condition::read(f)?))),
		"FieldFollowData"  => return Ok(Some(Table::FieldFollowData(field_follow::read(f)?))),
		"FieldMonsterData" => return Ok(Some(Table::FieldMonsterData(field_monster::read(f)?))),
		"PartTable"        => return Ok(Some(Table::PartTable(part::read(f)?))),
		"ReactionTable"    => return Ok(Some(Table::ReactionTable(reaction::read(f)?))),
		"SummonTable"      => return Ok(Some(Table::SummonTable(summon::read(f)?))),
		"WeaponAttTable"   => return Ok(Some(Table::WeaponAttTable(weapon_att::read(f)?))),
		name if name.starts_with("BookData") => return Ok(Some(Table::Book(book::read(f, name)?))),
		name if name.starts_with("FC_auto") => return Ok(Some(Table::FcAuto(fc_auto::read(f)?))),
		name if name.starts_with("StyleName") => return Ok(Some(Table::StyleName(style_name::read(f)?))),
		_ => {}
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
		Table::AddCollision(t) => add_collision::write(d, t)?,
		Table::ActionTable(t) => action::write(d, t)?,
		Table::AlgoTable(t) => algo::write(d, t)?,
		Table::AnimeClipTable(t) => anime_clip::write(d, t)?,
		Table::Book(t) => book::write(d, t)?,
		Table::BreakTable(t) => break_::write(d, t)?,
		Table::ConditionTable(t) => condition::write(d, t)?,
		Table::FcAuto(t) => fc_auto::write(d, t)?,
		Table::Btlset(t) => btlset::write(d, t)?,
		Table::FieldFollowData(t) => field_follow::write(d, t)?,
		Table::PartTable(t) => part::write(d, t)?,
		Table::ReactionTable(t) => reaction::write(d, t)?,
		Table::StyleName(t) => style_name::write(d, t)?,
		Table::SummonTable(t) => summon::write(d, t)?,
		Table::FieldMonsterData(t) => field_monster::write(d, t)?,
		Table::WeaponAttTable(t) => weapon_att::write(d, t)?,
		Table::Dummy(t) => {
			let mut f = Writer::new();
			f.slice(t.bytes());
			f
		}
	};
	Ok((align, f))
}

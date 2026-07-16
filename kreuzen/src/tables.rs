use gospel::write::Writer;

use crate::Game;
use crate::io::{CReader, OData};

pub mod action;
pub mod add_collision;
pub mod algo;
pub mod anime_clip;
pub mod book;
pub mod break_;
pub mod btlset;
pub mod condition;
pub mod fc_auto;
pub mod field_follow;
pub mod field_monster;
pub mod part;
pub mod reaction;
pub mod style_name;
pub mod summon;
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

#[derive(Debug, Clone, PartialEq)]
pub enum Table {
	AddCollision(Vec<add_collision::Collision>),
	ActionTable(Vec<action::Action>),
	AlgoTable(Vec<algo::Algo>),
	AnimeClipTable(Vec<anime_clip::AnimeClip>),
	BookData { name: String, book: book::BookData }, // Raw book data
	Book { name: String, pages: Vec<book::Page> },   // Desugared
	BreakTable(Vec<break_::Break>),
	ConditionTable(Vec<condition::Condition>),
	FcAuto { name: String, text: String },
	FieldFollowData(field_follow::FieldFollow),
	PartTable(Vec<part::Part>),
	ReactionTable(Vec<reaction::Reaction>),
	StyleName { name: String, style: style_name::StyleName },
	SummonTable(Vec<summon::Summon>),
	FieldMonsterData(field_monster::FieldMonster),
	WeaponAttTable(weapon_att::WeaponAtt),
	Btlset { name: String, btlset: btlset::Btlset },
	Dummy(Dummy),
}

impl Table {
	pub fn name(&self) -> &str {
		match self {
			Table::AddCollision(_) => "AddCollision",
			Table::ActionTable(_) => "ActionTable",
			Table::AlgoTable(_) => "AlgoTable",
			Table::AnimeClipTable(_) => "AnimeClipTable",
			Table::Book { name, .. } => name,
			Table::BookData { name, .. } => name,
			Table::BreakTable(_) => "BreakTable",
			Table::ConditionTable(_) => "ConditionTable",
			Table::FcAuto { name, .. } => name,
			Table::FieldFollowData(_) => "FieldFollowData",
			Table::PartTable(_) => "PartTable",
			Table::ReactionTable(_) => "ReactionTable",
			Table::StyleName { name, .. } => name,
			Table::SummonTable(_) => "SummonTable",
			Table::FieldMonsterData(_) => "FieldMonsterData",
			Table::WeaponAttTable(_) => "WeaponAttTable",
			Table::Btlset { name, .. } => name,
			Table::Dummy(_) => "",
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Dummy {
	Empty,
	Dff,
}

impl Dummy {
	const fn bytes(&self) -> &'static [u8] {
		match self {
			Dummy::Empty => b"",
			Dummy::Dff => {
				&const {
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
}

// Returns None if the chunk is code
#[rustfmt::skip]
pub(crate) fn read(f: &mut CReader, name: &str) -> rootcause::Result<Option<Table>> {
	Ok(Some(match name {
		"ActionTable"      => Table::ActionTable(action::read(f)?),
		"AddCollision"     => Table::AddCollision(add_collision::read(f)?),
		"AlgoTable"        => Table::AlgoTable(algo::read(f)?),
		"AnimeClipTable"   => Table::AnimeClipTable(anime_clip::read(f)?),
		"BreakTable"       => Table::BreakTable(break_::read(f)?),
		"ConditionTable"   => Table::ConditionTable(condition::read(f)?),
		"FieldFollowData"  => Table::FieldFollowData(field_follow::read(f)?),
		"FieldMonsterData" => Table::FieldMonsterData(field_monster::read(f)?),
		"PartTable"        => Table::PartTable(part::read(f)?),
		"ReactionTable"    => Table::ReactionTable(reaction::read(f)?),
		"SummonTable"      => Table::SummonTable(summon::read(f)?),
		"WeaponAttTable"   => Table::WeaponAttTable(weapon_att::read(f)?),
		name if name.starts_with("BookData")  => Table::BookData { name: name.to_owned(), book: book::read(f, name)? },
		name if name.starts_with("FC_auto")   => Table::FcAuto { name: name.to_owned(), text: fc_auto::read(f)? },
		name if name.starts_with("StyleName") => Table::StyleName { name: name.to_owned(), style: style_name::read(f)? },
		_ => return read_other(f, name),
	}))
}

fn read_other(f: &mut CReader<'_>, name: &str) -> Result<Option<Table>, rootcause::Report> {
	if name.is_empty() {
		let r = f.remaining();
		if r.is_empty() {
			return Ok(Some(Table::Dummy(Dummy::Empty)));
		}
		if matches!(f.game, Game::Cs1 | Game::Cs2) {
			if f.scena == "a0004" && f.clone().check(b"b").is_err() {
				f.slice(r.len())?;
				return Ok(None);
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
		return Ok(Some(Table::Btlset { name: name.to_owned(), btlset: btlset::read(f)? }));
	}

	// This is called from read_chunk, so we need to seek to end to prevent errors being reported
	let len = f.len();
	f.seek(len)?;
	Ok(None)
}

pub(crate) fn write(d: &OData, table: &Table) -> rootcause::Result<(usize, Writer)> {
	let align = match (table, d.game) {
		(Table::FcAuto { .. }, _) => 16,
		(Table::ReactionTable(_), Game::Cs1 | Game::Cs2) => 16,
		(Table::Btlset { name, .. }, Game::Cs2) if name == "ShinigPomBtlset" => 16,
		_ => 4,
	};
	let f = match table {
		Table::AddCollision(t) => add_collision::write(d, t)?,
		Table::ActionTable(t) => action::write(d, t)?,
		Table::AlgoTable(t) => algo::write(d, t)?,
		Table::AnimeClipTable(t) => anime_clip::write(d, t)?,
		Table::Book { .. } => rootcause::bail!("Book must be desugared before writing"),
		Table::BookData { book, .. } => book::write(d, book)?,
		Table::BreakTable(t) => break_::write(d, t)?,
		Table::ConditionTable(t) => condition::write(d, t)?,
		Table::FcAuto { text, .. } => fc_auto::write(d, text)?,
		Table::Btlset { btlset, .. } => btlset::write(d, btlset)?,
		Table::FieldFollowData(t) => field_follow::write(d, t)?,
		Table::PartTable(t) => part::write(d, t)?,
		Table::ReactionTable(t) => reaction::write(d, t)?,
		Table::StyleName { style, .. } => style_name::write(d, style)?,
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

use kreuzen::tables::action::Action;
use kreuzen::tables::add_collision::Collision;
use kreuzen::tables::algo::Algo;
use kreuzen::tables::anime_clip::AnimeClip;
use kreuzen::tables::book::{BookData, Page, TitlePage};
use kreuzen::tables::break_::Break;
use kreuzen::tables::btlset::{Btlset, Variant};
use kreuzen::tables::condition::Condition;
use kreuzen::tables::field_follow::FieldFollow;
use kreuzen::tables::field_monster::FieldMonster;
use kreuzen::tables::part::Part;
use kreuzen::tables::reaction::{PartReaction, Reaction, ReactionKind};
use kreuzen::tables::style_name::StyleName;
use kreuzen::tables::summon::Summon;
use kreuzen::tables::weapon_att::WeaponAtt;
use kreuzen::tables::{Dummy, Table};

use crate::Parse;

use super::parser::{Parser, Result};

impl Parse for Table {
	fn parse(p: &mut Parser) -> Result<Self> {
		p.alt()
			.test_kw("AddCollision", |p| Ok(Table::AddCollision(p.parse()?)))
			.test_kw("ActionTable", |p| Ok(Table::ActionTable(p.parse()?)))
			.test_kw("AlgoTable", |p| Ok(Table::AlgoTable(p.parse()?)))
			.test_kw("AnimeClipTable", |p| Ok(Table::AnimeClipTable(p.parse()?)))
			.test_kw("BookData", |p| Ok(Table::BookData(p.parse()?, p.parse()?)))
			.test_kw("Book", |p| Ok(Table::Book(p.parse()?, p.parse()?)))
			.test_kw("BreakTable", |p| Ok(Table::BreakTable(p.parse()?)))
			.test_kw("ConditionTable", |p| Ok(Table::ConditionTable(p.parse()?)))
			.test_kw("FcAuto", |p| Ok(Table::FcAuto(p.parse()?, p.parse()?)))
			.test_kw("FieldFollowData", |p| Ok(Table::FieldFollowData(p.parse()?)))
			.test_kw("PartTable", |p| Ok(Table::PartTable(p.parse()?)))
			.test_kw("ReactionTable", |p| Ok(Table::ReactionTable(p.parse()?)))
			.test_kw("StyleName", |p| Ok(Table::StyleName(p.parse()?, p.parse()?)))
			.test_kw("SummonTable", |p| Ok(Table::SummonTable(p.parse()?)))
			.test_kw("FieldMonsterData", |p| Ok(Table::FieldMonsterData(p.parse()?)))
			.test_kw("WeaponAttTable", |p| Ok(Table::WeaponAttTable(p.parse()?)))
			.test_kw("Btlset", |p| Ok(Table::Btlset(p.parse()?, p.parse()?)))
			.test_kw("Dummy", |p| Ok(Table::Dummy(p.parse()?)))
			.finish()
	}
}

impl Parse for Dummy {
	fn parse(p: &mut Parser) -> Result<Self> {
		p.alt().test_kw("empty", |_| Ok(Dummy::Empty)).test_kw("dff", |_| Ok(Dummy::Dff)).finish()
	}
}

crate::types::block!(
	Collision, Action, Algo, AnimeClip, Page, Break, Condition, Part, Reaction, Summon, Variant
);

impl Parse for Collision {
	fn parse(p: &mut Parser) -> Result<Self> {
		Ok(Collision { a: p.parse()?, b: p.parse()? })
	}
}
impl Parse for Action {
	fn parse(p: &mut Parser) -> Result<Self> {
		Ok(Action {
			id: p.parse()?,
			kind: p.parse()?,
			target: p.parse()?,
			u2: p.parse()?,
			cast_time: p.parse()?,
			recovery_time: p.parse()?,
			cp_cost: p.parse()?,
			flags: p.parse()?,
			ani: p.parse()?,
			name: p.parse()?,
			effects: p.parse_many()?,
		})
	}
}
impl Parse for Algo {
	fn parse(p: &mut Parser) -> Result<Self> {
		Ok(Algo {
			id: p.parse()?,
			chance: p.parse()?,
			use_limit: p.parse()?,
			target_priority: p.parse()?,
			cond: p.parse()?,
		})
	}
}
impl Parse for AnimeClip {
	fn parse(p: &mut Parser) -> Result<Self> {
		Ok(AnimeClip { kind: p.parse()?, a: p.parse()?, b: p.parse()? })
	}
}
impl Parse for Break {
	fn parse(p: &mut Parser) -> Result<Self> {
		Ok(Break { id: p.parse()?, value: p.parse()? })
	}
}
impl Parse for Condition {
	fn parse(p: &mut Parser) -> Result<Self> {
		Ok(Condition { id: p.parse()?, entries: p.parse_many()? })
	}
}
impl Parse for FieldFollow {
	fn parse(p: &mut Parser) -> Result<Self> {
		Ok(FieldFollow {
			a: p.parse()?,
			b: p.parse()?,
			c: p.parse()?,
			d: p.parse()?,
			e: p.parse()?,
		})
	}
}
impl Parse for FieldMonster {
	fn parse(p: &mut Parser) -> Result<Self> {
		Ok(FieldMonster {
			a: p.parse()?,
			b: p.parse()?,
			c: p.parse()?,
			floats: p.parse_many()?,
		})
	}
}
impl Parse for Part {
	fn parse(p: &mut Parser) -> Result<Self> {
		Ok(Part { id: p.parse()?, a: p.parse()?, b: p.parse()? })
	}
}
impl Parse for Summon {
	fn parse(p: &mut Parser) -> Result<Self> {
		Ok(Summon {
			kind: p.parse()?,
			a: p.parse()?,
			b: p.parse()?,
			name: p.parse()?,
		})
	}
}
impl Parse for WeaponAtt {
	fn parse(p: &mut Parser) -> Result<Self> {
		Ok(WeaponAtt {
			slash: p.parse()?,
			thrust: p.parse()?,
			pierce: p.parse()?,
			strike: p.parse()?,
		})
	}
}

impl Parse for StyleName {
	fn parse(p: &mut Parser) -> Result<Self> {
		Ok(StyleName(p.parse()?, p.parse()?))
	}
}

impl Parse for BookData {
	fn parse(p: &mut Parser) -> Result<Self> {
		p.alt()
			.test_kw("header", |p| p.parse().map(BookData::Header))
			.test_kw("title_page", |p| Ok(BookData::TitlePage(p.parse()?, p.parse()?)))
			.test_kw("page", |p| p.parse().map(BookData::Page))
			.test_kw("empty", |_| Ok(BookData::Empty))
			.finish()
	}
}

impl Parse for TitlePage {
	fn parse(p: &mut Parser) -> Result<Self> {
		Ok(TitlePage { title: p.parse()?, data: p.parse()? })
	}
}

impl Parse for Page {
	fn parse(p: &mut Parser) -> Result<Self> {
		let title = p
			.alt()
			.test_kw("title_page", |p| p.parse().map(Some))
			.test_kw("page", |_| Ok(None))
			.finish()?;
		let text = p.parse()?;
		Ok(Page { title, text })
	}
}

impl Parse for Reaction {
	fn parse(p: &mut Parser) -> Result<Self> {
		let id = p.parse()?;
		let kind = p
			.alt()
			.test(|p| p.parse().map(ReactionKind::Alias))
			// PartReaction prints its own parens, so this is three consecutive
			// groups rather than one array group.
			.test(|p| Ok(ReactionKind::Parts([p.parse()?, p.parse()?, p.parse()?])))
			.finish()?;
		Ok(Reaction { id, kind })
	}
}

impl Parse for PartReaction {
	fn parse(p: &mut Parser) -> Result<Self> {
		let (rating, unbalance, hit, miss, counter) = p.parse()?;
		Ok(PartReaction { rating, unbalance, hit, miss, counter })
	}
}

impl Parse for Btlset {
	fn parse(p: &mut Parser) -> Result<Self> {
		Ok(Btlset {
			field: p.parse()?,
			bounds: p.parse()?,
			btl_id: p.parse()?,
			unk1: p.parse()?,
			bgm: p.parse()?,
			unk2: p.parse()?,
			script: p.parse()?,
			variants: p.parse()?,
		})
	}
}

impl Parse for Variant {
	fn parse(p: &mut Parser) -> Result<Self> {
		let id = p.parse()?;
		let mut monsters = Vec::new();
		while let Ok(monster) = p.parse() {
			let prob = if p.glued_punct(':').is_ok() { p.parse()? } else { 100 };
			monsters.push((monster, prob));
		}
		Ok(Variant { id, monsters })
	}
}

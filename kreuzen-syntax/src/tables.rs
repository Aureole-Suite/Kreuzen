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

use crate::types::{block, row};
use crate::{Parse, Parser, Print, Printer, Result};

impl Print for Table {
	fn print(&self, ctx: &mut Printer) {
		match self {
			Table::AddCollision(t) => {
				ctx.word("AddCollision");
				t.print(ctx);
			}
			Table::ActionTable(t) => {
				ctx.word("ActionTable");
				t.print(ctx);
			}
			Table::AlgoTable(t) => {
				ctx.word("AlgoTable");
				t.print(ctx);
			}
			Table::AnimeClipTable(t) => {
				ctx.word("AnimeClipTable");
				t.print(ctx);
			}
			Table::BookData(name, book) => {
				ctx.word("BookData");
				name.print(ctx);
				book.print(ctx);
			}
			Table::Book(name, pages) => {
				ctx.word("Book");
				name.print(ctx);
				pages.print(ctx);
			}
			Table::BreakTable(t) => {
				ctx.word("BreakTable");
				t.print(ctx);
			}
			Table::ConditionTable(t) => {
				ctx.word("ConditionTable");
				t.print(ctx);
			}
			Table::FcAuto(name, text) => {
				ctx.word("FcAuto");
				name.print(ctx);
				text.print(ctx);
			}
			Table::FieldFollowData(t) => {
				ctx.word("FieldFollowData");
				t.print(ctx);
			}
			Table::PartTable(t) => {
				ctx.word("PartTable");
				t.print(ctx);
			}
			Table::ReactionTable(t) => {
				ctx.word("ReactionTable");
				t.print(ctx);
			}
			Table::StyleName(name, style) => {
				ctx.word("StyleName");
				name.print(ctx);
				style.print(ctx);
			}
			Table::SummonTable(t) => {
				ctx.word("SummonTable");
				t.print(ctx);
			}
			Table::FieldMonsterData(t) => {
				ctx.word("FieldMonsterData");
				t.print(ctx);
			}
			Table::WeaponAttTable(t) => {
				ctx.word("WeaponAttTable");
				t.print(ctx);
			}
			Table::Btlset(name, btlset) => {
				ctx.word("Btlset");
				name.print(ctx);
				btlset.print(ctx);
			}
			Table::Dummy(t) => {
				ctx.word("Dummy");
				t.print(ctx);
			}
		}
	}
}

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

block!(Action, "id kind target ?? cast recovery cp flags ani name effects");
row!(struct Action { id, kind, target, u2, cast_time, recovery_time, cp_cost, flags, ani, name, effects* });

block!(Algo, "id chance use_limit target_priority cond");
row!(struct Algo { id, chance, use_limit, target_priority, cond });

block!(AnimeClip);
row!(struct AnimeClip { kind, a, b });

block!(Break);
row!(struct Break { id, value });

block!(Collision);
row!(struct Collision { a, b });

block!(Condition);
row!(struct Condition { id, entries* });

row!(struct FieldFollow { a, b, c, d, e });

row!(struct FieldMonster { a, b, c, floats* });

block!(Part);
row!(struct Part { id, a, b });

block!(Reaction, "id (stars unbalance hit miss counter) ...");

impl Print for Reaction {
	fn print(&self, ctx: &mut Printer) {
		self.id.print(ctx);
		match &self.kind {
			ReactionKind::Parts(parts) => {
				for p in parts {
					p.print(ctx);
				}
			}
			ReactionKind::Alias(m) => {
				m.print(ctx);
			}
		}
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

impl Print for PartReaction {
	fn print(&self, ctx: &mut Printer) {
		let PartReaction { rating, unbalance, hit, miss, counter } = *self;
		(rating, unbalance, hit, miss, counter).print(ctx);
	}
}

impl Parse for PartReaction {
	fn parse(p: &mut Parser) -> Result<Self> {
		let (rating, unbalance, hit, miss, counter) = p.parse()?;
		Ok(PartReaction { rating, unbalance, hit, miss, counter })
	}
}

impl Print for StyleName {
	fn print(&self, ctx: &mut Printer) {
		self.0.print(ctx);
		self.1.print(ctx);
	}
}

impl Parse for StyleName {
	fn parse(p: &mut Parser) -> Result<Self> {
		Ok(StyleName(p.parse()?, p.parse()?))
	}
}

block!(Summon);
row!(struct Summon { kind, a, b, name });

row!(struct WeaponAtt { slash, thrust, pierce, strike });

row!(struct Btlset { field, bounds, btl_id, unk1, bgm, unk2, script, variants });

block!(Variant);

impl Print for Variant {
	fn print(&self, ctx: &mut Printer) {
		self.id.print(ctx);
		for (monster, prob) in &self.monsters {
			monster.print(ctx);
			if *prob != 100 {
				ctx.sym(":");
				prob.print(ctx);
			}
		}
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

impl Print for Dummy {
	fn print(&self, ctx: &mut Printer) {
		ctx.word(match self {
			Dummy::Empty => "empty",
			Dummy::Dff => "dff",
		});
	}
}

impl Parse for Dummy {
	fn parse(p: &mut Parser) -> Result<Self> {
		p.alt().test_kw("empty", |_| Ok(Dummy::Empty)).test_kw("dff", |_| Ok(Dummy::Dff)).finish()
	}
}

impl Print for BookData {
	fn print(&self, ctx: &mut Printer) {
		match self {
			BookData::Header(n) => {
				ctx.word("header");
				n.print(ctx);
			}
			BookData::TitlePage(title, text) => {
				ctx.word("title_page");
				title.print(ctx);
				text.print(ctx);
			}
			BookData::Page(text) => {
				ctx.word("page");
				text.print(ctx);
			}
			BookData::Empty => {
				ctx.word("empty");
			}
		}
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

block!(Page);

impl Print for Page {
	fn print(&self, ctx: &mut Printer) {
		match &self.title {
			Some(t) => {
				ctx.word("title_page");
				t.print(ctx);
			}
			None => ctx.word("page"),
		}
		self.text.print(ctx);
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

row!(struct TitlePage { title, data });

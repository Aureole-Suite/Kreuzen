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

use crate::types::block;
use crate::{Parse, Parser, Print, Printer, Result};

impl Print for Table {
	fn print(&self, ctx: &mut Printer) {
		match self {
			Table::AddCollision(t) => {
				ctx.word("AddCollision");
				ctx.block(t, Collision::print);
			}
			Table::ActionTable(t) => {
				ctx.word("ActionTable");
				ctx.block_commented("id kind target ?? cast recovery cp flags ani name effects", t, Action::print);
			}
			Table::AlgoTable(t) => {
				ctx.word("AlgoTable");
				ctx.block_commented("id chance use_limit target_priority cond", t, Algo::print);
			}
			Table::AnimeClipTable(t) => {
				ctx.word("AnimeClipTable");
				ctx.block(t, AnimeClip::print);
			}
			Table::BookData(name, book) => {
				ctx.word("BookData");
				name.print(ctx);
				book.print(ctx);
			}
			Table::Book(name, pages) => {
				ctx.word("Book");
				name.print(ctx);
				ctx.block(pages, Page::print);
			}
			Table::BreakTable(t) => {
				ctx.word("BreakTable");
				ctx.block_commented("id value", t, Break::print);
			}
			Table::ConditionTable(t) => {
				ctx.word("ConditionTable");
				ctx.block(t, Condition::print);
			}
			Table::FcAuto(name, text) => {
				ctx.word("FcAuto");
				name.print(ctx);
				text.print(ctx);
				ctx.sym_(";");
			}
			Table::FieldFollowData(t) => {
				ctx.word("FieldFollowData");
				t.print(ctx);
			}
			Table::PartTable(t) => {
				ctx.word("PartTable");
				ctx.block(t, Part::print);
			}
			Table::ReactionTable(t) => {
				ctx.word("ReactionTable");
				ctx.block_commented("id (stars unbalance hit miss counter) ...", t, Reaction::print);
			}
			Table::StyleName(name, style) => {
				ctx.word("StyleName");
				name.print(ctx);
				style.print(ctx);
			}
			Table::SummonTable(t) => {
				ctx.word("SummonTable");
				ctx.block(t, Summon::print);
			}
			Table::FieldMonsterData(t) => {
				ctx.word("FieldMonsterData");
				t.print(ctx);
			}
			Table::WeaponAttTable(t) => {
				ctx.word("WeaponAttTable");
				t.print(ctx);
				ctx.comment("slash thrust pierce strike");
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

block!(Action);
impl Print for Action {
	fn print(&self, ctx: &mut Printer) {
		self.id.print(ctx);
		self.kind.print(ctx);
		self.target.print(ctx);
		self.u2.print(ctx);
		self.cast_time.print(ctx);
		self.recovery_time.print(ctx);
		self.cp_cost.print(ctx);
		self.flags.print(ctx);
		self.ani.print(ctx);
		self.name.print(ctx);
		for item in &self.effects {
			item.print(ctx);
		}
		ctx.sym_(";");
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

block!(Algo);

impl Print for Algo {
	fn print(&self, ctx: &mut Printer) {
		self.id.print(ctx);
		self.chance.print(ctx);
		self.use_limit.print(ctx);
		self.target_priority.print(ctx);
		self.cond.print(ctx);
		ctx.sym_(";");
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

block!(AnimeClip);

impl Print for AnimeClip {
	fn print(&self, ctx: &mut Printer) {
		self.kind.print(ctx);
		self.a.print(ctx);
		self.b.print(ctx);
		ctx.sym_(";");
	}
}

impl Parse for AnimeClip {
	fn parse(p: &mut Parser) -> Result<Self> {
		Ok(AnimeClip { kind: p.parse()?, a: p.parse()?, b: p.parse()? })
	}
}

block!(Break);

impl Print for Break {
	fn print(&self, ctx: &mut Printer) {
		self.id.print(ctx);
		self.value.print(ctx);
		ctx.sym_(";");
	}
}

impl Parse for Break {
	fn parse(p: &mut Parser) -> Result<Self> {
		Ok(Break { id: p.parse()?, value: p.parse()? })
	}
}

block!(Collision);

impl Print for Collision {
	fn print(&self, ctx: &mut Printer) {
		self.a.print(ctx);
		self.b.print(ctx);
		ctx.sym_(";");
	}
}

impl Parse for Collision {
	fn parse(p: &mut Parser) -> Result<Self> {
		Ok(Collision { a: p.parse()?, b: p.parse()? })
	}
}

block!(Condition);

impl Print for Condition {
	fn print(&self, ctx: &mut Printer) {
		self.id.print(ctx);
		for item in &self.entries {
			item.print(ctx);
		}
		ctx.sym_(";");
	}
}

impl Parse for Condition {
	fn parse(p: &mut Parser) -> Result<Self> {
		Ok(Condition { id: p.parse()?, entries: p.parse_many()? })
	}
}

impl Print for FieldFollow {
	fn print(&self, ctx: &mut Printer) {
		self.a.print(ctx);
		self.b.print(ctx);
		self.c.print(ctx);
		self.d.print(ctx);
		self.e.print(ctx);
		ctx.sym_(";");
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

impl Print for FieldMonster {
	fn print(&self, ctx: &mut Printer) {
		self.a.print(ctx);
		self.b.print(ctx);
		self.c.print(ctx);
		for &v in &self.floats {
			v.print(ctx);
		}
		ctx.sym_(";");
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

block!(Part);

impl Print for Part {
	fn print(&self, ctx: &mut Printer) {
		self.id.print(ctx);
		self.a.print(ctx);
		self.b.print(ctx);
		ctx.sym_(";");
	}
}

impl Parse for Part {
	fn parse(p: &mut Parser) -> Result<Self> {
		Ok(Part { id: p.parse()?, a: p.parse()?, b: p.parse()? })
	}
}

block!(Reaction);

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
		ctx.sym_(";");
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
		ctx._sym("(");
		self.rating.print(ctx);
		self.unbalance.print(ctx);
		self.hit.print(ctx);
		self.miss.print(ctx);
		self.counter.print(ctx);
		ctx.sym_(")");
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
		ctx.sym_(";");
	}
}

impl Parse for StyleName {
	fn parse(p: &mut Parser) -> Result<Self> {
		Ok(StyleName(p.parse()?, p.parse()?))
	}
}

block!(Summon);

impl Print for Summon {
	fn print(&self, ctx: &mut Printer) {
		self.kind.print(ctx);
		self.a.print(ctx);
		self.b.print(ctx);
		self.name.print(ctx);
		ctx.sym_(";");
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

impl Print for WeaponAtt {
	fn print(&self, ctx: &mut Printer) {
		self.slash.print(ctx);
		self.thrust.print(ctx);
		self.pierce.print(ctx);
		self.strike.print(ctx);
		ctx.sym_(";");
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

impl Print for Btlset {
	fn print(&self, ctx: &mut Printer) {
		self.field.print(ctx);
		self.bounds.print(ctx);
		self.btl_id.print(ctx);
		self.unk1.print(ctx);
		self.bgm.print(ctx);
		self.unk2.print(ctx);
		self.script.print(ctx);
		ctx.block(&self.variants, Variant::print);
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
		ctx.sym_(";");
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
		ctx.sym_(";");
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
				ctx.sym_(";");
			}
			BookData::TitlePage(title, text) => {
				ctx.word("title_page");
				title.print(ctx);
				text.print(ctx);
				ctx.sym_(";");
			}
			BookData::Page(text) => {
				ctx.word("page");
				text.print(ctx);
				ctx.sym_(";");
			}
			BookData::Empty => {
				ctx.word("empty");
				ctx.sym_(";");
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
		ctx.sym_(";");
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

impl Print for TitlePage {
	fn print(&self, ctx: &mut Printer) {
		self.title.print(ctx);
		self.data.print(ctx);
	}
}

impl Parse for TitlePage {
	fn parse(p: &mut Parser) -> Result<Self> {
		Ok(TitlePage { title: p.parse()?, data: p.parse()? })
	}
}

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

row!(
	enum Table {
		AddCollision(t),
		ActionTable(t),
		AlgoTable(t),
		AnimeClipTable(t),
		BookData(name, book),
		Book(name, pages),
		BreakTable(t),
		ConditionTable(t),
		FcAuto(name, text),
		FieldFollowData(t),
		PartTable(t),
		ReactionTable(t),
		StyleName(name, style),
		SummonTable(t),
		FieldMonsterData(t),
		WeaponAttTable(t),
		Btlset(name, btlset),
		Dummy(t),
	}
);

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

row!(
	enum Dummy {
		Empty = "empty",
		Dff = "dff",
	}
);

row!(
	enum BookData {
		Header(n) = "header",
		TitlePage(title, text) = "title_page",
		Page(text) = "page",
		Empty = "empty",
	}
);

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

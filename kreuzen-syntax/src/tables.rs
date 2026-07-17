use kreuzen::tables::action::Action;
use kreuzen::tables::add_collision::Collision;
use kreuzen::tables::algo::Algo;
use kreuzen::tables::anime_clip::AnimeClip;
use kreuzen::tables::book::{BookData, Page};
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

use super::{Print, Printer};

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

impl Print for Collision {
	fn print(&self, ctx: &mut Printer) {
		self.a.print(ctx);
		self.b.print(ctx);
		ctx.sym_(";");
	}
}

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

impl Print for AnimeClip {
	fn print(&self, ctx: &mut Printer) {
		self.kind.print(ctx);
		self.a.print(ctx);
		self.b.print(ctx);
		ctx.sym_(";");
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
				title.title.print(ctx);
				title.data.print(ctx);
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

impl Print for Page {
	fn print(&self, ctx: &mut Printer) {
		match &self.title {
			Some(t) => {
				ctx.word("title_page");
				t.title.print(ctx);
				t.data.print(ctx);
			}
			None => ctx.word("page"),
		}
		self.text.print(ctx);
		ctx.sym_(";");
	}
}

impl Print for Break {
	fn print(&self, ctx: &mut Printer) {
		self.id.print(ctx);
		self.value.print(ctx);
		ctx.sym_(";");
	}
}

impl Print for Condition {
	fn print(&self, ctx: &mut Printer) {
		self.id.print(ctx);
		for item in &self.entries {
			item.print(ctx);
		}
		ctx.sym_(";");
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

impl Print for Part {
	fn print(&self, ctx: &mut Printer) {
		self.id.print(ctx);
		self.a.print(ctx);
		self.b.print(ctx);
		ctx.sym_(";");
	}
}

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

impl Print for StyleName {
	fn print(&self, ctx: &mut Printer) {
		self.0.print(ctx);
		self.1.print(ctx);
		ctx.sym_(";");
	}
}

impl Print for Summon {
	fn print(&self, ctx: &mut Printer) {
		self.kind.print(ctx);
		self.a.print(ctx);
		self.b.print(ctx);
		self.name.print(ctx);
		ctx.sym_(";");
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

impl Print for Dummy {
	fn print(&self, ctx: &mut Printer) {
		ctx.word(match self {
			Dummy::Empty => "empty",
			Dummy::Dff => "dff",
		});
		ctx.sym_(";");
	}
}

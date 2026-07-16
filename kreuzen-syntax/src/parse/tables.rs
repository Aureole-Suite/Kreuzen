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

use super::alt::Alt;
use super::parser::{Error, Expect, Parser, Result};
use super::types::Parse;
use super::PCtx;

pub fn parse_table(p: &mut Parser, _ctx: &PCtx) -> Result<Table> {
	let span = p.next_span();
	let name = p.ident()?;
	Ok(match name {
		"AddCollision" => Table::AddCollision(rows(p)?),
		"ActionTable" => Table::ActionTable(rows(p)?),
		"AlgoTable" => Table::AlgoTable(rows(p)?),
		"AnimeClipTable" => Table::AnimeClipTable(rows(p)?),
		"BookData" => {
			let name = p.parse()?;
			Table::BookData { name, book: p.parse()? }
		}
		"Book" => {
			let name = p.parse()?;
			Table::Book { name, pages: rows(p)? }
		}
		"BreakTable" => Table::BreakTable(rows(p)?),
		"ConditionTable" => Table::ConditionTable(rows(p)?),
		"FcAuto" => {
			let name = p.parse()?;
			let text = p.parse()?;
			Table::FcAuto { name, text }
		}
		"FieldFollowData" => Table::FieldFollowData(p.parse()?),
		"PartTable" => Table::PartTable(rows(p)?),
		"ReactionTable" => Table::ReactionTable(rows(p)?),
		"StyleName" => {
			let name = p.parse()?;
			Table::StyleName { name, style: p.parse()? }
		}
		"SummonTable" => Table::SummonTable(rows(p)?),
		"FieldMonsterData" => Table::FieldMonsterData(p.parse()?),
		"WeaponAttTable" => Table::WeaponAttTable(p.parse()?),
		"Btlset" => {
			let name = p.parse()?;
			Table::Btlset { name, btlset: p.parse()? }
		}
		"Dummy" => {
			let span = p.next_span();
			let dummy = match p.ident()? {
				"empty" => Dummy::Empty,
				"dff" => Dummy::Dff,
				d => {
					p.errors.error(format!("unknown dummy '{d}'"), span);
					return Err(Error);
				}
			};
			Table::Dummy(dummy)
		}
		_ => {
			p.errors.error(format!("unknown chunk '{name}'"), span);
			return Err(Error);
		}
	})
}

/// A `{ ... }` block of `;`-terminated rows.
fn rows<T: Parse>(p: &mut Parser) -> Result<Vec<T>> {
	let mut inner = p.delim('{')?;
	Ok(super::parse_seq(&mut inner, |p| p.parse()))
}

macro_rules! parse_row {
	($ty:ident { $($field:ident),* $(,)? }) => {
		impl Parse for $ty {
			fn parse(p: &mut Parser) -> Result<Self> {
				p.test(Expect::Nt(stringify!($ty)), |p| {
					$(let $field = p.parse()?;)*
					Ok($ty { $($field),* })
				})
			}
		}
	};
}

// Fields in print order, which is not always declaration order.
parse_row!(Collision { a, b });
parse_row!(Action {
	id,
	kind,
	target,
	u2,
	cast_time,
	recovery_time,
	cp_cost,
	flags,
	ani,
	name,
	effects
});
parse_row!(Algo { id, chance, use_limit, target_priority, cond });
parse_row!(AnimeClip { kind, a, b });
parse_row!(Break { id, value });
parse_row!(Condition { id, entries });
parse_row!(FieldFollow { a, b, c, d, e });
parse_row!(FieldMonster { a, b, c, floats });
parse_row!(Part { id, a, b });
parse_row!(Summon { kind, a, b, name });
parse_row!(WeaponAtt { slash, thrust, pierce, strike });

impl Parse for StyleName {
	fn parse(p: &mut Parser) -> Result<Self> {
		p.test(Expect::Nt("style name"), |p| {
			let a = p.parse()?;
			let b = p.parse()?;
			Ok(StyleName(a, b))
		})
	}
}

impl Parse for BookData {
	fn parse(p: &mut Parser) -> Result<Self> {
		Alt::new(p)
			.test(|p| {
				p.keyword("header")?;
				p.commit();
				p.parse().map(BookData::Header)
			})
			.test(|p| {
				p.keyword("title_page")?;
				p.commit();
				Ok(BookData::TitlePage(p.parse()?, p.parse()?))
			})
			.test(|p| {
				p.keyword("page")?;
				p.commit();
				p.parse().map(BookData::Page)
			})
			.test(|p| p.keyword("empty").map(|_| BookData::Empty))
			.finish()
	}
}

impl Parse for TitlePage {
	fn parse(p: &mut Parser) -> Result<Self> {
		let title = p.parse()?;
		let data = p.parse()?;
		Ok(TitlePage { title, data })
	}
}

impl Parse for Page {
	fn parse(p: &mut Parser) -> Result<Self> {
		let title = Alt::new(p)
			.test(|p| {
				p.keyword("title_page")?;
				p.commit();
				p.parse().map(Some)
			})
			.test(|p| p.keyword("page").map(|_| None))
			.finish()?;
		let text = p.parse()?;
		Ok(Page { title, text })
	}
}

impl Parse for Reaction {
	fn parse(p: &mut Parser) -> Result<Self> {
		p.test(Expect::Nt("reaction"), |p| {
			let id = p.parse()?;
			let kind = Alt::new(p)
				.test(|p| p.parse().map(ReactionKind::Alias))
				// PartReaction prints its own parens, so this is three consecutive
				// groups rather than one array group.
				.test(|p| Ok(ReactionKind::Parts([p.parse()?, p.parse()?, p.parse()?])))
				.finish()?;
			Ok(Reaction { id, kind })
		})
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
		p.test(Expect::Nt("btlset"), |p| {
			let field = p.parse()?;
			let bounds = p.parse()?;
			let btl_id = p.parse()?;
			let unk1 = p.parse()?;
			let bgm = p.parse()?;
			let unk2 = p.parse()?;
			let script = p.parse()?;
			let variants = rows(p)?;
			Ok(Btlset {
				field,
				bounds,
				btl_id,
				unk1,
				bgm,
				unk2,
				script,
				variants,
			})
		})
	}
}

impl Parse for Variant {
	fn parse(p: &mut Parser) -> Result<Self> {
		p.test(Expect::Nt("variant"), |p| {
			let id = p.parse()?;
			let mut monsters = Vec::new();
			while let Ok(monster) = p.parse() {
				let prob = if p.cursor.glued_punct(':').is_ok() { p.parse()? } else { 100 };
				monsters.push((monster, prob));
			}
			Ok(Variant { id, monsters })
		})
	}
}

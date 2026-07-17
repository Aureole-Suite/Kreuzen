use kreuzen::code::preload::Preload;
use kreuzen::code::shadow::{Shadow, ShadowOp};
use kreuzen::{Chunk, Function};

use super::alt::Alt;
use super::parser::{Error, Expect, Parser, Result};
use super::{PCtx, stmt, tables};

pub fn parse_chunks(p: &mut Parser, ctx: &PCtx) -> Vec<Chunk> {
	super::parse_seq(p, |p| parse_chunk(p, ctx))
}

fn parse_chunk(p: &mut Parser, ctx: &PCtx) -> Result<Chunk> {
	Alt::new(p)
		.test_kw("fn", |p| Ok(Chunk::Function(parse_function(ctx, p)?)))
		.test(|p| tables::parse_table(p, ctx).map(Chunk::Table))
		.finish()
}

fn parse_function(ctx: &PCtx, p: &mut super::alt::TryParser<'_, '_>) -> Result<Function, Error> {
	let name = p.parse()?;
	let body = stmt::block(p, ctx)?;
	let mut preload = Vec::new();
	if p.keyword("preload").is_ok() {
		preload = p.delim('{', |p| Ok(super::parse_seq(p, parse_preload)))?;
	}
	let mut shadow = Vec::new();
	while p.keyword("shadow").is_ok() {
		shadow.push(parse_shadow(p)?);
	}
	let function = Function { name, body, preload, shadow };
	Ok(function)
}

fn parse_preload(p: &mut Parser) -> Result<Preload> {
	let span = p.next_span();
	let name = p.ident()?;
	if !matches!(
		name,
		"Call" | "PkgLoad" | "EffLoad" | "SoundPlay" | "SoundPlayVoice" | "Voice" | "CharAniclipPlay" | "NameplateShow" | "opCE02"
	) {
		p.errors.error(format!("unknown preload '{name}'"), span);
		return Err(Error);
	}

	p.delim('(', |p| {
		Ok(match name {
			"Call" => {
				let n = p.parse()?;
				p.punct(',')?;
				Preload::Call(n, p.parse()?)
			}
			"PkgLoad" => Preload::PkgLoad(p.parse()?),
			"EffLoad" => Preload::EffLoad(p.parse()?),
			"SoundPlay" => Preload::SoundPlay(p.parse()?),
			"SoundPlayVoice" => Preload::SoundPlayVoice(p.parse()?),
			"Voice" => Preload::Voice(p.parse()?),
			"CharAniclipPlay" => {
				let chr = p.parse()?;
				p.punct(',')?;
				Preload::CharAniclipPlay(chr, p.parse()?)
			}
			"NameplateShow" => Preload::NameplateShow(p.parse()?),
			"opCE02" => Preload::opCE02(p.parse()?),
			_ => unreachable!(),
		})
	})
}

fn parse_shadow(p: &mut Parser) -> Result<Shadow> {
	let line = p
		.test(Expect::Nt("line"), |p| {
			let line = p.cursor.int()?;
			p.cursor.glued_punct('@')?;
			u16::try_from(line).map_err(|_| Error)
		})
		.unwrap_or(0);
	let ops = parse_shadow_ops(p)?;
	Ok(Shadow { line, ops })
}

fn parse_shadow_ops(p: &mut Parser) -> Result<Vec<ShadowOp>, Error> {
	p.delim('{', |p| Ok(super::parse_seq(p, parse_shadow_op)))
}

fn parse_shadow_op(p: &mut Parser) -> Result<ShadowOp> {
	Alt::new(p)
		.test_kw("Call", |p| {
			let table = p.parse()?;
			let name = p.parse()?;
			Ok(ShadowOp::Call { table, name })
		})
		.test_kw("CharAni", |p| {
			let chr = p.parse()?;
			let strings = p.parse()?;
			Ok(ShadowOp::CharAni { chr, strings })
		})
		.test_kw("Fork", |p| {
			let chr = p.parse()?;
			let slot = p.parse()?;
			let name = p.parse()?;
			let flags = p.parse()?;
			Ok(ShadowOp::Fork { chr, slot, name, flags })
		})
		.test_kw("ForkLambda", |p| {
			let chr = p.parse()?;
			let slot = p.parse()?;
			let name = p.parse()?;
			let ops = parse_shadow_ops(p)?;
			Ok(ShadowOp::ForkLambda { chr, slot, name, ops })
		})
		.finish()
}

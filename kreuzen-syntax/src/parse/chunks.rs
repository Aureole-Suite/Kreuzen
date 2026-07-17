use kreuzen::code::preload::Preload;
use kreuzen::code::shadow::{Shadow, ShadowOp};
use kreuzen::{Chunk, Function};

use crate::Parse;

use super::parser::{Error, Expect, Parser, Result};
use super::{PCtx, stmt};

pub fn parse_chunks(p: &mut Parser, ctx: &PCtx) -> Vec<Chunk> {
	super::parse_seq(p, |p| parse_chunk(p, ctx))
}

fn parse_chunk(p: &mut Parser, ctx: &PCtx) -> Result<Chunk> {
	p.alt()
		.test_kw("fn", |p| Ok(Chunk::Function(parse_function(ctx, p)?)))
		.test(|p| p.parse().map(Chunk::Table))
		.finish()
}

fn parse_function(ctx: &PCtx, p: &mut super::alt::TryParser<'_, '_>) -> Result<Function, Error> {
	let name = p.parse()?;
	let body = stmt::block(p, ctx)?;
	let mut preload = Vec::new();
	if p.keyword("preload").is_ok() {
		preload = p.delim('{', |p| Ok(super::parse_seq(p, |p| p.parse())))?;
	}
	let mut shadow = Vec::new();
	while p.keyword("shadow").is_ok() {
		shadow.push(p.parse()?);
	}
	Ok(Function { name, body, preload, shadow })
}

impl Parse for Preload {
	fn parse(p: &mut Parser) -> Result<Self> {
		p.alt()
			.test_kw("Call", |p| Ok(Preload::Call(p.parse()?, p.parse()?)))
			.test_kw("PkgLoad", |p| p.parse().map(Preload::PkgLoad))
			.test_kw("EffLoad", |p| p.parse().map(Preload::EffLoad))
			.test_kw("SoundPlay", |p| p.parse().map(Preload::SoundPlay))
			.test_kw("SoundPlayVoice", |p| p.parse().map(Preload::SoundPlayVoice))
			.test_kw("Voice", |p| p.parse().map(Preload::Voice))
			.test_kw("CharAniclipPlay", |p| Ok(Preload::CharAniclipPlay(p.parse()?, p.parse()?)))
			.test_kw("NameplateShow", |p| p.parse().map(Preload::NameplateShow))
			.test_kw("opCE02", |p| p.parse().map(Preload::opCE02))
			.finish()
	}
}

impl Parse for Shadow {
	fn parse(p: &mut Parser) -> Result<Self> {
		let line = p
			.test(Expect::Nt("line"), |p| {
				let meta = p.cursor.meta()?;
				if meta.width != 0 {
					return Err(Error);
				}
				Ok(meta.line)
			})
			.unwrap_or(0);
		let ops = parse_shadow_ops(p)?;
		Ok(Shadow { line, ops })
	}
}

fn parse_shadow_ops(p: &mut Parser) -> Result<Vec<ShadowOp>, Error> {
	p.delim('{', |p| Ok(super::parse_seq(p, |p| p.parse())))
}

impl Parse for ShadowOp {
	fn parse(p: &mut Parser) -> Result<Self> {
		p.alt()
			.test_kw("Call", |p| {
				let table = p.parse()?;
				let name = p.parse()?;
				Ok(ShadowOp::Call { table, name })
			})
			.test_kw("CharAni", |p| {
				let chr = p.parse()?;
				let strings = p.parse_many()?;
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
}

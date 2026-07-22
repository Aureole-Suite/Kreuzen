use kreuzen::{Body, Chunk, Function};

use crate::{Error, PCtx, Parser, Result};

pub fn parse_chunk(p: &mut Parser, ctx: &PCtx) -> Result<Chunk> {
	p.alt()
		.test_kw("fn", |p| Ok(Chunk::Function(parse_function(ctx, p)?)))
		.test(|p| p.parse().map(Chunk::Table))
		.finish()
}

fn parse_function(ctx: &PCtx, p: &mut Parser) -> Result<Function, Error> {
	let name = p.parse()?;
	let body = Body::Tree(crate::code::block(p, ctx)?);
	let mut preload = Vec::new();
	if p.keyword("preload").is_ok() {
		preload = p.parse()?;
	}
	let mut shadow = Vec::new();
	while p.keyword("shadow").is_ok() {
		shadow.push(p.parse()?);
	}
	Ok(Function { name, body, preload, shadow })
}

use kreuzen::code::preload::Preload;
use kreuzen::code::shadow::{Shadow, ShadowOp};
use kreuzen::{Chunk, Function};

use crate::types::block;
use crate::{Parse, Parser, Print, Printer, Result};

use super::parser::{Error, Expect};
use super::{PCtx, stmt};

pub fn parse_chunk(p: &mut Parser, ctx: &PCtx) -> Result<Chunk> {
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
		preload = p.parse()?;
	}
	let mut shadow = Vec::new();
	while p.keyword("shadow").is_ok() {
		shadow.push(p.parse()?);
	}
	Ok(Function { name, body, preload, shadow })
}

block!(Preload);
crate::types::row!(
	enum Preload {
		Call(a, b),
		PkgLoad(a),
		EffLoad(a),
		SoundPlay(a),
		SoundPlayVoice(a),
		Voice(a),
		CharAniclipPlay(a, b),
		NameplateShow(a),
		opCE02(a),
	}
);

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
		let ops = p.parse()?;
		Ok(Shadow { line, ops })
	}
}

block!(ShadowOp);
crate::types::row!(
	enum ShadowOp {
		Call { table, name },
		CharAni { chr, strings* },
		Fork { chr, slot, name, flags },
		ForkLambda { chr, slot, name, ops },
	}
);

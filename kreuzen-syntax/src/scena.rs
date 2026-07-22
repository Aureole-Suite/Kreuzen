use kreuzen::code::FlatOp;
use kreuzen::code::preload::Preload;
use kreuzen::code::shadow::{Shadow, ShadowOp};
use kreuzen::{Body, Chunk, Function, Game, Scena, ScenaInfo};

use crate::parse::{Expect, PCtx};
use crate::{Error, Parse, Parser, Print, Printer, Result};

impl Print for ScenaInfo {
	fn print(&self, ctx: &mut Printer) {
		ctx.word("scena");
		self.name.print(ctx);
		match self.enc {
			kreuzen::Enc::Sjis => ctx.word("sjis"),
			kreuzen::Enc::Utf8 => {}
		}
		self.game.print(ctx);
		if self.variant != 0 {
			ctx.sym("/");
			self.variant.print(ctx);
		}
		if self.oddness != 0 {
			self.oddness.print(ctx);
		}
	}
}

crate::types::row!(
	enum Game {
		Cs1 = "cs1",
		Cs2 = "cs2",
		Cs3 = "cs3",
		Cs4 = "cs4",
		Reverie = "reverie",
		Tx = "tx",
	}
);

impl Print for Body {
	fn print(&self, ctx: &mut Printer) {
		match self {
			Body::Flat(ops) => {
				ctx.word("raw");
				ctx.block(ops, FlatOp::print);
			}
			Body::Tree(stmts) => stmts.print(ctx),
		}
	}
}

impl Print for Function {
	fn print(&self, ctx: &mut Printer) {
		ctx.word("fn");
		self.name.print(ctx);
		self.body.print(ctx);
		if !self.preload.is_empty() {
			ctx.word("preload");
			self.preload.print(ctx);
		}
		for shadow in &self.shadow {
			ctx.word("shadow");
			shadow.print(ctx);
		}
	}
}

pub fn parse_function(ctx: &PCtx, p: &mut Parser) -> Result<Function, Error> {
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

impl Print for Chunk {
	fn print(&self, ctx: &mut Printer) {
		match self {
			Chunk::Function(f) => f.print(ctx),
			Chunk::Table(t) => t.print(ctx),
		}
	}
}

pub fn parse_chunk(p: &mut Parser, ctx: &PCtx) -> Result<Chunk> {
	p.alt()
		.test_kw("fn", |p| Ok(Chunk::Function(parse_function(ctx, p)?)))
		.test(|p| p.parse().map(Chunk::Table))
		.finish()
}

impl Print for Scena {
	fn print(&self, ctx: &mut Printer) {
		self.info.print(ctx);
		ctx.end_item();
		ctx.newline(1);
		for c in &self.chunks {
			c.print(ctx);
			ctx.end_item();
			ctx.newline(1);
		}
	}
}

crate::types::block!(Preload);
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

crate::types::block!(ShadowOp);
crate::types::row!(
	enum ShadowOp {
		Call { table, name },
		CharAni { chr, strings* },
		Fork { chr, slot, name, flags },
		ForkLambda { chr, slot, name, ops },
	}
);

impl Print for Shadow {
	fn print(&self, ctx: &mut Printer) {
		if self.line != 0 {
			ctx.token(format!("{}", self.line));
			ctx.sym("@");
		}
		self.ops.print(ctx);
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
		let ops = p.parse()?;
		Ok(Shadow { line, ops })
	}
}

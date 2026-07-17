use kreuzen::code::FlatOp;
use kreuzen::code::preload::Preload;
use kreuzen::code::shadow::{Shadow, ShadowOp};
use kreuzen::{Body, Chunk, Function, Scena, ScenaInfo};

use crate::parse::Expect;
use crate::{Error, Parse, Parser, Print, Printer, Result};

impl Print for ScenaInfo {
	fn print(&self, ctx: &mut Printer) {
		ctx.word("scena");
		self.name.print(ctx);
		ctx.token(format!(
			"game={:?} enc={:?} oddness={} variant={}",
			self.game, self.enc, self.oddness, self.variant
		));
	}
}

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

impl Print for Chunk {
	fn print(&self, ctx: &mut Printer) {
		match self {
			Chunk::Function(f) => f.print(ctx),
			Chunk::Table(t) => t.print(ctx),
		}
	}
}

impl Print for Scena {
	fn print(&self, ctx: &mut Printer) {
		self.info.print(ctx);
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

mod code;
mod ctx;
mod tables;
mod types;

pub use ctx::Ctx;

use kreuzen::code::FlatOp;
use kreuzen::code::preload::Preload;
use kreuzen::{Chunk, Function, RawChunk, RawFunction, RawScena, Scena, ScenaInfo};

pub trait Print {
	fn print(&self, ctx: &mut Ctx);
	fn print_to_string(&self) -> String {
		let mut ctx = Ctx::new();
		self.print(&mut ctx);
		ctx.finish()
	}
}

impl Print for ScenaInfo {
	fn print(&self, ctx: &mut Ctx) {
		ctx.token(format!(
			"scena {} game={:?} enc={:?} oddness={} variant={}",
			self.name, self.game, self.enc, self.oddness, self.variant
		));
	}
}

impl Print for RawFunction {
	fn print(&self, ctx: &mut Ctx) {
		ctx.word("fn");
		ctx.token(self.name.to_owned());
		ctx.block(&self.body, FlatOp::print);
		if !self.preload.is_empty() {
			ctx.word("preload");
			ctx.block(&self.preload, Preload::print);
		}
		for shadow in &self.shadow {
			ctx.word("shadow");
			shadow.print(ctx);
		}
	}
}

impl Print for RawChunk {
	fn print(&self, ctx: &mut Ctx) {
		match self {
			RawChunk::Function { function } => function.print(ctx),
			RawChunk::Table { name, table, shadow } => {
				ctx.token(name.to_owned());
				if *shadow {
					ctx.word("shadow");
				}
				table.print(ctx);
			}
		}
	}
}

impl Print for RawScena {
	fn print(&self, ctx: &mut Ctx) {
		ctx.word("raw");
		self.info.print(ctx);
		ctx.newline(1);
		for c in &self.chunks {
			c.print(ctx);
			ctx.newline(1);
		}
	}
}

impl Print for Function {
	fn print(&self, ctx: &mut Ctx) {
		ctx.word("fn");
		ctx.token(self.name.to_owned());
		self.body.print(ctx);
		if !self.preload.is_empty() {
			ctx.word("preload");
			ctx.block(&self.preload, Preload::print);
		}
		for shadow in &self.shadow {
			ctx.word("shadow");
			shadow.print(ctx);
		}
	}
}

impl Print for Chunk {
	fn print(&self, ctx: &mut Ctx) {
		match self {
			Chunk::Function(f) => f.print(ctx),
			Chunk::Table { name, table, shadow } => {
				ctx.token(name.to_owned());
				if *shadow {
					ctx.word("shadow");
				}
				table.print(ctx);
			}
		}
	}
}

impl Print for Scena {
	fn print(&self, ctx: &mut Ctx) {
		self.info.print(ctx);
		ctx.newline(1);
		for c in &self.chunks {
			c.print(ctx);
			ctx.newline(1);
		}
	}
}

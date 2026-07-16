mod code;
mod ctx;
pub mod diag;
pub mod lex;
mod parse;
mod tables;
mod types;

pub use ctx::Ctx;
pub use parse::{Rest, parse, parse_header, parse_scena};

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
		ctx.word("scena");
		types::print_name(&self.name, ctx);
		ctx.token(format!(
			"game={:?} enc={:?} oddness={} variant={}",
			self.game, self.enc, self.oddness, self.variant
		));
	}
}

impl Print for RawFunction {
	fn print(&self, ctx: &mut Ctx) {
		ctx.word("fn");
		types::print_name(&self.name, ctx);
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
			RawChunk::Function(f) => f.print(ctx),
			RawChunk::Table(t) => t.print(ctx),
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
		types::print_name(&self.name, ctx);
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
			Chunk::Table(t) => t.print(ctx),
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

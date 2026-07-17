mod code;
pub mod diag;
pub mod lex;
mod parse;
mod printer;
mod tables;
mod text;
mod types;

pub use parse::{Error, Parser, Rest, Result, parse, parse_header, parse_scena};
pub use printer::Printer;

use kreuzen::code::FlatOp;
use kreuzen::code::preload::Preload;
use kreuzen::{Chunk, Function, RawChunk, RawFunction, RawScena, Scena, ScenaInfo};

pub trait Print {
	fn print(&self, ctx: &mut Printer);
	fn print_to_string(&self) -> String {
		let mut ctx = Printer::new();
		self.print(&mut ctx);
		ctx.finish()
	}
}

/// Context-free values, mirroring the `Print` impls. Values that need the op
/// spec (ops, exprs, statements) are parsed by functions instead.
///
/// A failed parse may leave the cursor mid-value; callers that want to try
/// alternatives must go through `Alt` (or `Parser::test`), which restores it.
pub trait Parse: Sized {
	fn parse(p: &mut Parser) -> Result<Self>;
}

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

impl Print for RawFunction {
	fn print(&self, ctx: &mut Printer) {
		ctx.word("fn");
		self.name.print(ctx);
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
	fn print(&self, ctx: &mut Printer) {
		match self {
			RawChunk::Function(f) => f.print(ctx),
			RawChunk::Table(t) => t.print(ctx),
		}
	}
}

impl Print for RawScena {
	fn print(&self, ctx: &mut Printer) {
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
	fn print(&self, ctx: &mut Printer) {
		ctx.word("fn");
		self.name.print(ctx);
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
			ctx.newline(1);
		}
	}
}

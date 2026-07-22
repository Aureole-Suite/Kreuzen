pub mod diag;
pub mod lex;
mod parse;
mod print;

mod code;
mod scena;
mod tables;
mod text;
mod types;

pub use parse::{Error, Parser, Rest, Result, parse, parse_header, parse_scena};
pub use print::Printer;

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

mod alt;
mod parser;

use kreuzen::spec::Spec;
use kreuzen::{Enc, Scena, ScenaInfo};

use crate::diag::{Errors, Severity};
use crate::lex::{Cursor, Tokens};
pub use alt::TryParser;
pub use parser::{Error, Expect, Parser, Result};

/// Context available while parsing everything after the header.
#[derive(Clone, Copy)]
pub(crate) struct PCtx {
	pub spec: &'static Spec,
	pub can_break: bool,
	pub can_cont: bool,
}

/// Parses a `{}` block containing a statement-like sequence: items are
/// `;`-terminated unless they end with a `}` block, and a failed item skips
/// ahead and continues with the next one.
pub(crate) fn parse_block<T>(p: &mut Parser, mut f: impl FnMut(&mut Parser) -> Result<T>) -> Result<Vec<T>> {
	p.delim('{', |p| {
		let mut out = Vec::new();
		while !p.at_end() {
			parse_item(p, &mut out, &mut f);
		}
		Ok(out)
	})
}

/// A single item of a statement-like sequence; see [`parse_block`].
pub(crate) fn parse_item<T>(p: &mut Parser, out: &mut Vec<T>, f: impl FnOnce(&mut Parser) -> Result<T>) {
	let ok = match f(p) {
		Ok(v) => {
			out.push(v);
			p.cursor.prev_punct('}') || p.punct(';').is_ok()
		}
		Err(_) => false,
	};
	if !ok {
		p.report(recover);
	}
}

/// Skips to just past the next `;` or `{...}` block, which is hopefully the end of the item.
pub(crate) fn recover(c: &mut Cursor) {
	while !c.at_end() {
		if c.punct(';').is_ok() || c.delim('{').is_ok() {
			break;
		}
		c.skip_any();
	}
}

/// The part of a file after the header, to be parsed with [`parse_scena`]
/// once the caller has picked an op table based on the header.
pub struct Rest<'a> {
	cursor: Cursor<'a>,
}

/// Phase 1: parses the `scena` header line.
pub fn parse_header<'a>(tokens: &'a Tokens, errors: &mut Errors) -> Option<(ScenaInfo, Rest<'a>)> {
	let mut p = Parser::new(tokens.cursor(), errors);
	match parse_header_inner(&mut p) {
		Ok(info) => Some((info, Rest { cursor: p.cursor })),
		Err(_) => {
			p.report(|_| {});
			None
		}
	}
}

fn parse_header_inner(p: &mut Parser) -> Result<ScenaInfo> {
	p.keyword("scena")?;
	let name = p.parse()?;

	let enc = p.alt().test_kw("sjis", |_| Ok(Enc::Sjis)).test(|_| Ok(Enc::Utf8)).finish()?;
	let game = p.parse()?;
	let variant = if p.glued_punct('/').is_ok() { p.parse()? } else { 0 };
	let oddness = p.parse().unwrap_or(0);
	p.punct(';')?;

	Ok(ScenaInfo { name, game, enc, oddness, variant })
}

/// Phase 2: parses the rest of the file, using the given op table.
///
/// The spec must be `'static` because op names are borrowed from it;
/// get it from `kreuzen::spec::for_game`.
pub fn parse_scena(info: ScenaInfo, rest: Rest<'_>, spec: &'static Spec, errors: &mut Errors) -> Scena {
	let mut p = Parser::new(rest.cursor, errors);
	let ctx = PCtx {
		spec,
		can_break: false,
		can_cont: false,
	};
	let mut chunks = Vec::new();
	while !p.at_end() {
		parse_item(&mut p, &mut chunks, |p| crate::scena::parse_chunk(p, &ctx));
	}
	Scena { info, chunks }
}

/// Convenience wrapper over both phases:
/// `parse(src, |info| spec::for_game(info.game, info.variant), &mut errors)`.
pub fn parse(src: &str, spec: impl FnOnce(&ScenaInfo) -> &'static Spec, errors: &mut Errors) -> Option<Scena> {
	let tokens = crate::lex::lex(src, errors);
	if errors.max_severity() >= Severity::Fatal {
		return None;
	}
	let (info, rest) = parse_header(&tokens, errors)?;
	let spec = spec(&info);
	Some(parse_scena(info, rest, spec, errors))
}

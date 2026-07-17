mod alt;
mod chunks;
mod expr;
mod op;
mod parser;
mod stmt;
mod tables;
mod text;
mod types;

use kreuzen::spec::Spec;
use kreuzen::{Enc, Game, Scena, ScenaInfo};

use crate::diag::{Errors, Severity};
use crate::lex::{Cursor, Tokens};
use alt::Alt;
use parser::{Parser, Result};

/// Context available while parsing everything after the header.
pub(crate) struct PCtx {
	pub spec: &'static Spec,
	pub game: Game,
}

/// Parses a statement-like sequence until the end of the cursor: items are
/// `;`-terminated unless they end with a `}` block, and a failed item skips
/// ahead and continues with the next one.
pub(crate) fn parse_seq<T>(p: &mut Parser, mut f: impl FnMut(&mut Parser) -> Result<T>) -> Vec<T> {
	let mut out = Vec::new();
	while !p.at_end() {
		seq_item(p, &mut out, &mut f);
	}
	out
}

/// A single item of a statement-like sequence; see [`parse_seq`].
pub(crate) fn seq_item<T>(p: &mut Parser, out: &mut Vec<T>, f: impl FnOnce(&mut Parser) -> Result<T>) {
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
	is_raw: bool,
}

/// Phase 1: parses the `scena <name> game=.. enc=.. oddness=.. variant=..` header line.
pub fn parse_header<'a>(tokens: &'a Tokens, errors: &mut Errors) -> Option<(ScenaInfo, Rest<'a>)> {
	let mut p = Parser::new(tokens.cursor(), errors);
	match parse_header_inner(&mut p) {
		Ok((info, is_raw)) => Some((info, Rest { cursor: p.cursor, is_raw })),
		Err(_) => {
			p.report(|_| {});
			None
		}
	}
}

fn parse_header_inner(p: &mut Parser) -> Result<(ScenaInfo, bool)> {
	let is_raw = p.keyword("raw").is_ok();
	p.keyword("scena")?;
	let name = p.parse()?;

	p.keyword("game")?;
	p.punct('=')?;
	let game = Alt::new(p)
		.test_kw("Cs1", |_| Ok(Game::Cs1))
		.test_kw("Cs2", |_| Ok(Game::Cs2))
		.test_kw("Cs3", |_| Ok(Game::Cs3))
		.test_kw("Cs4", |_| Ok(Game::Cs4))
		.test_kw("Reverie", |_| Ok(Game::Reverie))
		.test_kw("Tx", |_| Ok(Game::Tx))
		.finish()?;

	p.keyword("enc")?;
	p.punct('=')?;
	let enc = Alt::new(p)
		.test_kw("Sjis", |_| Ok(Enc::Sjis))
		.test_kw("Utf8", |_| Ok(Enc::Utf8))
		.finish()?;

	p.keyword("oddness")?;
	p.punct('=')?;
	let oddness = p.parse()?;

	p.keyword("variant")?;
	p.punct('=')?;
	let variant = p.parse()?;

	Ok((ScenaInfo { name, game, enc, oddness, variant }, is_raw))
}

/// Phase 2: parses the rest of the file, using the given op table.
///
/// The spec must be `'static` because op names are borrowed from it;
/// get it from `kreuzen::spec::for_game`.
pub fn parse_scena(info: ScenaInfo, rest: Rest<'_>, spec: &'static Spec, errors: &mut Errors) -> Scena {
	let mut p = Parser::new(rest.cursor, errors);
	if rest.is_raw {
		let span = p.next_span();
		p.errors.fatal("raw files are not supported yet", span);
		return Scena { info, chunks: Vec::new() };
	}
	let ctx = PCtx { spec, game: info.game };
	let chunks = chunks::parse_chunks(&mut p, &ctx);
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

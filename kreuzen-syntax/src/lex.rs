use kreuzen::code::OpMeta;

use super::diag::Errors;

mod cursor;
pub use cursor::{Cursor, Error as CursorError};

#[derive(Debug, Clone)]
pub struct Tokens(Vec<RawToken>);

#[derive(Clone)]
struct RawToken {
	pub start: u32,
	pub end: u32,
	pub token: TokenKind,
	pub matched: u32,
}

impl RawToken {
	pub fn span(&self) -> std::ops::Range<usize> {
		self.start as usize..self.end as usize
	}
}

impl std::fmt::Debug for RawToken {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.write_fmt(format_args!("{}..{}:", self.start, self.end))?;
		self.token.fmt(f)?;
		if self.matched > 0 {
			f.write_fmt(format_args!("~{}", self.matched))?;
		}
		Ok(())
	}
}

#[derive(Debug, Clone)]
pub enum TokenKind {
	Ident(Box<str>),
	String(Box<str>),
	/// Raw contents between `"""` and `"""`, with escapes and `{...}` controls unprocessed.
	TextBlock(Box<str>),
	Int(i64),
	Float(f32),
	/// `N@`, `N@~`, `N@M~`, or `N~` markers. A bare `~` is never a meta:
	/// widths only exist in Reverie, where every op has a line number.
	Meta(OpMeta),
	Punct(char),
}

pub fn lex(src: &str, errors: &mut Errors) -> Tokens {
	let mut lexer = Lex { src, pos: 0, errors };
	lexer.skip_whitespace();
	let mut tokens = Vec::new();
	let dummy = RawToken {
		start: 0,
		end: 0,
		token: TokenKind::Punct('\0'),
		matched: 0,
	};
	tokens.push(dummy.clone());
	while let Some(token) = lexer.lex() {
		tokens.push(token);
	}
	tokens.push(RawToken {
		start: lexer.pos as u32,
		end: lexer.pos as u32,
		..dummy
	});

	match_delims(&mut tokens, errors);

	Tokens(tokens)
}

fn match_delims(tokens: &mut [RawToken], errors: &mut Errors) {
	let mut stack = Vec::new();
	for (i, token) in tokens.iter_mut().enumerate() {
		match token.token {
			TokenKind::Punct(o @ ('(' | '[' | '{')) => stack.push((i, o, token)),
			TokenKind::Punct(c @ (')' | ']' | '}')) => {
				let open_delim = match c {
					')' => '(',
					']' => '[',
					'}' => '{',
					_ => unreachable!(),
				};
				if let Some((j, o, open)) = stack.pop() {
					if o != open_delim {
						errors.fatal("mismatched delimiter", token.span()).note("doesn't match", open.span());
					}
					let diff = (i - j) as u32;
					open.matched = diff;
					token.matched = diff;
				} else {
					errors.fatal("unmatched delimiter", token.span());
				}
			}
			_ => {}
		}
	}
	for (_, _, open) in stack {
		errors.fatal("unclosed delimiter", open.span());
	}
}

struct Lex<'a> {
	src: &'a str,
	pos: usize,
	errors: &'a mut Errors,
}

impl<'a> Lex<'a> {
	fn peek_char(&self) -> Option<char> {
		self.src[self.pos..].chars().next()
	}

	fn next_char(&mut self) -> Option<char> {
		let c = self.peek_char();
		self.pos += c.map_or(0, |c| c.len_utf8());
		c
	}

	fn consume_if(&mut self, f: impl FnOnce(char) -> bool) -> bool {
		let c = self.peek_char();
		if c.is_some_and(f) {
			self.next_char();
			true
		} else {
			false
		}
	}

	fn consume(&mut self, s: &str) -> bool {
		if self.src[self.pos..].starts_with(s) {
			self.pos += s.len();
			true
		} else {
			false
		}
	}

	fn skip_whitespace(&mut self) {
		loop {
			if self.consume(" ") || self.consume("\t") || self.consume("\n") || self.consume("\r") {
				continue;
			}
			if self.consume("#") {
				while self.peek_char().is_some_and(|c| c != '\n') {
					self.next_char();
				}
				continue;
			}
			break;
		}
	}

	fn lex(&mut self) -> Option<RawToken> {
		let start = self.pos;
		let token = self.lex_token()?;
		let end = self.pos;
		self.skip_whitespace();
		Some(RawToken {
			start: start as u32,
			end: end as u32,
			token,
			matched: 0,
		})
	}

	fn lex_token(&mut self) -> Option<TokenKind> {
		let start = self.pos;

		if self.consume_if(unicode_ident::is_xid_start) {
			while self.consume_if(unicode_ident::is_xid_continue) {}
			return Some(TokenKind::Ident(self.src[start..self.pos].into()));
		}

		if self.consume("0x") {
			let numstart = self.pos;
			while self.consume_if(|c| c.is_ascii_hexdigit()) {}
			match u64::from_str_radix(&self.src[numstart..self.pos], 16) {
				Ok(n) => return Some(TokenKind::Int(n as i64)),
				Err(_) => {
					self.errors.error("invalid hex literal", start..self.pos);
					return Some(TokenKind::Int(0));
				}
			}
		}

		if matches!(self.peek_char(), Some('-' | '0'..='9')) {
			let neg = self.consume_if(|c| c == '-');
			let digitstart = self.pos;
			while self.consume_if(|c| c.is_ascii_digit()) {}

			if self.pos == digitstart {
				// If no digits, it's just a minus sign
				return Some(TokenKind::Punct('-'));
			}
			let digitend = self.pos;

			let mut float = false;
			if self.consume(".") {
				float = true;
				while self.consume_if(|c| c.is_ascii_digit()) {}
			}
			// f32's Debug can use exponent notation (1e8, 6e-6)
			let expstart = self.pos;
			if self.consume("e") || self.consume("E") {
				self.consume_if(|c| c == '+' || c == '-');
				let expdigits = self.pos;
				while self.consume_if(|c| c.is_ascii_digit()) {}
				if self.pos == expdigits {
					self.pos = expstart; // not an exponent, leave for the next token
				} else {
					float = true;
				}
			}

			if float {
				match self.src[start..self.pos].parse() {
					Ok(f) => return Some(TokenKind::Float(f)),
					Err(_) => {
						self.errors.error("invalid float literal", start..self.pos);
						return Some(TokenKind::Float(0.0));
					}
				}
			}

			if !neg {
				if self.consume("@") {
					let line = self.parse_num(digitstart..digitend, "line number");
					let width = self.lex_width().unwrap_or(0);
					return Some(TokenKind::Meta(OpMeta { line, width }));
				}
				if self.consume("~") {
					let width = self.parse_num(digitstart..digitend, "width");
					return Some(TokenKind::Meta(OpMeta { line: 0, width }));
				}
			}

			match self.src[start..self.pos].parse() {
				Ok(n) => return Some(TokenKind::Int(n)),
				Err(_) => {
					self.errors.error("invalid int literal", start..self.pos);
					return Some(TokenKind::Int(0));
				}
			}
		}

		if self.consume("\"\"\"") {
			return Some(self.lex_text_block(start));
		}

		if self.consume("\"") {
			return Some(TokenKind::String(self.lex_string(start)));
		}

		if let Some(c) = self.next_char() {
			return Some(TokenKind::Punct(c));
		}

		None
	}

	/// The width part after a `N@` line marker: `M~`, `~`, or nothing.
	fn lex_width(&mut self) -> Option<u8> {
		let start = self.pos;
		while self.consume_if(|c| c.is_ascii_digit()) {}
		if self.pos != start {
			let end = self.pos;
			if self.consume("~") {
				return Some(self.parse_num(start..end, "width"));
			}
			self.pos = start; // the digits belong to the next token
			return None;
		}
		if self.consume("~") {
			return Some(1);
		}
		None
	}

	fn parse_num<T: std::str::FromStr + Default>(&mut self, range: std::ops::Range<usize>, what: &str) -> T {
		match self.src[range.clone()].parse() {
			Ok(v) => v,
			Err(_) => {
				self.errors.error(format!("invalid {what}"), range);
				T::default()
			}
		}
	}

	// Contents never contain a raw '"' (the printer escapes them), so we can
	// simply scan for the closing delimiter and defer everything else.
	fn lex_text_block(&mut self, start: usize) -> TokenKind {
		let content_start = self.pos;
		loop {
			match self.next_char() {
				Some('"') => {
					let content = &self.src[content_start..self.pos - 1];
					if !self.consume("\"\"") {
						self.errors.error("stray '\"' in text block", self.pos - 1..self.pos);
						continue;
					}
					return TokenKind::TextBlock(content.into());
				}
				Some(_) => {}
				None => {
					self.errors.fatal("unterminated text block", start..start + 3);
					return TokenKind::TextBlock(self.src[content_start..self.pos].into());
				}
			}
		}
	}

	fn lex_string(&mut self, start: usize) -> Box<str> {
		let mut s = String::new();
		loop {
			let escstart = self.pos;
			match self.next_char() {
				Some('"') => break,
				Some('\\') => {
					if let Some(c) = lex_escape(self.src, &mut self.pos) {
						s.push(c);
					} else {
						self.errors.error("invalid escape sequence", escstart..self.pos);
					}
				}
				None | Some('\n') => {
					self.errors.error("unterminated string", start..self.pos - 1);
					break;
				}
				Some(c) => s.push(c),
			}
		}
		s.into_boxed_str()
	}
}

/// Parses one escape sequence (after the `\`) at `*pos`, advancing it.
/// Shared with text-block parsing, which processes escapes at parse time.
pub(crate) fn lex_escape(src: &str, pos: &mut usize) -> Option<char> {
	let mut chars = src[*pos..].chars();
	let mut next = || {
		let c = chars.next();
		*pos += c.map_or(0, |c| c.len_utf8());
		c
	};
	match next()? {
		'"' => Some('"'),
		'\\' => Some('\\'),
		'n' => Some('\n'),
		'r' => Some('\r'),
		't' => Some('\t'),
		'{' => Some('{'),
		'}' => Some('}'),
		'x' => {
			let a = next()?.to_digit(16)?;
			let b = next()?.to_digit(16)?;
			char::from_u32(a * 16 + b)
		}
		'u' => {
			if next()? != '{' {
				return None;
			}
			let mut v: u32 = 0;
			loop {
				match next()? {
					'}' => break,
					c => v = v.checked_mul(16)?.checked_add(c.to_digit(16)?)?,
				}
			}
			char::from_u32(v)
		}
		_ => None,
	}
}

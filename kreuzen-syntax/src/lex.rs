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
	Int(i64),
	Float(f32),
	/// `N@`, `N@~`, `N@M~`, or `N~` markers. A bare `~` is never a meta:
	/// widths only exist in Reverie, where every op has a line number.
	Meta(OpMeta),
	Punct(char),
}

pub fn lex(src: &str, errors: &mut Errors) -> Tokens {
	let mut lexer = Lex { src, pos: 0, errors, tokens: Vec::new() };
	let dummy = RawToken {
		start: 0,
		end: 0,
		token: TokenKind::Punct('\0'),
		matched: 0,
	};
	lexer.tokens.push(dummy.clone());
	lexer.run();
	lexer.tokens.push(RawToken {
		start: lexer.pos as u32,
		end: lexer.pos as u32,
		..dummy
	});

	let mut tokens = lexer.tokens;
	match_delims(&mut tokens, errors);

	Tokens(tokens)
}

fn match_delims(tokens: &mut [RawToken], errors: &mut Errors) {
	let mut stack = Vec::new();
	for (i, token) in tokens.iter_mut().enumerate() {
		let open_delim = match token.token {
			TokenKind::Punct(o @ ('(' | '[' | '{')) => {
				stack.push((i, o, token));
				continue;
			}
			// `"""` doesn't nest, so a quote is a closer iff one is already open
			TokenKind::Punct('"') if !matches!(stack.last(), Some((_, '"', _))) => {
				stack.push((i, '"', token));
				continue;
			}
			TokenKind::Punct(')') => '(',
			TokenKind::Punct(']') => '[',
			TokenKind::Punct('}') => '{',
			TokenKind::Punct('"') => '"',
			_ => continue,
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
	for (_, _, open) in stack {
		errors.fatal("unclosed delimiter", open.span());
	}
}

struct Lex<'a> {
	src: &'a str,
	pos: usize,
	errors: &'a mut Errors,
	tokens: Vec<RawToken>,
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

	fn push(&mut self, start: usize, token: TokenKind) {
		self.tokens.push(RawToken {
			start: start as u32,
			end: self.pos as u32,
			token,
			matched: 0,
		});
	}

	fn run(&mut self) {
		self.skip_whitespace();
		loop {
			let start = self.pos;
			if self.consume("\"\"\"") {
				self.push(start, TokenKind::Punct('"'));
				self.lex_text();
			} else if let Some(token) = self.lex_token() {
				self.push(start, token);
			} else {
				break;
			}
			self.skip_whitespace();
		}
	}

	fn lex_token(&mut self) -> Option<TokenKind> {
		let start = self.pos;

		if self.consume_if(|c| unicode_ident::is_xid_start(c) || c == '_') {
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

	/// The contents of a text block, after the opening `"""`. Leading
	/// whitespace on each line is indentation (a leading space is written
	/// `\ `), an escaped newline is a continuation, and `{}` groups contain
	/// ordinary tokens. The text between groups becomes a `String` token,
	/// with line breaks as `\n`.
	fn lex_text(&mut self) {
		let mut chunk = String::new();
		let mut chunk_start = self.pos;
		macro_rules! flush {
			() => {
				if !chunk.is_empty() {
					self.tokens.push(RawToken {
						start: chunk_start as u32,
						end: self.pos as u32,
						token: TokenKind::String(std::mem::take(&mut chunk).into()),
						matched: 0,
					});
				}
			};
		}
		loop {
			let start = self.pos;
			match self.peek_char() {
				None => {
					flush!();
					self.errors.fatal("unterminated text block", self.pos..self.pos);
					self.push(start, TokenKind::Punct('"'));
					return;
				}
				Some('"') if self.src[self.pos..].starts_with("\"\"\"") => {
					flush!();
					self.pos += 3;
					self.push(start, TokenKind::Punct('"'));
					return;
				}
				Some('"') => {
					self.errors.error("unescaped '\"' in text", start..start + 1);
					self.next_char();
					chunk.push('"');
				}
				Some('\n') => {
					self.next_char();
					while self.consume("\t") || self.consume(" ") {}
					chunk.push('\n');
				}
				Some('\\') => {
					self.next_char();
					if self.consume("\n") {
						while self.consume("\t") || self.consume(" ") {}
					} else {
						match lex_escape(self.src, &mut self.pos) {
							Some(c) => chunk.push(c),
							None => {
								self.errors.error("invalid escape sequence", start..self.pos);
							}
						}
					}
				}
				Some('{') => {
					flush!();
					self.next_char();
					self.push(start, TokenKind::Punct('{'));
					self.lex_control();
					chunk_start = self.pos;
				}
				Some('}') => {
					self.errors.error("stray '}' in text", start..start + 1);
					self.next_char();
				}
				Some(c) => {
					self.next_char();
					chunk.push(c);
				}
			}
		}
	}

	/// The inside of a `{}` control group in a text block: ordinary tokens up
	/// to the matching `}`. Stops short at `"""` or end of input, leaving
	/// `match_delims` to report the unclosed `{`.
	fn lex_control(&mut self) {
		let mut depth = 0usize;
		loop {
			self.skip_whitespace();
			if self.src[self.pos..].starts_with("\"\"\"") {
				return;
			}
			let start = self.pos;
			let Some(token) = self.lex_token() else { return };
			match token {
				TokenKind::Punct('{') => depth += 1,
				TokenKind::Punct('}') if depth == 0 => {
					self.push(start, token);
					return;
				}
				TokenKind::Punct('}') => depth -= 1,
				_ => {}
			}
			self.push(start, token);
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
/// Shared between strings and text blocks; an escaped newline is not an
/// escape sequence but a continuation, handled separately in `lex_text`.
fn lex_escape(src: &str, pos: &mut usize) -> Option<char> {
	let mut chars = src[*pos..].chars();
	let mut next = || {
		let c = chars.next();
		*pos += c.map_or(0, |c| c.len_utf8());
		c
	};
	match next()? {
		'"' => Some('"'),
		'\\' => Some('\\'),
		' ' => Some(' '),
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

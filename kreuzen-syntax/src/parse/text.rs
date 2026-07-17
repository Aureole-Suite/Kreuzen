use std::ops::Range;

use kreuzen::text::{Text, TextControl, TextPart};
use kreuzen::types::{Item, Magic, Sound};

use crate::Parse;
use crate::diag::Errors;

use super::parser::{Parser, Result};

impl Parse for Text {
	fn parse(p: &mut Parser) -> Result<Self> {
		let span = p.next_span();
		let raw = p.text_block()?;
		Ok(parse_content(raw, span, p.errors))
	}
}

struct Builder<'e> {
	parts: Vec<TextPart>,
	buf: String,
	span: Range<usize>,
	errors: &'e mut Errors,
}

fn parse_content(raw: &str, span: Range<usize>, errors: &mut Errors) -> Text {
	let mut b = Builder {
		parts: Vec::new(),
		buf: String::new(),
		span,
		errors,
	};

	if !raw.contains('\n') {
		// Single-line block: raw content, no line semantics.
		b.scan_line(raw);
	} else {
		let mut lines: Vec<&str> = raw.split('\n').collect();
		// The last line is just the indentation before the closing delimiter.
		let last = lines.pop().unwrap();
		if !last.trim_start_matches('\t').is_empty() {
			b.errors.error("closing \"\"\" must be on its own line", b.span.clone());
		}
		for line in lines {
			let mut line = line.trim_start_matches('\t');
			// a `\` protects a leading space from looking like indentation
			if line.starts_with("\\ ") {
				line = &line[1..];
			}
			let continuation = b.scan_line(line);
			if !continuation {
				b.flush();
				b.parts.push(TextPart::Control(TextControl::Line));
			}
		}
	}

	b.flush();
	Text(b.parts)
}

impl Builder<'_> {
	fn flush(&mut self) {
		if !self.buf.is_empty() {
			self.parts.push(TextPart::String(std::mem::take(&mut self.buf)));
		}
	}

	/// Returns true if the line ends with a `\` continuation marker (no newline).
	fn scan_line(&mut self, line: &str) -> bool {
		let mut i = 0;
		while i < line.len() {
			let c = line[i..].chars().next().unwrap();
			match c {
				'\\' if i + 1 == line.len() => return true,
				'\\' => {
					i += 1;
					match crate::lex::lex_escape(line, &mut i) {
						Some(c) => self.buf.push(c),
						None => {
							self.errors.error("invalid escape sequence in text", self.span.clone());
						}
					}
				}
				'{' => match line[i..].find('}') {
					Some(end) => {
						let inner = &line[i + 1..i + end];
						self.control(inner);
						i += end + 1;
					}
					None => {
						self.errors.error("unclosed '{' in text", self.span.clone());
						i = line.len();
					}
				},
				'}' => {
					self.errors.error("stray '}' in text", self.span.clone());
					i += 1;
				}
				c => {
					self.buf.push(c);
					i += c.len_utf8();
				}
			}
		}
		false
	}

	fn control(&mut self, s: &str) {
		use TextControl as T;
		let control = match s {
			"pause" => T::Pause,
			"clear" => T::Clear,
			"_06" => T::_06,
			"_07" => T::_07,
			"_08" => T::_08,
			"_09" => T::_09,
			"_0B" => T::_0B,
			"_0C" => T::_0C,
			"_0F" => T::_0F,
			"_13" => T::_13,
			"_16" => T::_16,
			"_18" => T::_18,
			"_1A" => T::_1A,
			_ => {
				let Some(control) = self.parse_valued(s) else {
					self.errors.error(format!("unknown text control '{{{s}}}'"), self.span.clone());
					return;
				};
				control
			}
		};
		self.flush();
		self.parts.push(TextPart::Control(control));
	}

	fn parse_valued(&mut self, s: &str) -> Option<TextControl> {
		fn wrapped<'a>(s: &'a str, prefix: &str, suffix: &str) -> Option<&'a str> {
			s.strip_prefix(prefix)?.strip_suffix(suffix)
		}
		use TextControl as T;
		if let Some(v) = wrapped(s, "item[", "]") {
			return Some(T::Item(Item(v.parse().ok()?)));
		}
		if let Some(v) = wrapped(s, "magic[", "]") {
			return Some(T::Magic(Magic(v.parse().ok()?)));
		}
		if let Some(v) = wrapped(s, "Voice(Sound(", "))") {
			return Some(T::Voice(Sound(v.parse().ok()?)));
		}
		if let Some(v) = wrapped(s, "VoiceSilent(Sound(", "))") {
			return Some(T::VoiceSilent(Sound(v.parse().ok()?)));
		}
		if let Some(v) = wrapped(s, "Param(", ")") {
			return Some(T::Param(v.parse().ok()?));
		}
		None
	}
}

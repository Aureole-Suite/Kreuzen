use std::ops::Range;

use kreuzen::text::{Text, TextControl, TextPart};
use kreuzen::types::{Item, Magic, Sound};

use crate::diag::Errors;
use crate::{Parse, Parser, Print, Printer, Result};

fn push_control(lines: &mut Vec<String>, control: &str) {
	let len = lines.len();
	assert!(len >= 2); // starts at 2 and is never popped without pushing after
	if lines[len - 1].is_empty() && !lines[len - 2].ends_with('\n') {
		lines.pop();
	}
	lines.last_mut().unwrap().push_str(control);
	lines.push(String::new());
}

fn format_line(mut line: String) -> String {
	if line.starts_with(' ') {
		line.insert(0, '\\');
	}
	if line.ends_with('\n') {
		line.pop();
	} else {
		line.push('\\');
	}
	line
}

impl Print for Text {
	fn print(&self, ctx: &mut Printer) {
		// start with two lines, so that push_control works. First will usually be empty.
		let mut lines = vec![String::new(), String::new()];

		for part in &self.0 {
			match part {
				TextPart::String(s) => crate::types::escape_str(lines.last_mut().unwrap(), s),
				TextPart::Control(TextControl::Line) => push_control(&mut lines, "\n"),
				TextPart::Control(TextControl::Pause) => push_control(&mut lines, "{pause}"),
				TextPart::Control(TextControl::Clear) => push_control(&mut lines, "{clear}"),
				TextPart::Control(c) => {
					let mut sub = Printer::new();
					c.print(&mut sub);
					let line = lines.last_mut().unwrap();
					line.push('{');
					line.push_str(&sub.finish());
					line.push('}');
				}
			}
		}

		assert!(lines.len() >= 2);
		if lines.len() == 2 && lines[0].is_empty() {
			ctx.token(format!(r#""""{}""""#, lines[1]));
		} else {
			if lines.last().is_some_and(|x| x.is_empty()) {
				lines.pop();
			}
			let mut iter = lines.into_iter().map(format_line);
			ctx.token(format!(r#""""{}"#, iter.next().unwrap()));
			ctx.indent += 1;
			for line in iter {
				ctx.newline(0);
				ctx.token(line);
			}
			ctx.indent -= 1;
			ctx.newline(0);
			ctx.token(r#"""""#);
		};
	}
}

impl Print for TextControl {
	fn print(&self, ctx: &mut Printer) {
		match self {
			TextControl::Line | TextControl::Pause | TextControl::Clear => unreachable!(),
			TextControl::Item(v) => v.print(ctx),
			TextControl::Magic(v) => v.print(ctx),
			_ => ctx.token(format!("{self:?}")),
		}
	}
}

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

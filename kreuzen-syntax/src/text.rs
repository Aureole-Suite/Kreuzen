use kreuzen::text::{Text, TextControl, TextPart};

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

fn render_control(control: &TextControl) -> String {
	format!("{{{}}}", control.print_to_string())
}

impl Print for Text {
	fn print(&self, ctx: &mut Printer) {
		// start with two lines, so that push_control works. First will usually be empty.
		let mut lines = vec![String::new(), String::new()];

		for part in &self.0 {
			match part {
				TextPart::String(s) => crate::types::escape_str(lines.last_mut().unwrap(), s),
				TextPart::Control(TextControl::Line) => push_control(&mut lines, "\n"),
				TextPart::Control(c @ (TextControl::Pause | TextControl::Clear)) => push_control(&mut lines, &render_control(c)),
				TextPart::Control(c) => lines.last_mut().unwrap().push_str(&render_control(c)),
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

impl Parse for Text {
	fn parse(p: &mut Parser) -> Result<Self> {
		p.delim('"', |p| {
			let mut parts = Vec::new();
			while !p.at_end() {
				if let Ok(s) = p.string() {
					for (i, line) in s.split('\n').enumerate() {
						if i > 0 {
							parts.push(TextPart::Control(TextControl::Line));
						}
						if !line.is_empty() {
							parts.push(TextPart::String(line.to_owned()));
						}
					}
				} else if let Ok(control) = p.delim('{', |p| p.parse()) {
					parts.push(TextPart::Control(control));
				}
			}
			Ok(Text(parts))
		})
	}
}

impl Print for TextControl {
	fn print(&self, ctx: &mut Printer) {
		use TextControl as T;
		match self {
			T::Line => unreachable!("printed as a line break"),
			T::Pause => ctx.word("pause"),
			T::Clear => ctx.word("clear"),
			T::_06 => ctx.word("_06"),
			T::_07 => ctx.word("_07"),
			T::_08 => ctx.word("_08"),
			T::_09 => ctx.word("_09"),
			T::_0B => ctx.word("_0B"),
			T::_0C => ctx.word("_0C"),
			T::_0F => ctx.word("_0F"),
			T::Item(v) => v.print(ctx),
			T::Voice(v) => {
				ctx.word("voice");
				v.print(ctx);
			}
			T::VoiceSilent(v) => {
				ctx.word("voice_silent");
				v.print(ctx);
			}
			T::_13 => ctx.word("_13"),
			T::_16 => ctx.word("_16"),
			T::Param(v) => {
				ctx.word("param");
				v.print(ctx);
			}
			T::_18 => ctx.word("_18"),
			T::Magic(v) => v.print(ctx),
			T::_1A => ctx.word("_1A"),
		}
	}
}

impl Parse for TextControl {
	fn parse(p: &mut Parser) -> Result<Self> {
		use TextControl as T;
		p.alt()
			.test_kw("pause", |_| Ok(T::Pause))
			.test_kw("clear", |_| Ok(T::Clear))
			.test_kw("_06", |_| Ok(T::_06))
			.test_kw("_07", |_| Ok(T::_07))
			.test_kw("_08", |_| Ok(T::_08))
			.test_kw("_09", |_| Ok(T::_09))
			.test_kw("_0B", |_| Ok(T::_0B))
			.test_kw("_0C", |_| Ok(T::_0C))
			.test_kw("_0F", |_| Ok(T::_0F))
			.test_kw("_13", |_| Ok(T::_13))
			.test_kw("_16", |_| Ok(T::_16))
			.test_kw("_18", |_| Ok(T::_18))
			.test_kw("_1A", |_| Ok(T::_1A))
			.test_kw("voice", |p| p.parse().map(T::Voice))
			.test_kw("voice_silent", |p| p.parse().map(T::VoiceSilent))
			.test_kw("param", |p| p.parse().map(T::Param))
			.test(|p| p.parse().map(T::Item))
			.test(|p| p.parse().map(T::Magic))
			.finish()
	}
}

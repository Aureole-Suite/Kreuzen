use kreuzen::text::{Text, TextControl, TextPart};
use kreuzen::types;

use crate::{Ctx, Print};

macro_rules! print_via_debug {
	($($t:ty),* $(,)?) => {
		$(
			impl Print for $t {
				fn print(&self, ctx: &mut Ctx) {
					ctx.token(format!("{self:?}"));
				}
			}
		)*
	};
}

print_via_debug!(u8, u16, i64, i32, u32, f32);
print_via_debug!(types::Flags8, types::Flags16, types::Flags32);

fn escape_str(out: &mut String, s: &str) {
	use std::fmt::Write;
	for c in s.chars() {
		match c {
			'\\' => out.push_str("\\\\"),
			'"' => out.push_str("\\\""),
			'\n' => out.push_str("\\n"),
			'\t' => out.push_str("\\t"),
			'\r' => out.push_str("\\r"),
			'{' => out.push_str("\\{"),
			'}' => out.push_str("\\}"),
			c if c.is_ascii_control() => write!(out, "\\x{:02X}", c as u32).unwrap(),
			c if c.is_control() => write!(out, "\\u{{{:04X}}}", c as u32).unwrap(),
			c => out.push(c),
		}
	}
}

impl Print for str {
	fn print(&self, ctx: &mut Ctx) {
		let mut out = String::with_capacity(self.len() + 2);
		out.push('"');
		escape_str(&mut out, self);
		out.push('"');
		ctx.token(out);
	}
}

impl Print for String {
	fn print(&self, ctx: &mut Ctx) {
		self.as_str().print(ctx);
	}
}

macro_rules! print_tuple {
	($($t:ident)*) => {
		#[expect(non_snake_case)]
		impl<$($t: Print,)+> Print for ($($t,)+) {
			fn print(&self, ctx: &mut Ctx) {
				let ($($t,)+) = self;
				ctx._sym("(");
				$($t.print(ctx);)+
				ctx.sym_(")");
			}
		}
	};
}

print_tuple!(A);
print_tuple!(A B);
print_tuple!(A B C);
print_tuple!(A B C D);
print_tuple!(A B C D E);

impl<T: Print, const N: usize> Print for [T; N] {
	fn print(&self, ctx: &mut Ctx) {
		ctx._sym("(");
		for v in self {
			v.print(ctx);
		}
		ctx.sym_(")");
	}
}

macro_rules! print_bracket {
	($($t:ty => $name:literal),* $(,)?) => {
		$(
			impl Print for $t {
				fn print(&self, ctx: &mut Ctx) {
					ctx.token(format!("{}[{}]", $name, self.0));
				}
			}
		)*
	};
}

print_bracket!(
	types::Item => "item",
	types::Battle => "battle",
	types::Magic => "magic",
	types::Sound => "sound",
	types::Music => "music",
	types::Flag => "flag",
	types::Global => "global",
	types::Var => "var",
	types::FuncArg => "func_arg",
	types::NumReg => "num_reg",
	types::StrReg => "str_reg",
	types::Attr => "attr",
);

impl Print for types::Char {
	fn print(&self, ctx: &mut Ctx) {
		let inner = match self.0 {
			0xFFFE => "self".to_string(),
			0xFFFF => "null".to_string(),
			n if n >= 0xF000 => format!("0x{n:04X}"),
			n => format!("{n}"),
		};
		ctx.token(format!("char[{inner}]"));
	}
}

impl Print for types::CharAttr {
	fn print(&self, ctx: &mut Ctx) {
		self.0.print(ctx);
		ctx.sym(".");
		ctx.token(self.1.to_string())
	}
}

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
	fn print(&self, ctx: &mut Ctx) {
		// start with two lines, so that push_control works. First will usually be empty.
		let mut lines = vec![String::new(), String::new()];

		for part in &self.0 {
			match part {
				TextPart::String(s) => escape_str(lines.last_mut().unwrap(), s),
				TextPart::Control(TextControl::Line) => push_control(&mut lines, "\n"),
				TextPart::Control(TextControl::Pause) => push_control(&mut lines, "{pause}"),
				TextPart::Control(TextControl::Clear) => push_control(&mut lines, "{clear}"),
				TextPart::Control(c) => {
					let mut sub = Ctx::new();
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
	fn print(&self, ctx: &mut Ctx) {
		match self {
			TextControl::Line | TextControl::Pause | TextControl::Clear => unreachable!(),
			TextControl::Item(v) => v.print(ctx),
			TextControl::Magic(v) => v.print(ctx),
			_ => ctx.token(format!("{self:?}")),
		}
	}
}

use kreuzen::text::{Text, TextControl, TextPart};
use kreuzen::types;

use crate::{Error, Parse, Parser, Print, Printer, Result};

macro_rules! int {
	($($t:ty),* $(,)?) => {
		$(impl Print for $t {
			fn print(&self, ctx: &mut Printer) {
				ctx.token(format!("{self:?}"));
			}
		})*
		$(impl Parse for $t {
			fn parse(p: &mut Parser) -> Result<Self> {
				let v = p.int()?;
				<$t>::try_from(v).map_err(|_| {
					p.errors.error(concat!("value out of range for ", stringify!($t)), p.prev_span());
					Error
				})
			}
		})*
	};
}

int!(u8, u16, u32, i8, i16, i32, i64);

impl Print for f32 {
	fn print(&self, ctx: &mut Printer) {
		ctx.token(format!("{self:?}"));
	}
}

impl Parse for f32 {
	fn parse(p: &mut Parser) -> Result<Self> {
		p.alt()
			.test(|p| p.float())
			.test_kw("inf", |_| Ok(f32::INFINITY))
			.test_kw("NaN", |_| Ok(f32::NAN))
			.test(|p| {
				p.punct('-')?;
				p.keyword("inf")?;
				Ok(f32::NEG_INFINITY)
			})
			.finish()
	}
}

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
	fn print(&self, ctx: &mut Printer) {
		let mut out = String::with_capacity(self.len() + 2);
		out.push('"');
		escape_str(&mut out, self);
		out.push('"');
		ctx.token(out);
	}
}

impl Print for String {
	fn print(&self, ctx: &mut Printer) {
		self.as_str().print(ctx);
	}
}

impl Parse for String {
	fn parse(p: &mut Parser) -> Result<Self> {
		p.string().map(str::to_owned)
	}
}

macro_rules! tuple {
	($($t:ident)*) => {
		#[expect(non_snake_case)]
		impl<$($t: Print,)+> Print for ($($t,)+) {
			fn print(&self, ctx: &mut Printer) {
				let ($($t,)+) = self;
				ctx._sym("(");
				$($t.print(ctx);)+
				ctx.sym_(")");
			}
		}

		#[expect(non_snake_case)]
		impl<$($t: Parse,)+> Parse for ($($t,)+) {
			fn parse(p: &mut Parser) -> Result<Self> {
				p.delim('(', |p| {
					$(let $t = p.parse::<$t>()?;)+
					Ok(($($t,)+))
				})
			}
		}
	};
}

tuple!(A);
tuple!(A B);
tuple!(A B C);
tuple!(A B C D);
tuple!(A B C D E);

impl<T: Print, const N: usize> Print for [T; N] {
	fn print(&self, ctx: &mut Printer) {
		ctx._sym("(");
		for v in self {
			v.print(ctx);
		}
		ctx.sym_(")");
	}
}

impl<T: Parse, const N: usize> Parse for [T; N] {
	fn parse(p: &mut Parser) -> Result<Self> {
		p.delim('(', |p| {
			let mut out = Vec::with_capacity(N);
			for _ in 0..N {
				out.push(p.parse::<T>()?);
			}
			out.try_into().map_err(|_| Error)
		})
	}
}

fn bracket<T>(p: &mut Parser, name: &'static str, f: impl FnOnce(&mut Parser) -> Result<T>) -> Result<T> {
	p.cursor.keyword(name)?;
	p.delim('[', f)
}

macro_rules! bracket {
	($($t:path => $name:literal),* $(,)?) => {
		$(impl Print for $t {
			fn print(&self, ctx: &mut Printer) {
				ctx.word($name);
				ctx.sym("[");
				self.0.print(ctx);
				ctx.sym_("]");
			}
		})*
		$(impl Parse for $t {
			fn parse(p: &mut Parser) -> Result<Self> {
				bracket(p, $name, |p| p.parse()).map($t)
			}
		})*
	};
}

bracket!(
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
	types::SystemFlags => "system",
);

macro_rules! flags {
	($($t:path => $width:literal),* $(,)?) => {
		$(impl Print for $t {
			fn print(&self, ctx: &mut Printer) {
				ctx.token(format!("0x{:01$X}", self.0, $width));
			}
		})*
		$(impl Parse for $t {
			fn parse(p: &mut Parser) -> Result<Self> {
				p.parse().map($t)
			}
		})*
	};
}
flags!(
	types::Flags8 => 2,
	types::Flags16 => 4,
	types::Flags32 => 8,
);

impl Print for types::Char {
	fn print(&self, ctx: &mut Printer) {
		let inner = match self.0 {
			0xFFFE => "self".to_string(),
			0xFFFF => "null".to_string(),
			n if n >= 0xF000 => format!("0x{n:04X}"),
			n => format!("{n}"),
		};
		ctx.token(format!("char[{inner}]"));
	}
}

impl Parse for types::Char {
	fn parse(p: &mut Parser) -> Result<Self> {
		bracket(p, "char", |p| {
			p.alt()
				.test_kw("self", |_| Ok(types::Char(0xFFFE)))
				.test_kw("null", |_| Ok(types::Char(0xFFFF)))
				.test(|p| p.parse().map(types::Char))
				.finish()
		})
	}
}

impl Print for types::CharAttr {
	fn print(&self, ctx: &mut Printer) {
		self.0.print(ctx);
		ctx.sym(".");
		ctx.token(self.1.to_string())
	}
}

impl Parse for types::CharAttr {
	fn parse(p: &mut Parser) -> Result<Self> {
		let c = p.parse()?;
		p.glued_punct('.')?;
		let a = p.parse()?;
		Ok(types::CharAttr(c, a))
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
	fn print(&self, ctx: &mut Printer) {
		// start with two lines, so that push_control works. First will usually be empty.
		let mut lines = vec![String::new(), String::new()];

		for part in &self.0 {
			match part {
				TextPart::String(s) => escape_str(lines.last_mut().unwrap(), s),
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

pub mod tables;

use std::borrow::Cow;

use kreuzen::code::preload::Preload;
use kreuzen::code::{Arg, Code, FlatOp, Label, Op, OpMeta};
use kreuzen::decompile::{Case, Stmt};
use kreuzen::expr::{AssOp, BinOp, Expr, UnOp};
use kreuzen::text::{Text, TextControl, TextPart};
use kreuzen::types;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default)]
enum Space {
	#[default]
	None,
	Inline,
	Block(usize),
}

struct Ctx {
	out: String,
	space: Space,
	indent: usize,
}

impl Ctx {
	fn new() -> Self {
		Self { out: String::new(), space: Space::None, indent: 0 }
	}

	fn token(&mut self, word: impl Into<Cow<'static, str>>) {
		self.do_space(true);
		self.out.push_str(&word.into());
		self.set_space(Space::Inline);
	}

	fn word(&mut self, word: &'static str) {
		self.token(word)
	}

	fn sym(&mut self, sym: &'static str) {
		self.do_space(false);
		self.out.push_str(sym);
		self.set_space(Space::None);
	}

	fn _sym(&mut self, sym: &'static str) {
		self.do_space(true);
		self.out.push_str(sym);
		self.set_space(Space::None);
	}

	fn sym_(&mut self, sym: &'static str) {
		self.do_space(false);
		self.out.push_str(sym);
		self.set_space(Space::Inline);
	}

	fn _sym_(&mut self, sym: &'static str) {
		self.do_space(true);
		self.out.push_str(sym);
		self.set_space(Space::Inline);
	}

	fn do_space(&mut self, inline: bool) {
		if self.out.is_empty() {
			self.space = Space::None;
			return;
		}
		match self.space {
			Space::None => {}
			Space::Inline => {
				if inline {
					self.out.push(' ');
				}
			}
			Space::Block(n) => {
				for _ in 0..=n {
					self.out.push('\n');
				}
				for _ in 0..self.indent {
					self.out.push('\t');
				}
			}
		}
		self.space = Space::None;
	}

	fn set_space(&mut self, space: Space) {
		self.space = self.space.max(space);
	}

	fn block<I: IntoIterator>(&mut self, block: I, mut f: impl FnMut(I::Item, &mut Self)) {
		self._sym_("{");
		self.indent += 1;
		for stmt in block {
			self.set_space(Space::Block(0));
			f(stmt, self);
		}
		self.set_space(Space::Block(0));
		self.indent -= 1;
		self._sym_("}");
	}

	fn meta(&mut self, m: OpMeta) {
		if m.line != 0 {
			self.token(format!("{}", m.line));
			self.sym("@");
		}
		if m.width > 1 {
			self.token(format!("{}", m.width));
		}
		if m.width > 0 {
			self.sym("~");
		}
	}
}

pub fn print_function(stmts: &[Stmt]) -> String {
	let mut ctx = Ctx::new();
	stmts.print(&mut ctx);
	ctx.out
}

pub fn print_flat(code: &Code) -> String {
	let mut ctx = Ctx::new();
	ctx.block(&code.ops, FlatOp::print);
	ctx.out
}

pub fn print_preload(preloads: &[Preload]) -> String {
	let mut ctx = Ctx::new();
	ctx.block(preloads, Preload::print);
	ctx.out
}

trait Print {
	fn print(&self, ctx: &mut Ctx);
}

impl Print for Stmt {
	fn print(&self, ctx: &mut Ctx) {
		match self {
			Stmt::Op(op) => {
				op.print(ctx);
				ctx.sym_(";");
			}
			Stmt::Break(m) => {
				ctx.meta(*m);
				ctx.word("break");
				ctx.sym_(";");
			}
			Stmt::Continue(m) => {
				ctx.meta(*m);
				ctx.word("continue");
				ctx.sym_(";");
			}
			Stmt::If(m, e, then, els) => {
				ctx.meta(*m);
				ctx.word("if");
				e.print(ctx);
				then.print(ctx);
				if let Some((m2, els)) = els {
					ctx.meta(*m2);
					ctx.word("else");
					if let [stmt @ Stmt::If(..)] = els.as_slice() {
						stmt.print(ctx);
					} else {
						els.print(ctx);
					}
				}
			}
			Stmt::While(m, e, body, _) => {
				ctx.meta(*m);
				ctx.word("while");
				e.print(ctx);
				body.print(ctx);
			}
			Stmt::Switch(m, e, cases) => {
				ctx.meta(*m);
				ctx.word("switch");
				e.print(ctx);
				ctx.block(cases, |(case, body), ctx| {
					match case {
						Case::Default => {
							ctx.word("default");
							ctx.sym_(":");
						}
						Case::Case(v) => {
							ctx.word("case");
							ctx.token(v.to_string());
							ctx.sym_(":");
						}
						Case::None => {}
					}
					ctx.indent += 1;
					for stmt in body {
						ctx.set_space(Space::Block(0));
						stmt.print(ctx);
					}
					ctx.indent -= 1;
				});
			}
		}
	}
}

impl Print for [Stmt] {
	fn print(&self, ctx: &mut Ctx) {
		ctx.block(self, Stmt::print);
	}
}

impl Print for FlatOp {
	fn print(&self, ctx: &mut Ctx) {
		match self {
			FlatOp::Op(op) => {
				op.print(ctx);
				ctx.sym_(";");
				return;
			}
			FlatOp::Label(l) => {
				ctx.indent -= 1;
				l.print(ctx);
				ctx.sym_(":");
				ctx.indent += 1;
				return;
			}
			FlatOp::Goto(m, l) => {
				ctx.meta(*m);
				ctx.word("goto");
				l.print(ctx);
			}
			FlatOp::If(m, e, l) => {
				ctx.meta(*m);
				ctx.word("if");
				e.print(ctx);
				l.print(ctx);
			}
			FlatOp::Switch(m, e, cases, default) => {
				ctx.meta(*m);
				ctx.word("switch");
				e.print(ctx);
				ctx.block(cases, |(value, label), ctx| {
					ctx.token(value.to_string());
					ctx._sym_("=>");
					label.print(ctx);
					ctx.sym_(";");
				});
				default.print(ctx);
			}
		}
		ctx.sym_(";");
	}
}

impl Print for Preload {
	fn print(&self, ctx: &mut Ctx) {
		macro_rules! inner {
			($($name:ident ( $($arg:ident),* ),)*) => {
				match self {
					$(Preload::$name($($arg),*) => {
						ctx.word(stringify!($name));
						ctx.sym("(");
						let mut _first = true;
						$(
							if !_first { ctx.sym_(","); }
							_first = false;
							Print::print($arg, ctx);
						)+
						ctx.sym_(")");
					})*
				}
			}
		}

		inner! {
			Call(n, s),
			PkgLoad(s),
			EffLoad(s),
			SoundPlay(n),
			SoundPlayVoice(n),
			Voice(n),
			CharAniclipPlay(c, s),
			NameplateShow(s),
			opCE02(s),
		};
		ctx.sym_(";");
	}
}

impl Print for Op {
	fn print(&self, ctx: &mut Ctx) {
		ctx.meta(self.meta);
		if matches!(self.name, "SetAttr" | "SetVar" | "SetNumReg" | "SetGlobal" | "SetCharAttr") {
			assert_eq!(self.args.len(), 2);
			self.args[0].print(ctx);
			let Arg::Expr(expr) = &self.args[1] else {
				panic!("setter second arg must be expr");
			};
			expr.print(ctx);
		} else {
			ctx.token(self.name);
			for arg in &self.args {
				arg.print(ctx);
			}
		}
	}
}

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

print_via_debug!(String, str, u8, u16, i64, i32, u32, f32);
print_via_debug!(types::Flags8, types::Flags16, types::Flags32);

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

impl Print for Arg {
	fn print(&self, ctx: &mut Ctx) {
		match self {
			Arg::Str(v) => v.print(ctx),
			Arg::Int(v) => v.print(ctx),
			Arg::F32(v) => v.print(ctx),
			Arg::F32Munged(v) => {
				v.print(ctx);
				ctx.sym_("'");
			}
			Arg::I32Munged(v) => {
				v.print(ctx);
				ctx.sym_("'");
			}
			Arg::Char(v) => v.print(ctx),
			Arg::Item(v) => v.print(ctx),
			Arg::Battle(a, v) => {
				ctx.token(format!("btlset[{a}]"));
				ctx.sym(":");
				v.print(ctx);
			}
			Arg::Magic(v) => v.print(ctx),
			Arg::Sound(v) => v.print(ctx),
			Arg::Music(v) => v.print(ctx),
			Arg::Flag(v) => v.print(ctx),
			Arg::Global(v) => v.print(ctx),
			Arg::Var(v) => v.print(ctx),
			Arg::FuncArg(v) => v.print(ctx),
			Arg::NumReg(v) => v.print(ctx),
			Arg::StrReg(v) => v.print(ctx),
			Arg::Attr(v) => v.print(ctx),
			Arg::CharAttr(v) => v.print(ctx),
			Arg::Flags8(v) => v.print(ctx),
			Arg::Flags16(v) => v.print(ctx),
			Arg::Flags32(v) => v.print(ctx),
			Arg::Expr(_) => unreachable!("expr handled separately"),
			Arg::Text(v) => v.print(ctx),
		}
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
				TextPart::String(s) => lines.last_mut().unwrap().push_str(s),
				TextPart::Control(TextControl::Line) => push_control(&mut lines, "\n"),
				TextPart::Control(TextControl::Pause) => push_control(&mut lines, "{pause}"),
				TextPart::Control(TextControl::Clear) => push_control(&mut lines, "{clear}"),
				TextPart::Control(c) => {
					let mut sub = Ctx::new();
					c.print(&mut sub);
					let line = lines.last_mut().unwrap();
					line.push('{');
					line.push_str(&sub.out);
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
				ctx.set_space(Space::Block(0));
				ctx.token(line);
			}
			ctx.indent -= 1;
			ctx.set_space(Space::Block(0));
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

impl Print for Expr {
	fn print(&self, ctx: &mut Ctx) {
		print_expr(self, ctx, 0);
	}
}

fn print_expr(e: &Expr, ctx: &mut Ctx, prec: u32) {
	match e {
		Expr::Int(v) => {
			if *v >= 0x10000 && v.count_ones() == 1 {
				ctx.token(format!("0x{v:08X}"));
			} else {
				v.print(ctx)
			}
		}
		Expr::Op(op) => op.print(ctx),
		Expr::Flag(v) => v.print(ctx),
		Expr::Var(v) => v.print(ctx),
		Expr::Attr(v) => v.print(ctx),
		Expr::CharAttr(v) => v.print(ctx),
		Expr::Rand => ctx.word("rand"),
		Expr::Global(v) => v.print(ctx),
		Expr::SystemFlags(v) => v.print(ctx),
		Expr::NumReg(v) => v.print(ctx),
		Expr::Bin(op, a, b) => {
			let (sym, p) = binop_prio(*op);
			if p < prec {
				ctx._sym("(");
			}
			print_expr(a, ctx, p);
			ctx._sym_(sym);
			print_expr(b, ctx, p + 1);
			if p < prec {
				ctx.sym_(")");
			}
		}
		Expr::Un(op, a) => {
			ctx._sym(match op {
				UnOp::BoolNot => "!",
				UnOp::Neg => "-",
				UnOp::BitNot => "~",
			});
			print_expr(a, ctx, 10);
		}
		Expr::Ass(op, a) => {
			ctx._sym_(match op {
				AssOp::Ass => "=",
				AssOp::MulAss => "*=",
				AssOp::DivAss => "/=",
				AssOp::ModAss => "%=",
				AssOp::AddAss => "+=",
				AssOp::SubAss => "-=",
				AssOp::AndAss => "&=",
				AssOp::XorAss => "^=",
				AssOp::OrAss => "|=",
			});
			print_expr(a, ctx, 0);
		}
	}
}

fn binop_prio(op: BinOp) -> (&'static str, u32) {
	use BinOp::*;
	match op {
		Mul => ("*", 7),
		Div => ("/", 7),
		Mod => ("%", 7),
		Add => ("+", 6),
		Sub => ("-", 6),
		BitAnd => ("&", 5),
		Xor => ("^", 4),
		Or => ("|", 3),
		Eq => ("==", 2),
		Ne => ("!=", 2),
		Lt => ("<", 2),
		Gt => (">", 2),
		Le => ("<=", 2),
		Ge => (">=", 2),
		BoolAnd => ("&&", 1),
	}
}

impl Print for Label {
	fn print(&self, ctx: &mut Ctx) {
		ctx._sym("$");
		ctx.token(format!("L{}", self.0));
	}
}

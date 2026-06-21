use std::borrow::Cow;

use kreuzen::code::{Arg, Code, FlatOp, Label, Op, OpMeta};
use kreuzen::decompile::{Case, Stmt};
use kreuzen::expr::{AssOp, BinOp, Expr, UnOp};
use kreuzen::tables::preload::Preload;
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

	fn arglist<I: IntoIterator>(&mut self, args: I, mut f: impl FnMut(I::Item, &mut Self)) {
		self.sym("(");
		for (i, arg) in args.into_iter().enumerate() {
			if i != 0 {
				self.sym_(",");
			}
			f(arg, self);
		}
		self.sym_(")");
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
	ctx.block(stmts, Stmt::print);
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
				ctx.block(then, Stmt::print);
				if let Some((m2, els)) = els {
					ctx.meta(*m2);
					ctx.word("else");
					if let [stmt @ Stmt::If(..)] = els.as_slice() {
						stmt.print(ctx);
					} else {
						ctx.block(els, Stmt::print);
					}
				}
			}
			Stmt::While(m, e, body, _) => {
				ctx.meta(*m);
				ctx.word("while");
				e.print(ctx);
				ctx.block(body, Stmt::print);
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

macro_rules! call {
	($ctx:expr, $name:expr $(, $arg:expr)* $(,)?) => {{
		let __c: &mut Ctx = $ctx;
		__c.word($name);
		__c.sym("(");
		let mut _first = true;
		$(
			if !_first { __c.sym_(","); }
			_first = false;
			$arg.print(&mut *__c);
		)*
		__c.sym_(")");
	}};
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
		match self {
			Preload::Call(n, s) => call!(ctx, "Call", n, s),
			Preload::PkgLoad(s) => call!(ctx, "PkgLoad", s),
			Preload::EffLoad(s) => call!(ctx, "EffLoad", s),
			Preload::SoundPlay(n) => call!(ctx, "SoundPlay", n),
			Preload::SoundPlayVoice(n) => call!(ctx, "SoundPlayVoice", n),
			Preload::Voice(n) => call!(ctx, "Voice", n),
			Preload::CharAniclipPlay(c, s) => call!(ctx, "CharAniclipPlay", c, s),
			Preload::NameplateShow(s) => call!(ctx, "NameplateShow", s),
			Preload::opCE02(s) => call!(ctx, "opCE02", s),
		}
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
		} else if self.name == "return" {
			assert!(self.args.is_empty());
			ctx.token(self.name);
		} else {
			ctx.token(self.name);
			ctx.arglist(self.args.iter(), Arg::print);
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

print_via_debug!(
	String, str, i64, i32, u32, f32,
	types::Char, types::Item, types::Magic, types::Flag, types::Global, types::Var,
	types::FuncArg, types::NumReg, types::StrReg, types::Attr, types::CharAttr,
	types::Flags8, types::Flags16, types::Flags32,
);

impl Print for Arg {
	fn print(&self, ctx: &mut Ctx) {
		match self {
			Arg::Str(v) => v.print(ctx),
			Arg::Int(v) => v.print(ctx),
			Arg::F32(v) => v.print(ctx),
			Arg::F32Munged(v) => { v.print(ctx); ctx.sym("'"); }
			Arg::I32Munged(v) => { v.print(ctx); ctx.sym("'"); }
			Arg::Char(v) => v.print(ctx),
			Arg::Item(v) => v.print(ctx),
			Arg::Magic(v) => v.print(ctx),
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

impl Print for Text {
	fn print(&self, ctx: &mut Ctx) {
		let mut body = String::from("\"\"\"");
		for part in &self.0 {
			match part {
				TextPart::String(s) => body.push_str(s),
				TextPart::Control(c) => {
					let mut sub = Ctx::new();
					c.print(&mut sub);
					body.push('{');
					body.push_str(&sub.out);
					body.push('}');
				}
			}
		}
		body.push_str("\"\"\"");
		ctx.token(body);
	}
}

impl Print for TextControl {
	fn print(&self, ctx: &mut Ctx) {
		match self {
			TextControl::Line => ctx.word("line"),
			TextControl::Pause => ctx.word("pause"),
			TextControl::Clear => ctx.word("clear"),
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
		Expr::Int(v) => v.print(ctx),
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
			if p < prec { ctx._sym("("); }
			print_expr(a, ctx, p);
			ctx._sym_(sym);
			print_expr(b, ctx, p + 1);
			if p < prec { ctx.sym_(")"); }
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

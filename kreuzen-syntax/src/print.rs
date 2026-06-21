use std::borrow::Cow;

use kreuzen::code::{Arg, Op, OpMeta};
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
	ctx.out.push('\n');
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

impl Print for Op {
	fn print(&self, ctx: &mut Ctx) {
		ctx.meta(self.meta);
		ctx.token(self.name);
		ctx.arglist(self.args.iter(), Arg::print);
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
	String, str, i64, i32, f32,
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
			Arg::Expr(e) => e.print(ctx),
			Arg::Text(t) => t.print(ctx),
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
			if p < prec { ctx.sym("("); }
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

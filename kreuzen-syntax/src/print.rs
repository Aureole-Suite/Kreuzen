use std::borrow::Cow;

use kreuzen::code::{Arg, Op, OpMeta};
use kreuzen::decompile::{Case, Stmt};
use kreuzen::expr::{AssOp, BinOp, Expr, UnOp};
use kreuzen::text::{Text, TextPart};

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

impl Print for Arg {
	fn print(&self, ctx: &mut Ctx) {
		match self {
			Arg::Expr(e) => e.print(ctx),
			Arg::Text(t) => t.print(ctx),
			_ => ctx.token(format!("{self:?}")), // TODO
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
					body.push('{');
					body.push_str(&format!("{c:?}"));
					body.push('}');
				}
			}
		}
		body.push_str("\"\"\"");
		ctx.token(body);
	}
}

impl Print for Expr {
	fn print(&self, ctx: &mut Ctx) {
		print_expr(self, ctx, 0);
	}
}

fn print_expr(e: &Expr, ctx: &mut Ctx, prec: u32) {
	match e {
		Expr::Int(v) => ctx.token(v.to_string()),
		Expr::Op(op) => op.print(ctx),
		Expr::Rand => ctx.word("rand"),
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
		_ => ctx.token(format!("{e:?}")), // TODO
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

use kreuzen::code::preload::Preload;
use kreuzen::code::shadow::{Shadow, ShadowOp};
use kreuzen::code::{Arg, Code, FlatOp, Label, Op, OpMeta};
use kreuzen::decompile::{Case, Stmt};
use kreuzen::expr::{AssOp, BinOp, Expr, UnOp};

use crate::{Ctx, Print};

impl Print for OpMeta {
	fn print(&self, ctx: &mut Ctx) {
		if self.line != 0 {
			ctx.token(format!("{}", self.line));
			ctx.sym("@");
		}
		if self.width > 1 {
			ctx.token(format!("{}", self.width));
		}
		if self.width > 0 {
			ctx.sym("~");
		}
	}
}

impl Print for Stmt {
	fn print(&self, ctx: &mut Ctx) {
		match self {
			Stmt::Op(op) => {
				op.print(ctx);
				ctx.sym_(";");
			}
			Stmt::Break(m) => {
				m.print(ctx);
				ctx.word("break");
				ctx.sym_(";");
			}
			Stmt::Continue(m) => {
				m.print(ctx);
				ctx.word("continue");
				ctx.sym_(";");
			}
			Stmt::If(m, e, then, els) => {
				m.print(ctx);
				ctx.word("if");
				e.print(ctx);
				then.print(ctx);
				if let Some((m2, els)) = els {
					m2.print(ctx);
					ctx.word("else");
					if let [stmt @ Stmt::If(..)] = els.as_slice() {
						stmt.print(ctx);
					} else {
						els.print(ctx);
					}
				}
			}
			Stmt::While(m, e, body, _) => {
				m.print(ctx);
				ctx.word("while");
				e.print(ctx);
				body.print(ctx);
			}
			Stmt::Switch(m, e, cases) => {
				m.print(ctx);
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
						ctx.newline(0);
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
				m.print(ctx);
				ctx.word("goto");
				l.print(ctx);
			}
			FlatOp::If(m, e, l) => {
				m.print(ctx);
				ctx.word("if");
				e.print(ctx);
				l.print(ctx);
			}
			FlatOp::Switch(m, e, cases, default) => {
				m.print(ctx);
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

impl Print for Shadow {
	fn print(&self, ctx: &mut Ctx) {
		if self.line != 0 {
			ctx.token(format!("{}", self.line));
			ctx.sym("@");
		}
		ctx.block(&self.ops, ShadowOp::print);
	}
}

impl Print for ShadowOp {
	fn print(&self, ctx: &mut Ctx) {
		match self {
			ShadowOp::Call { table, name } => {
				ctx.word("Call");
				table.print(ctx);
				name.print(ctx);
				ctx.sym_(";");
			}
			ShadowOp::CharAni { chr, strings } => {
				ctx.word("CharAni");
				chr.print(ctx);
				for s in strings {
					s.print(ctx);
				}
				ctx.sym_(";");
			}
			ShadowOp::Fork { chr, slot, name, flags } => {
				ctx.word("Fork");
				chr.print(ctx);
				slot.print(ctx);
				name.print(ctx);
				flags.print(ctx);
				ctx.sym_(";");
			}
		}
	}
}

impl Print for Op {
	fn print(&self, ctx: &mut Ctx) {
		self.meta.print(ctx);
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

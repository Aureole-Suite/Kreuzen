use kreuzen::code::{Arg, FlatOp, Label, Op, OpMeta};
use kreuzen::decompile::{Case, Stmt};
use kreuzen::expr::Expr;

pub mod expr;

use crate::{Print, Printer};

impl Print for OpMeta {
	fn print(&self, ctx: &mut Printer) {
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
	fn print(&self, ctx: &mut Printer) {
		match self {
			Stmt::Op(op) => {
				op.print(ctx);
			}
			Stmt::Break(m) => {
				m.print(ctx);
				ctx.word("break");
			}
			Stmt::Continue(m) => {
				m.print(ctx);
				ctx.word("continue");
			}
			Stmt::If(m, e, then, els) => {
				m.print(ctx);
				ctx.word("if");
				expr::print_bool(e, ctx);
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
			Stmt::While(m, e, body, m2) => {
				m.print(ctx);
				ctx.word("while");
				expr::print_bool(e, ctx);
				if *m2 == OpMeta::default() {
					body.print(ctx);
				} else {
					// The loopback op's meta, as a trailing marker in the block.
					// It is not `;`-terminated, so this can't use ctx.block.
					ctx._sym_("{");
					ctx.indent += 1;
					for stmt in body {
						ctx.newline(0);
						stmt.print(ctx);
						ctx.end_item();
					}
					ctx.newline(0);
					ctx.indent -= 1;
					m2.print(ctx);
					ctx.sym_("}");
				}
			}
			Stmt::ForkLambda(m, chr, slot, name, body) => {
				m.print(ctx);
				ctx.word("ForkLambda");
				chr.print(ctx);
				slot.print(ctx);
				name.print(ctx);
				body.print(ctx);
			}
			Stmt::Switch(m, e, cases) => {
				m.print(ctx);
				ctx.word("switch");
				expr::print(e, ctx);
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
						ctx.end_item();
					}
					ctx.indent -= 1;
				});
			}
		}
	}
}

impl Print for [Stmt] {
	fn print(&self, ctx: &mut Printer) {
		ctx.block(self, Stmt::print);
	}
}

impl Print for FlatOp {
	fn print(&self, ctx: &mut Printer) {
		match self {
			FlatOp::Op(op) => {
				op.print(ctx);
			}
			FlatOp::Label(l) => {
				ctx.indent -= 1;
				l.print(ctx);
				ctx.sym_(":");
				ctx.indent += 1;
			}
			FlatOp::Goto(m, l) => {
				m.print(ctx);
				ctx.word("goto");
				l.print(ctx);
			}
			FlatOp::If(m, e, l) => {
				m.print(ctx);
				ctx.word("if");
				expr::print_bool(e, ctx);
				l.print(ctx);
			}
			FlatOp::Switch(m, e, cases, default) => {
				m.print(ctx);
				ctx.word("switch");
				expr::print(e, ctx);
				ctx.block(cases, |(value, label), ctx| {
					ctx.token(value.to_string());
					ctx._sym_("=>");
					label.print(ctx);
				});
				default.print(ctx);
			}
		}
	}
}

impl Print for Op {
	fn print(&self, ctx: &mut Printer) {
		self.meta.print(ctx);
		// Setters print infix, but only if the expr is an assignment;
		// bare exprs (no trailing Ass op in the data) use the generic form.
		if matches!(self.name, "SetAttr" | "SetVar" | "SetNumReg" | "SetGlobal" | "SetCharAttr")
			&& let [lhs, Arg::Expr(expr @ Expr::Ass(..))] = self.args.as_slice()
		{
			lhs.print(ctx);
			expr::print(expr, ctx);
		} else {
			ctx.token(self.name);
			for arg in &self.args {
				arg.print(ctx);
			}
		}
	}
}

impl Print for Arg {
	fn print(&self, ctx: &mut Printer) {
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
			Arg::SystemFlags(v) => v.print(ctx),
			Arg::Expr(v) => expr::print(v, ctx),
			Arg::Text(v) => v.print(ctx),
		}
	}
}

impl Print for Label {
	fn print(&self, ctx: &mut Printer) {
		ctx._sym("$");
		ctx.token(format!("L{}", self.0));
	}
}

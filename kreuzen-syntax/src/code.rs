use kreuzen::code::{FlatOp, Label, OpMeta};
use kreuzen::decompile::{Case, Stmt};

pub mod expr;
pub mod op;

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

impl Print for Label {
	fn print(&self, ctx: &mut Printer) {
		ctx._sym("$");
		ctx.token(format!("L{}", self.0));
	}
}

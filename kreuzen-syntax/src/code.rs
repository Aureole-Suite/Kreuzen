use kreuzen::code::{FlatOp, Label, OpMeta};

pub mod expr;
pub mod op;
pub mod stmt;
pub use stmt::block;

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

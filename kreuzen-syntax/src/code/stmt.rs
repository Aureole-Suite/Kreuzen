use kreuzen::code::{Arg, Op, OpMeta};
use kreuzen::decompile::{Case, Stmt};
use kreuzen::expr::Expr;

use crate::code::expr;
use crate::{Parse, Print, Printer};

use crate::parse::TryParser;
use crate::{Error, Expect, PCtx, Parser, Result};

/// A `{ ... }` block of statements.
pub fn block(p: &mut Parser, ctx: &PCtx) -> Result<Vec<Stmt>> {
	crate::parse::parse_block(p, |p| parse_stmt(p, ctx))
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
		if self.is_empty() {
			// Occasionally we get long elseif chains with empty bodies, which look much better if split
			ctx._sym_("{");
			ctx.newline(0);
			ctx._sym_("}");
		} else {
			ctx.block(self, Stmt::print);
		}
	}
}

fn parse_stmt(p: &mut Parser, ctx: &PCtx) -> Result<Stmt> {
	let meta = p.meta().unwrap_or_default();

	p.alt()
		.test_kw("if", |p| parse_if(p, ctx, meta))
		.test_kw("while", |p| parse_while(p, ctx, meta))
		.test_kw("switch", |p| parse_switch(p, ctx, meta))
		.test_kw("break", |p| {
			if !ctx.can_break {
				let span = p.prev_span();
				p.errors.error("break outside of while/switch", span);
			}
			Ok(Stmt::Break(meta))
		})
		.test_kw("continue", |p| {
			if !ctx.can_cont {
				let span = p.prev_span();
				p.errors.error("continue outside of while", span);
			}
			Ok(Stmt::Continue(meta))
		})
		.test_kw("ForkLambda", |p| {
			let chr = p.parse()?;
			let slot = p.int()?;
			let name = p.parse()?;
			let body = block(p, &PCtx { can_break: false, can_cont: false, ..*ctx })?;
			Ok(Stmt::ForkLambda(meta, chr, slot, name, body))
		})
		.test(|p| parse_assignment(p, ctx, meta))
		.test(|p| crate::code::op::parse(p, ctx, meta).map(Stmt::Op))
		.finish()
}

fn parse_if(p: &mut Parser, ctx: &PCtx, meta: OpMeta) -> Result<Stmt> {
	let e = expr::parse(p, ctx)?;
	let then = block(p, ctx)?;

	let els = p.test(Expect::Str("else"), |p| {
		let meta2 = p.meta().unwrap_or_default();
		p.cursor.keyword("else")?;
		Ok(meta2)
	});
	let els = match els {
		Ok(meta2) => {
			let body = p
				.alt()
				.test(|p| block(p, ctx))
				.test(|p| {
					// `else if`
					let stmt = parse_stmt(p, ctx)?;
					if !matches!(stmt, Stmt::If(..)) {
						return Err(Error);
					}
					Ok(vec![stmt])
				})
				.finish()?;
			Some((meta2, body))
		}
		Err(_) => None,
	};
	Ok(Stmt::If(meta, e, then, els))
}

fn parse_while(p: &mut Parser, ctx: &PCtx, meta: OpMeta) -> Result<Stmt> {
	let e = expr::parse(p, ctx)?;
	let ctx = &PCtx { can_break: true, can_cont: true, ..*ctx };
	// While has a trailing meta, so can't use super::parse_block
	let (body, meta2) = p.delim('{', |p| {
		let mut body = Vec::new();
		let mut meta2 = OpMeta::default();
		while !p.at_end() {
			// a trailing meta before the closing brace is the loopback op's meta
			if let Ok(m) = p.test(Expect::Nt("trailing meta"), |p| {
				let m = p.meta()?;
				if p.cursor.at_end() { Ok(m) } else { Err(Error) }
			}) {
				meta2 = m;
				break;
			}
			crate::parse::parse_item(p, &mut body, |p| parse_stmt(p, ctx));
		}
		Ok((body, meta2))
	})?;
	Ok(Stmt::While(meta, e, body, meta2))
}

fn parse_switch(p: &mut Parser, ctx: &PCtx, meta: OpMeta) -> Result<Stmt> {
	let e = expr::parse(p, ctx)?;
	let ctx = &PCtx { can_break: true, ..*ctx };
	let cases = p.delim('{', |p| {
		let mut cases: Vec<(Case, Vec<Stmt>)> = Vec::new();
		while !p.at_end() {
			if let Ok(case) = p.parse() {
				cases.push((case, Vec::new()));
				continue;
			}
			if cases.is_empty() {
				cases.push((Case::None, Vec::new()));
			}
			crate::parse::parse_item(p, &mut cases.last_mut().unwrap().1, |p| parse_stmt(p, ctx));
		}
		Ok(cases)
	})?;
	Ok(Stmt::Switch(meta, e, cases))
}

// Setter ops print as `lhs = expr;`; reconstruct the op.
fn parse_assignment(p: &mut TryParser, ctx: &PCtx, meta: OpMeta) -> Result<Stmt> {
	let (name, lhs) = p
		.alt()
		.test(|p| p.parse().map(|v| ("SetAttr", Arg::Attr(v))))
		.test(|p| p.parse().map(|v| ("SetVar", Arg::Var(v))))
		.test(|p| p.parse().map(|v| ("SetNumReg", Arg::NumReg(v))))
		.test(|p| p.parse().map(|v| ("SetGlobal", Arg::Global(v))))
		.test(|p| p.parse().map(|v| ("SetCharAttr", Arg::CharAttr(v))))
		.finish()?;
	let assop = p.parse()?;
	p.commit();
	let rhs = expr::parse(p, ctx)?;

	if !ctx.spec.by_name.contains_key(name) {
		let span = p.prev_span();
		p.errors.error(format!("'{name}' does not exist in this game"), span);
	}
	Ok(Stmt::Op(Op {
		name,
		meta,
		args: vec![lhs, Arg::Expr(Expr::Ass(assop, Box::new(rhs)))],
	}))
}

/// `case N:` or `default:`. `Case::None` is implicit and never written.
impl Parse for Case {
	fn parse(p: &mut Parser) -> Result<Self> {
		p.alt()
			.test_kw("case", |p| {
				let v = p.parse()?;
				p.punct(':')?;
				Ok(Case::Case(v))
			})
			.test_kw("default", |p| {
				p.punct(':')?;
				Ok(Case::Default)
			})
			.finish()
	}
}

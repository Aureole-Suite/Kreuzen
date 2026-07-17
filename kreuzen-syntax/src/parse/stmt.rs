use kreuzen::code::{Arg, Op, OpMeta};
use kreuzen::decompile::{Case, Stmt};
use kreuzen::expr::{AssOp, Expr};

use super::alt::{Alt, TryParser};
use super::parser::{Error, Expect, Parser, Result};
use super::types::Parse;
use super::{PCtx, expr, op};

/// A `{ ... }` block of statements.
pub fn block(p: &mut Parser, ctx: &PCtx) -> Result<Vec<Stmt>> {
	p.delim('{', |p| Ok(super::parse_seq(p, |p| parse_stmt(p, ctx))))
}

fn parse_stmt(p: &mut Parser, ctx: &PCtx) -> Result<Stmt> {
	let meta = p.meta().unwrap_or_default();

	Alt::new(p)
		.test_kw("if", |p| parse_if(p, ctx, meta))
		.test_kw("while", |p| parse_while(p, ctx, meta))
		.test_kw("switch", |p| parse_switch(p, ctx, meta))
		.test_kw("break", |_| Ok(Stmt::Break(meta)))
		.test_kw("continue", |_| Ok(Stmt::Continue(meta)))
		.test_kw("ForkLambda", |p| {
			let chr = p.parse()?;
			let slot = p.int()?;
			let name = p.parse()?;
			let body = block(p, ctx)?;
			Ok(Stmt::ForkLambda(meta, chr, slot, name, body))
		})
		.test(|p| parse_assignment(p, ctx, meta))
		.test(|p| op::parse_op_named(p, ctx, meta).map(Stmt::Op))
		.finish()
}

fn parse_if(p: &mut Parser, ctx: &PCtx, meta: OpMeta) -> Result<Stmt> {
	let e = expr::parse_expr(p, ctx)?;
	let then = block(p, ctx)?;

	let els = p.test(Expect::Str("else"), |p| {
		let meta2 = p.meta().unwrap_or_default();
		p.cursor.keyword("else")?;
		Ok(meta2)
	});
	let els = match els {
		Ok(meta2) => {
			let body = Alt::new(p)
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
	let e = expr::parse_expr(p, ctx)?;
	// While has a trailing meta, so can't use super::parse_seq
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
			super::seq_item(p, &mut body, |p| parse_stmt(p, ctx));
		}
		Ok((body, meta2))
	})?;
	Ok(Stmt::While(meta, e, body, meta2))
}

fn parse_switch(p: &mut Parser, ctx: &PCtx, meta: OpMeta) -> Result<Stmt> {
	let e = expr::parse_expr(p, ctx)?;
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
			super::seq_item(p, &mut cases.last_mut().unwrap().1, |p| parse_stmt(p, ctx));
		}
		Ok(cases)
	})?;
	Ok(Stmt::Switch(meta, e, cases))
}

// Setter ops print as `lhs = expr;`; reconstruct the op.
fn parse_assignment(p: &mut TryParser, ctx: &PCtx, meta: OpMeta) -> Result<Stmt> {
	let (name, lhs) = Alt::new(p)
		.test(|p| p.parse().map(|v| ("SetAttr", Arg::Attr(v))))
		.test(|p| p.parse().map(|v| ("SetVar", Arg::Var(v))))
		.test(|p| p.parse().map(|v| ("SetNumReg", Arg::NumReg(v))))
		.test(|p| p.parse().map(|v| ("SetGlobal", Arg::Global(v))))
		.test(|p| p.parse().map(|v| ("SetCharAttr", Arg::CharAttr(v))))
		.finish()?;
	let assop = p.parse()?;
	p.commit();
	let rhs = expr::parse_expr(p, ctx)?;

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
		Alt::new(p)
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

impl Parse for AssOp {
	fn parse(p: &mut Parser) -> Result<Self> {
		use AssOp::*;
		const OPS: &[(&str, AssOp)] = &[
			("*=", MulAss),
			("/=", DivAss),
			("%=", ModAss),
			("+=", AddAss),
			("-=", SubAss),
			("&=", AndAss),
			("^=", XorAss),
			("|=", OrAss),
		];
		for (tok, assop) in OPS {
			if p.operator(tok).is_ok() {
				return Ok(*assop);
			}
		}
		// `=`, but not `==`
		p.test('=', |p| {
			p.cursor.punct('=')?;
			if p.cursor.glued_punct('=').is_ok() {
				return Err(Error);
			}
			Ok(Ass)
		})
	}
}

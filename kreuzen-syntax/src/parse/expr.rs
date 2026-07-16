use kreuzen::expr::{BinOp, Expr, UnOp};

use super::alt::Alt;
use super::parser::{Error, Parser, Result};
use super::{PCtx, op};

pub fn parse_expr(p: &mut Parser, ctx: &PCtx) -> Result<Expr> {
	let mut prio = Prio::new(parse_atom(p, ctx)?);
	while let Some(binop) = parse_binop(p) {
		prio.push(binop, parse_atom(p, ctx)?);
	}
	Ok(prio.finish())
}

struct Prio {
	ops: Vec<(BinOp, u32)>,
	stack: Vec<Expr>,
}

impl Prio {
	fn new(e: Expr) -> Self {
		Self { ops: Vec::new(), stack: vec![e] }
	}

	fn reduce(&mut self, prio: u32) {
		while let Some(&(binop, p2)) = self.ops.last()
			&& p2 >= prio
		{
			self.ops.pop();
			let b = self.stack.pop().unwrap();
			let a = self.stack.pop().unwrap();
			self.stack.push(Expr::Bin(binop, Box::new(a), Box::new(b)));
		}
	}

	fn push(&mut self, binop: BinOp, e: Expr) {
		let prio = binop_prio(binop);
		self.reduce(prio);
		self.ops.push((binop, prio));
		self.stack.push(e);
	}

	fn finish(mut self) -> Expr {
		self.reduce(0);
		assert_eq!(self.stack.len(), 1);
		self.stack.pop().unwrap()
	}
}

// Must match `binop_prio` in crate::code (the printer).
fn binop_prio(op: BinOp) -> u32 {
	use BinOp::*;
	match op {
		Mul | Div | Mod => 7,
		Add | Sub => 6,
		BitAnd => 5,
		Xor => 4,
		Or => 3,
		Eq | Ne | Lt | Gt | Le | Ge => 2,
		BoolAnd => 1,
	}
}

fn parse_binop(p: &mut Parser) -> Option<BinOp> {
	use BinOp::*;
	// Multi-char operators must come before their single-char prefixes.
	// `Or` prints as `|`, but accept `||` too.
	const OPS: &[(&str, BinOp)] = &[
		("==", Eq),
		("!=", Ne),
		("<=", Le),
		(">=", Ge),
		("&&", BoolAnd),
		("||", Or),
		("<", Lt),
		(">", Gt),
		("&", BitAnd),
		("|", Or),
		("^", Xor),
		("+", Add),
		("-", Sub),
		("*", Mul),
		("/", Div),
		("%", Mod),
	];
	for (tok, binop) in OPS {
		if p.operator(tok).is_ok() {
			return Some(*binop);
		}
	}
	None
}

fn parse_atom(p: &mut Parser, ctx: &PCtx) -> Result<Expr> {
	fn unop(op: UnOp, e: Expr) -> Expr {
		Expr::Un(op, Box::new(e))
	}
	Alt::new(p)
		.test(|p| {
			let cursor = p.delim_later('(')?;
			p.commit();
			let mut inner = Parser::new(cursor, p.errors);
			let e = parse_expr(&mut inner, ctx)?;
			if !inner.cursor.at_end() {
				return Err(Error);
			}
			Ok(e)
		})
		.test(|p| {
			p.punct('!')?;
			p.commit();
			Ok(unop(UnOp::BoolNot, parse_atom(p, ctx)?))
		})
		.test(|p| {
			p.punct('~')?;
			p.commit();
			Ok(unop(UnOp::BitNot, parse_atom(p, ctx)?))
		})
		.test(|p| {
			p.punct('-')?;
			p.commit();
			Ok(unop(UnOp::Neg, parse_atom(p, ctx)?))
		})
		.test_kw("rand", |_| Ok(Expr::Rand))
		.test(|p| p.parse().map(Expr::Flag))
		.test(|p| p.parse().map(Expr::Var))
		.test(|p| p.parse().map(Expr::Attr))
		.test(|p| p.parse().map(Expr::CharAttr))
		.test(|p| p.parse().map(Expr::Global))
		.test(|p| p.parse().map(Expr::NumReg))
		.test(|p| p.parse().map(Expr::SystemFlags))
		// An op with a meta prefix; must be tried before plain ints, which would eat the line number.
		.test(|p| {
			let meta = op::parse_meta_present(p)?;
			p.commit();
			op::parse_op_named(p, ctx, meta).map(Expr::Op)
		})
		.test(|p| p.parse().map(Expr::Int))
		.test(|p| op::parse_op_named(p, ctx, Default::default()).map(Expr::Op))
		.finish()
}

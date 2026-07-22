use kreuzen::expr::{AssOp, BinOp, Expr, UnOp};

use crate::parse::PCtx;
use crate::{Error, Parse, Parser, Print, Printer, Result};

pub fn print(e: &Expr, ctx: &mut Printer) {
	print_expr_inner(e, ctx, 0, false);
}

pub fn print_bool(e: &Expr, ctx: &mut Printer) {
	print_expr_inner(e, ctx, 0, true);
}

fn looks_boolean(e: &Expr) -> bool {
	use BinOp::*;
	match e {
		Expr::Bin(BoolAnd | Eq | Ne | Lt | Gt | Le | Ge, ..) => true,
		Expr::Bin(Or, l, r) => looks_boolean(l) || looks_boolean(r),
		Expr::Un(UnOp::BoolNot, _) => true,
		_ => false,
	}
}

fn print_expr_inner(e: &Expr, ctx: &mut Printer, prec: u32, bool_ctx: bool) {
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
			let boolean = bool_ctx || looks_boolean(a) || looks_boolean(b);
			let sym = binop_sym(*op, boolean);
			let p = binop_prio(sym);
			let child_bool_ctx = *op == BinOp::BoolAnd || sym == "||";
			if p < prec {
				ctx._sym("(");
			}
			print_expr_inner(a, ctx, p, child_bool_ctx);
			ctx._sym_(sym);
			print_expr_inner(b, ctx, p + 1, child_bool_ctx);
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
			// -(5) would otherwise be indistinguishable from a literal -5
			if matches!((op, &**a), (UnOp::Neg, Expr::Int(_))) {
				ctx.sym("(");
				print_expr_inner(a, ctx, 0, false);
				ctx.sym_(")");
			} else {
				print_expr_inner(a, ctx, 10, *op == UnOp::BoolNot);
			}
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
			print_expr_inner(a, ctx, 0, false);
		}
	}
}

// Multi-char operators must come before their single-char prefixes.
const BINOPS: &[(&str, BinOp, u32)] = &[
	("==", BinOp::Eq, 2),
	("!=", BinOp::Ne, 2),
	("<=", BinOp::Le, 2),
	(">=", BinOp::Ge, 2),
	("&&", BinOp::BoolAnd, 1),
	("||", BinOp::Or, 0),
	("<", BinOp::Lt, 2),
	(">", BinOp::Gt, 2),
	("&", BinOp::BitAnd, 5),
	("|", BinOp::Or, 3),
	("^", BinOp::Xor, 4),
	("+", BinOp::Add, 6),
	("-", BinOp::Sub, 6),
	("*", BinOp::Mul, 7),
	("/", BinOp::Div, 7),
	("%", BinOp::Mod, 7),
];

fn binop_sym(op: BinOp, boolean: bool) -> &'static str {
	use BinOp::*;
	match op {
		Mul => "*",
		Div => "/",
		Mod => "%",
		Add => "+",
		Sub => "-",
		BitAnd => "&",
		Xor => "^",
		Or if boolean => "||",
		Or => "|",
		Eq => "==",
		Ne => "!=",
		Lt => "<",
		Gt => ">",
		Le => "<=",
		Ge => ">=",
		BoolAnd => "&&",
	}
}

fn binop_prio(sym: &str) -> u32 {
	BINOPS.iter().find(|(tok, ..)| *tok == sym).unwrap().2
}

pub fn parse(p: &mut Parser, ctx: &PCtx) -> Result<Expr> {
	let mut prio = Prio::new(parse_atom(p, ctx)?);
	while let Ok((binop, p2)) = parse_binop(p) {
		prio.push(binop, p2, parse_atom(p, ctx)?);
	}
	Ok(prio.finish())
}

fn parse_binop(p: &mut Parser) -> Result<(BinOp, u32)> {
	for (tok, binop, prio) in BINOPS {
		if p.operator(tok).is_ok() {
			return Ok((*binop, *prio));
		}
	}
	Err(Error)
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

	fn push(&mut self, binop: BinOp, prio: u32, e: Expr) {
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

impl Parse for UnOp {
	fn parse(p: &mut Parser) -> Result<Self> {
		p.alt()
			.test(|p| p.punct('!').map(|_| UnOp::BoolNot))
			.test(|p| p.punct('~').map(|_| UnOp::BitNot))
			.test(|p| p.punct('-').map(|_| UnOp::Neg))
			.finish()
	}
}

fn parse_atom(p: &mut Parser, ctx: &PCtx) -> Result<Expr> {
	p.alt()
		.test(|p| p.delim('(', |p| parse(p, ctx)))
		.test(|p| {
			let unop = p.parse()?;
			p.commit();
			Ok(Expr::Un(unop, Box::new(parse_atom(p, ctx)?)))
		})
		.test_kw("rand", |_| Ok(Expr::Rand))
		.test(|p| p.parse().map(Expr::Flag))
		.test(|p| p.parse().map(Expr::Var))
		.test(|p| p.parse().map(Expr::Attr))
		.test(|p| p.parse().map(Expr::CharAttr))
		.test(|p| p.parse().map(Expr::Global))
		.test(|p| p.parse().map(Expr::NumReg))
		.test(|p| p.parse().map(Expr::SystemFlags))
		.test(|p| p.parse().map(Expr::Int))
		.test(|p| {
			let meta = p.meta().unwrap_or_default();
			super::op::parse(p, ctx, meta).map(Expr::Op)
		})
		.finish()
}

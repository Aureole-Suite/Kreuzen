use rootcause::prelude::ResultExt;

use crate::code::{Arg, Code, Op, OpMeta};
use crate::decompile::Stmt;
use crate::expr::Expr;
use crate::types::Char;

#[derive(Debug, Clone, PartialEq)]
pub struct Shadow {
	pub line: u16,
	pub ops: Vec<ShadowOp>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ShadowOp {
	Call { table: i64, name: String },
	CharAni { chr: Char, strings: Vec<String> },
	Fork { chr: Char, slot: u8, name: String, flags: i64 },
}

pub fn parse(code: &Code) -> rootcause::Result<Shadow> {
	let stmts = crate::decompile::decompile(code)?;
	let mut iter = stmts.iter();
	crate::ensure!(matches!(iter.next_back(), Some(Stmt::Op(op)) if op.name == "return"));
	let stmts: Vec<&Stmt> = iter.collect();

	let line = stmts
		.first()
		.map(|s| match s {
			Stmt::Op(op) => op.meta.line,
			Stmt::If(meta, ..) => meta.line,
			_ => 0,
		})
		.unwrap_or(0);

	let mut items = Vec::new();
	for stmt in stmts {
		items.push(parse_one(stmt).context_with(|| format!("parsing {stmt:?}"))?);
	}

	Ok(Shadow { line, ops: items })
}

fn parse_one(stmt: &Stmt) -> rootcause::Result<ShadowOp> {
	match stmt {
		Stmt::Op(op) if op.name == "call" => {
			crate::ensure!(let [Arg::Int(table), Arg::Str(name)] = op.args.as_slice());
			Ok(ShadowOp::Call { table: *table, name: name.clone() })
		}
		Stmt::If(_, expr, body, None) => {
			crate::ensure!(let Expr::Op(cond) = expr);
			crate::ensure!(cond.name == "CharExists");
			crate::ensure!(let [Arg::Char(chr)] = cond.args.as_slice());
			let chr = *chr;
			match body.as_slice() {
				_ if body.iter().all(|s| matches!(s, Stmt::Op(op) if op.name == "CharAniPreload")) => {
					let mut strings = Vec::new();
					for s in body {
						let Stmt::Op(op) = s else { unreachable!() };
						crate::ensure!(let [Arg::Char(c), Arg::Int(slot), rest @ ..] = op.args.as_slice());
						crate::ensure!(*c == chr && *slot == 0 && rest.len() == 16);
						crate::ensure!(rest[11..].iter().all(|a| matches!(a, Arg::Str(s) if s.is_empty())));
						for arg in &rest[..11] {
							crate::ensure!(let Arg::Str(s) = arg);
							strings.push(s.clone());
						}
					}
					while strings.last().is_some_and(|s| s.is_empty()) {
						strings.pop();
					}
					Ok(ShadowOp::CharAni { chr, strings })
				}
				[Stmt::Op(fork), Stmt::Op(wait)] if fork.name == "Fork" && wait.name == "ForkWait" => {
					crate::ensure!(let [Arg::Char(fc), Arg::Int(slot), Arg::Str(name), Arg::Int(flags)] = fork.args.as_slice());
					crate::ensure!(let [Arg::Char(wc), Arg::Int(wslot)] = wait.args.as_slice());
					crate::ensure!(fc == &chr && wc == &chr && slot == wslot);
					let slot = u8::try_from(*slot).context_with(|| format!("Fork slot {slot} out of u8 range"))?;
					Ok(ShadowOp::Fork { chr, slot, name: name.clone(), flags: *flags })
				}
				_ => rootcause::bail!("unrecognized if body: {body:?}"),
			}
		}
		_ => rootcause::bail!("unrecognized statement: {stmt:?}"),
	}
}

pub fn flatten(shadows: &Shadow) -> Code {
	let mut stmts: Vec<Stmt> = shadows.ops.iter().map(|s| flatten_one(s, shadows.line)).collect();
	stmts.push(Stmt::Op(Op {
		name: "return",
		meta: OpMeta::default(),
		args: vec![],
	}));
	crate::decompile::compile(&stmts).expect("is valid code")
}

fn flatten_one(shadow: &ShadowOp, line: u16) -> Stmt {
	let meta = OpMeta { line, width: 0 };
	let cond = |chr: Char, body: Vec<Stmt>| {
		Stmt::If(
			meta,
			Expr::Op(Op {
				name: "CharExists",
				meta,
				args: vec![Arg::Char(chr)],
			}),
			body,
			None,
		)
	};
	match shadow {
		ShadowOp::Call { table, name } => Stmt::Op(Op {
			name: "call",
			meta: OpMeta { line, width: 1 },
			args: vec![Arg::Int(*table), Arg::Str(name.clone())],
		}),
		ShadowOp::CharAni { chr, strings } => {
			let body = strings
				.chunks(11)
				.map(|chunk| {
					let mut args = vec![Arg::Char(*chr), Arg::Int(0)];
					args.extend(chunk.iter().map(|s| Arg::Str(s.clone())));
					args.resize(2 + 16, Arg::Str(String::new()));
					Stmt::Op(Op { name: "CharAniPreload", meta, args })
				})
				.collect();
			cond(*chr, body)
		}
		ShadowOp::Fork { chr, slot, name, flags } => {
			let slot_i = i64::from(*slot);
			let body = vec![
				Stmt::Op(Op {
					name: "Fork",
					meta: OpMeta { line, width: 1 },
					args: vec![Arg::Char(*chr), Arg::Int(slot_i), Arg::Str(name.clone()), Arg::Int(*flags)],
				}),
				Stmt::Op(Op {
					name: "ForkWait",
					meta,
					args: vec![Arg::Char(*chr), Arg::Int(slot_i)],
				}),
			];
			cond(*chr, body)
		}
	}
}

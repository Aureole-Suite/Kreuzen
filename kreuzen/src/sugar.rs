use std::collections::VecDeque;

use crate::{Entry, Scena};
use crate::func::{Arg, Op, Stmt};

pub fn resugar(scena: Scena) -> Scena {
	let mut deque = VecDeque::from(scena.entries);
	let mut out = Vec::new();

	while let Some(mut entry) = deque.pop_front() {
		if let Entry::Func(func) = &mut entry.1 {
			resugar_inner(&mut deque, func);
		}
		out.push(entry);
	}

	Scena { entries: out, ..scena }
}

fn resugar_inner(deq: &mut VecDeque<(String, Entry)>, stmts: &mut [Stmt]) {
	for stmt in stmts {
		match stmt {
			Stmt::If(_, _, then, els) => {
				resugar_inner(deq, then);
				if let Some((_, els)) = els {
					resugar_inner(deq, els);
				}
			}
			Stmt::While(_, _, body, _) => {
				resugar_inner(deq, body);
			}
			Stmt::Break(_) => {}
			Stmt::Continue(_) => {}
			Stmt::Switch(_, _, cases) => {
				for (_, body) in cases {
					resugar_inner(deq, body);
				}
			}
			Stmt::ForkLambda(..) => panic!("can't resugar twice"),

			Stmt::Op(op) => {
				if op.name == "Fork"
				&& let [Arg::Char(c), Arg::U8(n), Arg::Str(ref name), Arg::U8(11)] = op.args[..]
				&& name.starts_with("_Lambda_") {
					if let Some((name2, Entry::Func(_))) = deq.front()
						&& name2 == name
					{
						let Some((_, Entry::Func(func))) = deq.pop_front() else {
							unreachable!();
						};
						*stmt = Stmt::ForkLambda(op.meta, c, n, func);
					} else {
						tracing::warn!("Fork with name {name} but no matching function in the queue");
					}
				}
			}
		}
	}
}

struct DesugarContext {
	entries: Vec<(String, Entry)>,
	lambda_count: u32,
}

pub fn desugar(scena: Scena) -> Scena {
	let mut ctx = DesugarContext {
		entries: Vec::new(),
		lambda_count: 0,
	};
	for mut entry in scena.entries {
		if let Entry::Func(func) = &mut entry.1 {
			let pos = ctx.entries.len();
			ctx.desugar(func);
			ctx.entries.insert(pos, entry);
		} else {
			ctx.entries.push(entry);
		}
	}
	let entries = ctx.entries;
	Scena { entries, ..scena }
}

impl DesugarContext {
	fn desugar(&mut self, stmts: &mut [Stmt]) {
		for stmt in stmts {
			match stmt {
				Stmt::If(_, _, then, els) => {
					self.desugar(then);
					if let Some((_, els)) = els {
						self.desugar(els);
					}
				}
				Stmt::While(_, _, body, _) => {
					self.desugar(body);
				}
				Stmt::Break(_) => {}
				Stmt::Continue(_) => {}
				Stmt::Switch(_, _, cases) => {
					for (_, body) in cases {
						self.desugar(body);
					}
				}
				Stmt::Op(_) => {}

				Stmt::ForkLambda(meta, char, n, stmts) => {
					let name = format!("_Lambda_{}", self.lambda_count);
					self.lambda_count += 1;

					self.entries.push((name.clone(), Entry::Func(std::mem::take(stmts))));

					*stmt = Stmt::Op(Op {
						name: "Fork",
						meta: *meta,
						args: vec![
							Arg::Char(*char),
							Arg::U8(*n),
							Arg::Str(name),
							Arg::U8(11),
						],
					});
				}
			}
		}
	}
}


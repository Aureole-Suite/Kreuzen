//! Merge forked `_Lambda_` functions into the statement that forks them.
//!
//! Assumes every lambda is forked exactly once, and that each lambda chunk sits
//! immediately after the chunk that forks it.
use std::collections::HashMap;

use crate::code::{Arg, Op, OpMeta};
use crate::decompile::Stmt;
use crate::{Chunk, Function, Scena};

pub fn resugar(scena: &mut Scena) -> rootcause::Result<()> {
	let mut lambdas = HashMap::new();
	let mut chunks = Vec::with_capacity(scena.chunks.len());
	for chunk in scena.chunks.drain(..) {
		match chunk {
			Chunk::Function { function } if function.name.starts_with("_Lambda_") => {
				let name = function.name.clone();
				crate::ensure!(
					lambdas.insert(name.clone(), function).is_none(),
					"duplicate lambda {name}"
				);
			}
			chunk => chunks.push(chunk),
		}
	}
	scena.chunks = chunks;

	for chunk in &mut scena.chunks {
		let Chunk::Function { function } = chunk else { continue };
		merge_forks(&mut function.body, &mut lambdas)?;
	}
	crate::ensure!(
		lambdas.is_empty(),
		"lambdas are never forked: {:?}",
		lambdas.keys().collect::<Vec<_>>()
	);
	Ok(())
}

fn merge_forks(body: &mut [Stmt], lambdas: &mut HashMap<String, Function>) -> rootcause::Result<()> {
	for stmt in crate::decompile::leaves_mut(body) {
		if let Stmt::Op(op) = stmt
			&& op.name == "Fork"
			&& let [.., Arg::Str(name), Arg::Int(11)] = op.args.as_slice()
			&& name.starts_with("_Lambda_")
		{
			crate::ensure!(
				let [Arg::Char(chr), Arg::Int(slot), _, _] = op.args.as_slice(),
				"unexpected args in fork of {name}: {:?}", op.args
			);
			crate::ensure!(
				let Some(mut lambda) = lambdas.remove(name.as_str()),
				"{name} is missing or forked more than once"
			);
			let (meta, chr, slot) = (op.meta, *chr, *slot);
			merge_forks(&mut lambda.body, lambdas)?;
			*stmt = Stmt::ForkLambda(meta, chr, slot, lambda);
		}
	}
	Ok(())
}

pub fn desugar(scena: &mut Scena) -> rootcause::Result<()> {
	let mut chunks = Vec::with_capacity(scena.chunks.len());
	for chunk in scena.chunks.drain(..) {
		match chunk {
			Chunk::Function { mut function } => {
				let mut lambdas = Vec::new();
				extract_forks(&mut function.body, &mut lambdas);
				chunks.push(Chunk::Function { function });
				chunks.extend(lambdas.into_iter().map(|function| Chunk::Function { function }));
			}
			chunk => chunks.push(chunk),
		}
	}
	scena.chunks = chunks;
	Ok(())
}

fn extract_forks(body: &mut [Stmt], out: &mut Vec<Function>) {
	for stmt in crate::decompile::leaves_mut(body) {
		if matches!(stmt, Stmt::ForkLambda(..)) {
			let placeholder = Stmt::Break(OpMeta::default());
			let Stmt::ForkLambda(meta, chr, slot, mut lambda) = std::mem::replace(stmt, placeholder) else {
				unreachable!()
			};
			*stmt = Stmt::Op(Op {
				name: "Fork",
				meta,
				args: vec![Arg::Char(chr), Arg::Int(slot), Arg::Str(lambda.name.clone()), Arg::Int(11)],
			});

			let mut nested = Vec::new();
			extract_forks(&mut lambda.body, &mut nested);
			out.push(lambda);
			out.extend(nested);
		}
	}
}

//! Merge forked `_Lambda_` functions into the statement that forks them.
//!
//! The lambda's body is inlined into the forking `Stmt::ForkLambda`, and its shadow
//! section into the `ShadowOp::ForkLambda` that forks it from the parent's shadow.
//!
//! Assumes every lambda is forked exactly once, and that each lambda chunk sits
//! immediately after the chunk that forks it.
use std::collections::HashMap;

use crate::code::shadow::{Shadow, ShadowOp};
use crate::code::{Arg, Op, OpMeta};
use crate::decompile::Stmt;
use crate::{Chunk, Function, Scena};

pub fn resugar(scena: &mut Scena) -> rootcause::Result<()> {
	let mut lambdas = HashMap::new();
	let mut chunks = Vec::with_capacity(scena.chunks.len());
	for chunk in scena.chunks.drain(..) {
		match chunk {
			Chunk::Function(function) if function.name.starts_with("_Lambda_") => {
				let name = function.name.clone();
				crate::ensure!(lambdas.insert(name.clone(), function).is_none(), "duplicate lambda {name}");
			}
			chunk => chunks.push(chunk),
		}
	}
	scena.chunks = chunks;

	for chunk in &mut scena.chunks {
		let Chunk::Function(function) = chunk else { continue };
		merge_function(function, &mut lambdas)?;
	}
	crate::ensure!(lambdas.is_empty(), "lambdas are never forked: {:?}", lambdas.keys().collect::<Vec<_>>());
	Ok(())
}

fn merge_function(function: &mut Function, lambdas: &mut HashMap<String, Function>) -> rootcause::Result<()> {
	// shadow sections of body-forked lambdas, to be claimed by this function's shadow section
	let mut shadows = HashMap::new();

	for stmt in crate::decompile::leaves_mut(&mut function.body) {
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
			merge_function(&mut lambda, lambdas)?;
			crate::ensure!(lambda.preload.is_empty(), "lambda {} has a preload section", lambda.name);
			if function.shadow.is_empty() {
				if !lambda.shadow.is_empty() {
					// a1005:TK_Temp, a debug script, has a shadow on a lambda despite its host having none
					tracing::warn!("lambda {} has a shadow section, but its host does not", lambda.name);
				}
			} else {
				crate::ensure!(
					let Some(shadow_section) = lambda.shadow.pop(),
					"lambda {} has no shadow section", lambda.name
				);
				crate::ensure!(lambda.shadow.is_empty(), "lambda {} has more than one shadow section", lambda.name);
				shadows.insert(lambda.name.clone(), shadow_section);
			}
			*stmt = Stmt::ForkLambda(meta, chr, slot, lambda.name, lambda.body);
		}
	}

	for shadow in &mut function.shadow {
		let line = shadow.line;
		for op in &mut shadow.ops {
			if let ShadowOp::Fork { chr, slot, name, flags } = op
				&& name.starts_with("_Lambda_")
			{
				crate::ensure!(*flags == 11, "unexpected flags {flags} in shadow fork of {name}");
				crate::ensure!(
					let Some(inner) = shadows.remove(name.as_str()),
					"{name} is not forked from this function's body, or referenced twice"
				);
				crate::ensure!(inner.line == line, "shadow of {name} has line {}, expected {line}", inner.line);
				*op = ShadowOp::ForkLambda {
					chr: *chr,
					slot: *slot,
					name: std::mem::take(name),
					ops: inner.ops,
				};
			}
		}
	}

	// unreferenced shadow sections must be empty
	for (name, shadow) in shadows {
		crate::ensure!(shadow.ops.is_empty(), "shadow section of {name} ({shadow:?}) is never referenced");
	}
	Ok(())
}

pub fn desugar(scena: &mut Scena) -> rootcause::Result<()> {
	let mut chunks = Vec::with_capacity(scena.chunks.len());
	for chunk in scena.chunks.drain(..) {
		match chunk {
			Chunk::Function(mut function) => {
				let mut lambdas = Vec::new();
				extract_function(&mut function, &mut lambdas)?;
				chunks.push(Chunk::Function(function));
				chunks.extend(lambdas.into_iter().map(Chunk::Function));
			}
			chunk => chunks.push(chunk),
		}
	}
	scena.chunks = chunks;
	Ok(())
}

fn extract_function(function: &mut Function, out: &mut Vec<Function>) -> rootcause::Result<()> {
	// reclaim inlined shadow sections before rebuilding the lambdas themselves
	let mut shadows = HashMap::new();
	for shadow in &mut function.shadow {
		let line = shadow.line;
		for op in &mut shadow.ops {
			if matches!(op, ShadowOp::ForkLambda { .. }) {
				let placeholder = ShadowOp::Call { table: 0, name: String::new() };
				let ShadowOp::ForkLambda { chr, slot, name, ops } = std::mem::replace(op, placeholder) else {
					unreachable!()
				};
				*op = ShadowOp::Fork { chr, slot, name: name.clone(), flags: 11 };
				crate::ensure!(
					shadows.insert(name.clone(), Shadow { line, ops }).is_none(),
					"{name} is referenced from more than one shadow"
				);
			}
		}
	}

	for stmt in crate::decompile::leaves_mut(&mut function.body) {
		if matches!(stmt, Stmt::ForkLambda(..)) {
			let placeholder = Stmt::Break(OpMeta::default());
			let Stmt::ForkLambda(meta, chr, slot, name, body) = std::mem::replace(stmt, placeholder) else {
				unreachable!()
			};
			*stmt = Stmt::Op(Op {
				name: "Fork",
				meta,
				args: vec![Arg::Char(chr), Arg::Int(slot), Arg::Str(name.clone()), Arg::Int(11)],
			});
			let shadow = match shadows.remove(&name) {
				Some(shadow) => vec![shadow],
				// hosts without a shadow section produce lambdas without one
				None if function.shadow.is_empty() => Vec::new(),
				// otherwise unreferenced lambdas keep a single empty shadow section (which has line 0)
				None => vec![Shadow::default()],
			};
			let mut lambda = Function { name, body, preload: Vec::new(), shadow };

			let mut nested = Vec::new();
			extract_function(&mut lambda, &mut nested)?;
			out.push(lambda);
			out.extend(nested);
		}
	}

	crate::ensure!(
		shadows.is_empty(),
		"shadow sections of lambdas that are not forked from the body: {:?}",
		shadows.keys().collect::<Vec<_>>()
	);
	Ok(())
}

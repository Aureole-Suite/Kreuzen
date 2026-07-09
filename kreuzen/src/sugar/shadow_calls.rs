//! Replace `call 11 _aN_SelfName` with a `CallShadow N` pseudo-op, and strip the
//! `_a0_` prefix from names referenced inside shadows.
use crate::code::Arg;
use crate::code::shadow::{ShadowOp, parse_name};
use crate::decompile::Stmt;
use crate::{Chunk, Scena};

pub fn resugar(scena: &mut Scena) -> rootcause::Result<()> {
	for chunk in &mut scena.chunks {
		let Chunk::Function { function } = chunk else { continue };
		for stmt in crate::decompile::leaves_mut(&mut function.body) {
			if let Stmt::Op(op) = stmt
				&& op.name == "call"
				&& let [Arg::Int(11), Arg::Str(name)] = op.args.as_slice()
				&& let Some((idx, owner)) = parse_name(name)
			{
				crate::ensure!(
					owner == function.name,
					"{} calls shadow {name}, which is not its own", function.name
				);
				op.name = "CallShadow";
				op.args = vec![Arg::Int(idx as i64)];
			}
		}

		for shadow in &mut function.shadow {
			for op in &mut shadow.ops {
				if let ShadowOp::Call { name, .. } | ShadowOp::Fork { name, .. } = op {
					crate::ensure!(
						let Some((0, base)) = parse_name(name),
						"shadow references {name}, which does not start with _a0_"
					);
					*name = base.to_owned();
				}
			}
		}
	}
	Ok(())
}

pub fn desugar(scena: &mut Scena) -> rootcause::Result<()> {
	for chunk in &mut scena.chunks {
		let Chunk::Function { function } = chunk else { continue };
		for stmt in crate::decompile::leaves_mut(&mut function.body) {
			if let Stmt::Op(op) = stmt
				&& op.name == "CallShadow"
			{
				crate::ensure!(let [Arg::Int(idx)] = op.args.as_slice());
				let idx = *idx;
				op.name = "call";
				op.args = vec![Arg::Int(11), Arg::Str(format!("_a{idx}_{}", function.name))];
			}
		}

		for shadow in &mut function.shadow {
			for op in &mut shadow.ops {
				if let ShadowOp::Call { name, .. } | ShadowOp::Fork { name, .. } = op {
					*name = format!("_a0_{name}");
				}
			}
		}
	}
	Ok(())
}

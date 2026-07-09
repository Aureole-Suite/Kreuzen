//! Work around a presumed bug in Falcom's compiler where shadows are misattributed to lambdas.
use std::collections::HashMap;

use crate::code::Arg;
use crate::code::shadow::{Shadow, parse_name};
use crate::decompile::Stmt;
use crate::{Chunk, Scena};

pub fn resugar(scena: &mut Scena) -> rootcause::Result<()> {
	let mut shadows = HashMap::new();
	let mut moves = Vec::new();
	for chunk in &mut scena.chunks {
		let Chunk::Function { function } = chunk else { continue };
		shadows.insert(function.name.clone(), &mut function.shadow);

		for stmt in crate::decompile::leaves_mut(&mut function.body) {
			if let Stmt::Op(op) = stmt
				&& op.name == "call"
				&& let [Arg::Int(11), Arg::Str(name)] = op.args.as_mut_slice()
			{
				moves.push((function.name.clone(), name));
			}
		}
	}

	move_shadows(moves, shadows)?;
	Ok(())
}

pub fn desugar(scena: &mut Scena) -> rootcause::Result<()> {
	let mut shadows = HashMap::new();
	let mut moves = Vec::new();
	for chunk in &mut scena.chunks {
		let Chunk::Function { function } = chunk else { continue };
		shadows.insert(function.name.clone(), &mut function.shadow);

		let mut current = function.name.clone();
		for stmt in crate::decompile::leaves_mut(&mut function.body) {
			if let Stmt::Op(op) = stmt
				&& op.name == "Fork"
				&& let [.., Arg::Str(name), Arg::Int(11)] = op.args.as_slice()
				&& name.starts_with("_Lambda_")
			{
				current = name.clone();
			}

			if let Stmt::Op(op) = stmt
				&& op.name == "call"
				&& let [Arg::Int(11), Arg::Str(name)] = op.args.as_mut_slice()
			{
				moves.push((current.to_owned(), name));
			}
		}
	}
	move_shadows(moves, shadows)?;
	Ok(())
}

fn move_shadows(moves: Vec<(String, &mut String)>, mut shadows: HashMap<String, &mut Vec<Shadow>>) -> rootcause::Result<()> {
	let mut dest_for = HashMap::<String, String>::new();
	let mut order = Vec::new();
	for (dest, src) in &moves {
		let src = src.as_str();
		let Some((idx, owner)) = parse_name(src) else { continue };
		if !dest_for.contains_key(src) {
			dest_for.insert(src.to_owned(), dest.clone());
			order.push((src.to_owned(), idx, owner.to_owned()));
		}
	}

	let mut by_owner = HashMap::<&String, Vec<usize>>::new();
	for (src, idx, owner) in &order {
		if dest_for[src] != *owner {
			by_owner.entry(owner).or_default().push(*idx);
		}
	}

	let mut taken = HashMap::<(String, usize), Shadow>::new();
	for (owner, mut idxs) in by_owner {
		crate::ensure!(
			let Some(list) = shadows.get_mut(owner.as_str()),
			"moving shadows out of nonexistent function {owner}"
		);
		idxs.sort_unstable();
		crate::ensure!(
			idxs.len() <= list.len() && idxs.iter().copied().eq(list.len() - idxs.len()..list.len()),
			"moved shadows {idxs:?} are not a tail of {owner}'s {} shadows",
			list.len()
		);
		let start = list.len() - idxs.len();
		for (i, shadow) in list.split_off(start).into_iter().enumerate() {
			taken.insert((owner.clone(), start + i), shadow);
		}
	}

	let mut rename = HashMap::<String, String>::new();
	for (src, idx, owner) in order {
		let dest = &dest_for[&src];
		if *dest == owner {
			continue;
		}
		let shadow = taken.remove(&(owner, idx)).expect("was taken above");
		crate::ensure!(
			let Some(list) = shadows.get_mut(dest.as_str()),
			"moving shadow {src} to nonexistent function {dest}"
		);
		rename.insert(src, format!("_a{}_{dest}", list.len()));
		list.push(shadow);
	}

	for (_, name) in moves {
		if let Some(new) = rename.get(name.as_str()) {
			name.clone_from(new);
		}
	}
	Ok(())
}

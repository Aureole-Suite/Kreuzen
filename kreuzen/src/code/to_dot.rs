use std::collections::BTreeMap;
use std::fmt::Write;

use crate::code::{FlatOp, Label};

pub fn to_dot(ops: &[FlatOp]) -> String {
	let mut labels: BTreeMap<Label, usize> = BTreeMap::new();
	for (i, stmt) in ops.iter().enumerate() {
		if let FlatOp::Label(l) = stmt {
			labels.insert(*l, i);
		}
	}

	struct State<'a> {
		nodes: Vec<String>,
		edges: Vec<String>,
		pos: usize,
		ops: &'a [FlatOp],
	}
	let mut state = State {
		nodes: Vec::new(),
		edges: Vec::new(),
		pos: 0,
		ops,
	};

	impl<'a> State<'a> {
		fn flush(&mut self, until: usize, shape: &str) {
			let pos = self.pos;
			if until == pos {
				return;
			}
			if let Some(FlatOp::Label(l)) = self.ops.get(pos) {
				self.nodes.push(format!("n{pos} [xlabel=n{pos}, label=\"{l}\", shape={shape}]"));
			} else {
				self.nodes.push(format!("n{pos} [xlabel=n{pos}, label=\"\", shape={shape}]"));
			}
			self.pos = until;
		}
	}

	for (i, stmt) in ops.iter().enumerate() {
		let pos = state.pos;
		match stmt {
			FlatOp::Label(_) => {
			}
			FlatOp::Op(_) => {
				if matches!(ops.get(i+1), Some(FlatOp::Label(..))) {
					state.edges.push(format!("n{pos} -> n{until} [color=black]", until = i + 1));
					state.flush(i + 1, "box");
				}
			}
			FlatOp::If(_, _, l) => {
				state.edges.push(format!("n{pos} -> n{until} [color=green]", until = i + 1));
				state.edges.push(format!("n{pos} -> n{} [color=red]", labels[l]));
				state.flush(i + 1, "diamond");
			}
			FlatOp::Switch(_, _, cs, d) => {
				for (v, l) in cs {
					state.edges.push(format!("n{pos} -> n{} [color=orange, label=\"{v}\"]", labels[l]));
				}
				state.edges.push(format!("n{pos} -> n{} [color=blue]", labels[d]));
				state.flush(i + 1, "octagon");
			}
			FlatOp::Goto(_, l) => {
				state.edges.push(format!("n{pos} -> n{} [color=black, style=dashed]", labels[l]));
				state.flush(i + 1, "oval");
			}
		}
	}
	state.flush(ops.len(), "box");

	let mut out = String::new();
	writeln!(out, "digraph {{").unwrap();
	writeln!(out, "    node [shape=box fontname=\"monospace\"]").unwrap();
	for node in state.nodes {
		writeln!(out, "    {node}").unwrap();
	}
	for edge in state.edges {
		writeln!(out, "    {edge}").unwrap();
	}
	writeln!(out, "}}").unwrap();
	out
}


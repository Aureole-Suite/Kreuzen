use rootcause::option_ext::OptionExt as _;
use rootcause::prelude::{IteratorExt as _, ResultExt as _};
use std::collections::{BTreeMap, BTreeSet};
use std::fmt::Write;

use crate::spec::{Op, Opcode, Part, Spec};
use crate::Game;

pub type Lines = BTreeMap<Opcode, (String, Vec<Part>)>;

pub fn parse_lines(name: &str) -> Lines {
	match try_parse_lines(name) {
		Ok(lines) => lines,
		Err(e) => {
			eprintln!("{e}");
			std::process::exit(1);
		}
	}
}

pub fn try_parse_lines(name: &str) -> rootcause::Result<Lines> {
	let text = super::text_for(name).context_with(|| format!("unknown spec: {name}"))?;
	let mut ops = BTreeMap::new();
	let mut add = |code: Opcode, name: String, parts: Vec<Part>| {
		assert!(!ops.contains_key(&code), "Duplicate code in spec: {code} and {name}");
		ops.insert(code, (name, parts));
	};
	() = text
		.lines()
		.map(|line| parse_line(line, &mut add).context_with(|| format!("error parsing line: {line:?}")))
		.collect_reports()
		.context_with(|| format!("error parsing spec: {name}"))?;
	Ok(ops)
}

fn parse_line(line0: &str, add: &mut impl FnMut(Opcode, String, Vec<Part>)) -> rootcause::Result<()> {
	let line = line0.split('#').next().unwrap().trim();
	let mut words = line.split_whitespace();
	let Some(first) = words.next() else {
		return Ok(());
	};
	if first == "import" {
		let from = words.next().context("import missing source")?;
		let range = words.next().context("import missing range")?;
		assert!(words.next().is_none());
		let (a, b) = range.split_once("..").context("invalid import range")?;
		let a = a.parse::<Opcode>().context("invalid import range start")?;
		let b = b.parse::<Opcode>().context("invalid import range end")?;
		let include = super::lines_for(from).context_with(|| format!("unknown import source: {from}"))?;
		for (code, (name, parts)) in include.range(a..b) {
			add(*code, name.clone(), parts.clone());
		}
	} else if let Ok(code) = first.parse() {
		let mut name = String::new();
		let mut parts = Vec::<Part>::new();
		for word in words {
			if word.starts_with('\'') && word.ends_with('\'') {
				name.push_str(&word[1..word.len() - 1])
			} else {
				parts.push(word.parse().context_with(|| format!("invalid part: {word}"))?);
			};
		}
		add(code, name, parts);
	} else {
		rootcause::bail!("invalid line start: {first}");
	}
	Ok(())
}

pub fn parse_spec(game: Game, ops: &Lines) -> Spec {
	Spec { game, ops: build_ops(ops), by_name: build_names(ops) }
}

fn build_ops(ops: &Lines) -> [Option<Op>; 256] {
	let mut out = std::array::from_fn(|_| None);
	for (k, (name, parts)) in ops {
		assert!(!k.is_empty(), "Empty code in spec");
		let mut op = out[k[0] as usize].get_or_insert_with(Op::default);
		for byte in k.iter().skip(1) {
			if op.child_keys.last().is_none_or(|last| last < byte) {
				op.child_keys.push(*byte);
				op.children.push(Op::default());
			}
			op = op.children.last_mut().unwrap();
		}
		op.name = name.clone();
		op.parts = parts.clone();
	}
	for (i, op) in out.iter_mut().enumerate() {
		if let Some(op) = op {
			fill_name(op, i as u8, "op", false);
		}
	}
	out
}

fn fill_name(op: &mut Op, byte: u8, prefix: &str, parent_has_name: bool) {
	let has_name = !op.name.is_empty();
	if !has_name {
		if parent_has_name {
			op.name = format!("{}_{:02X}", prefix, byte);
		} else {
			op.name = format!("{}{:02X}", prefix, byte);
		}
	}
	for (child_key, child) in op.child_keys.iter().zip(op.children.iter_mut()) {
		fill_name(child, *child_key, &op.name, has_name);
	}
}

fn build_names(inp: &BTreeMap<Opcode, (String, Vec<Part>)>) -> BTreeMap<String, Opcode> {
	let mut all = BTreeSet::new();
	let mut leaves = BTreeSet::new();
	for op in inp.keys() {
		for p in op.prefixes() {
			leaves.remove(&p);
			all.insert(p);
		}
		leaves.insert(*op);
	}

	let mut by_name = BTreeMap::new();
	let mut put = |op: Opcode, mut name: String| {
		if leaves.contains(&op) {
			if let Some(prev) = by_name.insert(name.clone(), op) {
				panic!("Duplicate name in spec: {prev} and {op} are both named {name}");
			}
		} else {
			name.push('_');
		}
	};

	for op in all {
		let mut s = String::from("op");
		for b in op {
			write!(s, "{b:02X}").unwrap();
		}
		put(op, s);
	}

	for op in inp.keys() {
		for p in op.prefixes() {
			if let Some((s, _)) = inp.get(&p)
				&& !s.is_empty()
			{
				let mut s = s.clone();
				if p.len() < op.len() {
					s.push('_');
					for b in &op[p.len()..] {
						write!(s, "{b:02X}").unwrap();
					}
				}
				put(*op, s);
			}
		}
	}

	by_name
}

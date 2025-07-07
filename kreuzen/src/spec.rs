use std::sync::LazyLock;
use trie_rs::map::{Trie, TrieBuilder};

#[derive(Debug, Clone, PartialEq, Eq, derive_more::FromStr)]
pub enum Part {
	U8,
	U16,
	U32,
	I8,
	I16,
	I32,
	F32,
	Str,
	Expr,
	Text,
	Dyn,
	Dyn2,
	Ndyn,
	_3E,
	_40,
}

#[derive(Debug, Clone)]
pub struct Spec {
	pub names: Trie<u8, String>,
	pub ops: Trie<u8, Vec<Part>>,
}

pub static SPEC: LazyLock<Spec> = LazyLock::new(|| parse_spec(include_str!("../../ed85.txt")));

fn parse_line(line: &str) -> Option<(Vec<u8>, String, Vec<Part>)> {
	let mut words = line.split_whitespace();

	let codeword = words.next().unwrap();
	let code = hex::decode(codeword).ok()?;

	let mut name = String::new();
	let mut parts = Vec::<Part>::new();
	for word in words {
		if word.starts_with('\'') != word.ends_with('\'') {
			panic!("Invalid name in spec: {line}");
		}
		if word.starts_with('\'') {
			name.push_str(&word[1..word.len() - 1])
		} else {
			parts.push(word.parse().ok()?);
		};
	}
	Some((code, name, parts))
}

pub fn parse_spec(text: &str) -> Spec {
	let mut names = TrieBuilder::new();
	let mut ops = TrieBuilder::new();
	for line0 in text.lines() {
		let line = line0.split('#').next().unwrap().trim();
		if line.is_empty() {
			continue;
		}

		let (code, name, op) = parse_line(line).unwrap_or_else(|| {
			panic!("Failed to parse spec: {line0}");
		});

		if !name.is_empty() {
			names.insert(code.clone(), name);
		}
		ops.insert(code, op);
	}
	Spec {
		names: names.build(),
		ops: ops.build(),
	}
}

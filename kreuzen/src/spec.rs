use std::{collections::{btree_map::Entry, BTreeMap}, sync::LazyLock};

use arrayvec::ArrayVec;

#[derive(Debug, Clone, Copy, PartialEq, Eq, derive_more::FromStr)]
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
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OpSpec {
	pub code: ArrayVec<u8, 2>,
	pub name: Option<String>,
	pub parts: Vec<Part>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Spec {
	Single(OpSpec),
	Multi(BTreeMap<u8, OpSpec>),
}

pub static SPEC: LazyLock<BTreeMap<u8, Spec>> = LazyLock::new(|| parse_spec(include_str!("../../ed85.txt")));

fn parse_opspec(line: &str) -> Option<OpSpec> {
	let mut words = line.split_whitespace();

	let codeword = words.next().unwrap();
	let mut code = ArrayVec::<u8, 2>::new();
	if codeword.len() == 2 {
		code.push(u8::from_str_radix(codeword, 16).ok()?);
	} else if codeword.len() == 4 {
		code.push(u8::from_str_radix(&codeword[0..2], 16).ok()?);
		code.push(u8::from_str_radix(&codeword[2..4], 16).ok()?);
	} else {
		return None;
	}

	let word1 = words.clone().next().unwrap_or("");

	if word1.starts_with('\'') != word1.ends_with('\'') {
		panic!("Invalid name in spec: {line}");
	}
	let name = if word1.starts_with('\'') {
		let name = word1[1..word1.len() - 1].to_string();
		words.next();
		Some(name)
	} else {
		None
	};

	let mut parts = Vec::<Part>::new();
	for word in words {
		parts.push(word.parse().ok()?);
	}
	Some(OpSpec { code, name, parts })
}

pub fn parse_spec(text: &str) -> BTreeMap<u8, Spec> {
	let mut spec = BTreeMap::new();
	for line0 in text.lines() {
		let line = line0.split('#').next().unwrap().trim();
		if line.is_empty() {
			continue;
		}

		let opspec = parse_opspec(line).unwrap_or_else(|| {
			panic!("Failed to parse opspec: {line0}");
		});

		println!("Parsed spec: {opspec:X?}");

		let c = &opspec.code;
		match c.as_slice() {
			[a] => {
				if let Entry::Vacant(e) = spec.entry(*a) {
					e.insert(Spec::Single(opspec));
				} else {
					panic!("Conflicting spec for code {c:02X?}");
				}
			}
			[a, b] => {
				let e = spec.entry(*a).or_insert_with(|| {
					Spec::Multi(BTreeMap::new())
				});
				let Spec::Multi(map) = e else {
					panic!("Conflicting spec for code {c:02X?}");
				};
				if let Entry::Vacant(e) = map.entry(*b) {
					e.insert(opspec);
				} else {
					panic!("Conflicting spec for code {c:02X?}");
				}
			}
			_ => unreachable!(),
		}
	}
	dbg!(&spec);
	spec
}

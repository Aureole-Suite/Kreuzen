mod opcode;
use std::collections::{BTreeMap, BTreeSet};
use std::fmt::Write;
use std::sync::LazyLock;

use crate::Game;
pub use opcode::Opcode;

macro_rules! spec {
	($($name:ident),* $(,)?) => {
		#[cfg(test)]
		mod parse_test {
			use super::*;
			$(#[test] fn $name() {
				LazyLock::force(&lines::$name);
			})*
		}

		#[allow(non_upper_case_globals)]
		#[cfg(not(feature = "live"))]
		mod text {
			$(pub static $name: &str =
				include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/../spec/", stringify!($name), ".txt"));)*
		}

		#[allow(non_upper_case_globals)]
		#[cfg(feature = "live")]
		mod text {
			use super::*;
			$(pub static $name: LazyLock<String> = LazyLock::new(|| {
				std::fs::read_to_string(concat!(env!("CARGO_MANIFEST_DIR"), "/../spec/", stringify!($name), ".txt"))
					.unwrap()
			});)*
		}

		#[allow(non_upper_case_globals)]
		mod lines {
			use super::*;
			$(pub static $name: LazyLock<Lines> = LazyLock::new(|| parse_lines(&text::$name));)*
		}

		fn lines_for(name: &str) -> &'static Lines {
			match name {
				$(stringify!($name) => &lines::$name,)*
				_ => panic!("Unknown spec: {name}"),
			}
		}

		#[allow(non_upper_case_globals)]
		mod specs {
			use super::*;
			$(pub static $name: LazyLock<Spec> = LazyLock::new(|| parse_spec(&lines::$name));)*
		}
	};
}

spec! {
	cs1, cs1_menu,
	cs2, cs2_1, cs2_menu,
	cs3, cs3_1, cs3_2, cs3_3,
	cs4, cs4_1,
	reverie, reverie_1,
	tx,
}

pub fn for_game(game: Game, variant: u8) -> &'static Spec {
	match game {
		Game::Cs1 if variant == 100 => &specs::cs1_menu,
		Game::Cs1 => &specs::cs1,
		Game::Cs2 if variant == 0 => &specs::cs2,
		Game::Cs2 if variant == 1 => &specs::cs2_1,
		Game::Cs2 if variant == 100 => &specs::cs2_menu,
		Game::Cs3 if variant == 0 => &specs::cs3,
		Game::Cs3 if variant == 1 => &specs::cs3_1,
		Game::Cs3 if variant == 2 => &specs::cs3_2,
		Game::Cs3 if variant == 3 => &specs::cs3_3,
		Game::Cs4 if variant == 0 => &specs::cs4,
		Game::Cs4 if variant == 1 => &specs::cs4_1,
		Game::Reverie if variant == 0 => &specs::reverie,
		Game::Reverie if variant == 1 => &specs::reverie_1,
		Game::Tx => &specs::tx,
		_ => panic!("Unsupported game or variant: {game:?}/{variant}"),
	}
}

#[allow(non_camel_case_types)]
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

	Char,
	Item,
	Magic,
	Flag,
	Global,
	Var,
	FuncArg,
	NumReg,
	StrReg,
	Attr,
	CharAttr,
	Flags8,
	Flags16,
	Flags32,

	Expr,
	Text,
	Dyn,
	Ndyn,
	Dync,

	Cs1_CharCreate,
	Cs1_CharAniclipPlay,
	Cs1_2834,
	Cs1_36,
	Cs1_3C,

	Cs2_37,

	Tx_3C,
	Tx_isforceload,
	Tx_2F,

	Cs3_98,
	Cs3_c0,

	Cs4_40,
	Cs4_wtf_are_you_doing,

	Rev_3E,
	Rev_79,
	Rev_D2,
	Rev_E002,

	Print,
	Fail,
}

#[derive(Debug)]
pub struct Spec {
	pub ops: [Option<Op>; 256],
	pub by_name: BTreeMap<String, Opcode>,
}

#[derive(Debug, Clone, Default)]
pub struct Op {
	pub parts: Vec<Part>,
	pub name: String,
	child_keys: Vec<u8>,
	children: Vec<Op>,
}

impl Op {
	pub fn has_children(&self) -> bool {
		!self.child_keys.is_empty()
	}

	pub fn child(&self, key: u8) -> Option<&Op> {
		assert_eq!(self.child_keys.len(), self.children.len());
		if self.child_keys.is_empty() {
			return None;
		}
		let index = self.child_keys.binary_search(&key).ok()?;
		self.children.get(index)
	}
}

type Lines = BTreeMap<Opcode, (String, Vec<Part>)>;

fn parse_lines(text: &str) -> Lines {
	let mut ops = BTreeMap::new();
	let mut add = |code: Opcode, name: String, parts: Vec<Part>| {
		assert!(
			!ops.contains_key(&code),
			"Duplicate code in spec: {code} and {name}"
		);
		ops.insert(code, (name, parts));
	};
	for line in text.lines() {
		parse_line(line, &mut add);
	}
	ops
}

fn parse_line(line0: &str, add: &mut impl FnMut(Opcode, String, Vec<Part>)) {
	let line = line0.split('#').next().unwrap().trim();
	let mut words = line.split_whitespace();
	let Some(first) = words.next() else {
		return;
	};
	if first == "import" {
		let from = words.next().unwrap();
		let range = words.next().unwrap();
		assert!(words.next().is_none());
		let (a, b) = range.split_once("..").unwrap();
		let a = a.parse::<Opcode>().unwrap();
		let b = b.parse::<Opcode>().unwrap();
		let include = lines_for(from);
		for (code, (name, parts)) in include.range(a..b) {
			add(*code, name.clone(), parts.clone());
		}
	} else {
		let code = first.parse().unwrap();
		let mut name = String::new();
		let mut parts = Vec::<Part>::new();
		for word in words {
			if word.starts_with('\'') != word.ends_with('\'') || word == "''" {
				panic!("Invalid name in spec: {line}");
			}
			if word.starts_with('\'') {
				name.push_str(&word[1..word.len() - 1])
			} else {
				parts.push(word.parse().unwrap());
			};
		}
		add(code, name, parts);
	}
}

fn parse_spec(ops: &Lines) -> Spec {
	Spec {
		ops: build_ops(ops),
		by_name: build_names(ops),
	}
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

fn build_names(
	inp: &BTreeMap<Opcode, (String, Vec<Part>)>,
) -> BTreeMap<String, Opcode> {
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
			if let Some((s, _)) = inp.get(&p) && !s.is_empty() {
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

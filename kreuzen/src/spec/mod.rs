mod opcode;
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::fmt::Write;
use std::sync::LazyLock;

use crate::Game;
pub use opcode::Opcode;

macro_rules! spec {
	($name:ident, $test:ident, $file:expr) => {
		#[test]
		fn $test() {
			LazyLock::force(&$name);
		}

		static $name: LazyLock<Spec> = LazyLock::new(|| {
			#[cfg(not(feature = "live"))]
			let source = include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/../spec/", $file));
			#[cfg(feature = "live")]
			let source =
				std::fs::read_to_string(concat!(env!("CARGO_MANIFEST_DIR"), "/../spec/", $file))
					.unwrap();
			Spec::parse(&source)
		});
	};
}

spec!(CS1, test_ed81, "cs1.txt");
spec!(CS1_1, test_ed81_1, "cs1_1.txt");
spec!(CS1_2, test_ed81_2, "cs1_2.txt");
spec!(CS1_3, test_ed81_3, "cs1_3.txt");
spec!(CS1_MENU, test_ed81_menu, "cs1_menu.txt");
spec!(CS2, test_ed82, "cs2.txt");
spec!(CS2_1, test_ed82_1, "cs2_1.txt");
spec!(CS2_MENU, test_ed82_menu, "cs2_menu.txt");
spec!(CS3, test_ed83, "cs3.txt");
spec!(CS3_1, test_ed83_1, "cs3_1.txt");
spec!(CS3_2, test_ed83_2, "cs3_2.txt");
spec!(CS3_3, test_ed83_3, "cs3_3.txt");
spec!(CS4, test_ed84, "cs4.txt");
spec!(CS4_1, test_ed84_1, "cs4_1.txt");
spec!(REVERIE, test_reverie, "reverie.txt");
spec!(REVERIE_1, test_reverie_1, "reverie_1.txt");
spec!(TX, test_tx, "tx.txt");

pub fn for_game(game: Game, variant: u8) -> &'static Spec {
	match game {
		Game::Cs1 if variant == 0 => &CS1,
		Game::Cs1 if variant == 1 => &CS1_1,
		Game::Cs1 if variant == 2 => &CS1_2,
		Game::Cs1 if variant == 3 => &CS1_3,
		Game::Cs1 if variant == 100 => &CS1_MENU,
		Game::Cs2 if variant == 0 => &CS2,
		Game::Cs2 if variant == 1 => &CS2_1,
		Game::Cs2 if variant == 100 => &CS2_MENU,
		Game::Cs3 if variant == 0 => &CS3,
		Game::Cs3 if variant == 1 => &CS3_1,
		Game::Cs3 if variant == 2 => &CS3_2,
		Game::Cs3 if variant == 3 => &CS3_3,
		Game::Cs4 if variant == 0 => &CS4,
		Game::Cs4 if variant == 1 => &CS4_1,
		Game::Reverie if variant == 0 => &REVERIE,
		Game::Reverie if variant == 1 => &REVERIE_1,
		Game::Tx => &TX,
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
	pub names: HashMap<Opcode, String>,
	pub by_name: BTreeMap<String, Opcode>,
}

impl Spec {
	pub fn parse(text: &str) -> Self {
		parse_spec(text)
	}
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

struct Line {
	code: Opcode,
	name: String,
	parts: Vec<Part>,
}

fn parse_line(line: &str) -> Option<Line> {
	let mut words = line.split_whitespace();

	let code = words.next().unwrap().parse().unwrap();

	let mut name = String::new();
	let mut parts = Vec::<Part>::new();
	for word in words {
		if word.starts_with('\'') != word.ends_with('\'') || word == "''" {
			panic!("Invalid name in spec: {line}");
		}
		if word.starts_with('\'') {
			name.push_str(&word[1..word.len() - 1])
		} else {
			parts.push(word.parse().ok()?);
		};
	}
	Some(Line { code, name, parts })
}

fn parse_spec(text: &str) -> Spec {
	let mut names = BTreeMap::new();
	let mut ops = BTreeMap::new();
	for line0 in text.lines() {
		let line = line0.split('#').next().unwrap().trim();
		if line.is_empty() {
			continue;
		}

		let line = parse_line(line).unwrap_or_else(|| {
			panic!("Failed to parse spec: {line0}");
		});
		assert!(
			!ops.contains_key(&line.code),
			"Duplicate code in spec: {}",
			&line.code
		);
		names.insert(line.code, line.name);
		ops.insert(line.code, line.parts);
	}

	let (names, by_name) = build_names(&names);

	Spec {
		ops: build_ops(ops),
		by_name,
		names,
	}
}

fn build_ops(ops: BTreeMap<Opcode, Vec<Part>>) -> [Option<Op>; 256] {
	let mut out = std::array::from_fn(|_| None);
	for (k, v) in ops {
		assert!(!k.is_empty(), "Empty code in spec");
		let mut op = out[k[0] as usize].get_or_insert_with(Op::default);
		for byte in k.iter().skip(1) {
			if op.child_keys.last().is_none_or(|last| last < byte) {
				op.child_keys.push(*byte);
				op.children.push(Op::default());
			}
			op = op.children.last_mut().unwrap();
		}
		op.parts = v;
	}
	out
}

fn build_names(
	inp: &BTreeMap<Opcode, String>,
) -> (HashMap<Opcode, String>, BTreeMap<String, Opcode>) {
	let mut all = BTreeSet::new();
	let mut leaves = BTreeSet::new();
	for op in inp.keys() {
		for p in op.prefixes() {
			leaves.remove(&p);
			all.insert(p);
		}
		leaves.insert(*op);
	}

	let mut names = HashMap::new();
	let mut by_name = BTreeMap::new();
	let mut put = |op: Opcode, mut name: String| {
		if leaves.contains(&op) {
			if let Some(prev) = by_name.insert(name.clone(), op) {
				panic!("Duplicate name in spec: {prev} and {op} are both named {name}");
			}
		} else {
			name.push('_');
		}
		names.insert(op, name);
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
			if let Some(s) = inp.get(&p) && !s.is_empty() {
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

	(names, by_name)
}

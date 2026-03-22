mod opcode;
use std::collections::{BTreeMap, BTreeSet};
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
			#[cfg(not(feature="live"))]
			let source = include_str!($file);
			#[cfg(feature="live")]
			let source = std::fs::read_to_string(concat!(env!("CARGO_MANIFEST_DIR"), "/src/spec/", $file)).unwrap();
			Spec::parse(&source)
		});
	};
}

spec!(CS1, test_ed81, "cs1.txt");
spec!(CS2, test_ed82, "cs2.txt");
spec!(CS3, test_ed83, "cs3.txt");
spec!(CS4, test_ed84, "cs4.txt");
spec!(REVERIE, test_ed85, "reverie.txt");
spec!(TX, test_tx, "tx.txt");

pub fn for_game(game: Game) -> &'static Spec {
	match game {
		Game::Cs1 => &CS1,
		Game::Cs2 => &CS2,
		Game::Cs3 => &CS3,
		Game::Cs4 => &CS4,
		Game::Reverie => &REVERIE,
		Game::Tx => &TX,
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

	F32Opt,

	Cs1_22,
	Cs1_36,
	Cs1_3C,
	Cs1_2834,

	Cs2_37,
	Cs2_7C,

	Tx_3C,

	Cs3_98,
	Cs3_c0,

	Cs4_odd,
	Cs4_3348,
	Cs4_333C,
	Cs4_3339,
	Cs4_333D,
	Cs4_sound_play,
	Cs4_40_a,
	Cs4_40_b,
	Cs4_5E00,

	_3E,
	_3F,
	_40,
	_4E,
	_6C,
	_79,
	_98,
	_AB00,
	_AB02,
	_C0,
	_D2,

	Broken20,
	BrokenEffLoad,
	Broken40,
	Broken62,

	Print,
	Fail,
}

#[derive(Debug)]
pub struct Spec {
	pub ops: [Option<Op>; 256],
	pub names: BTreeMap<Opcode, String>,
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
) -> (BTreeMap<Opcode, String>, BTreeMap<String, Opcode>) {
	let mut leaves = BTreeSet::new();
	for op in inp.keys() {
		for p in op.prefixes() {
			leaves.remove(&p);
		}
		leaves.insert(*op);
	}
	let mut names = BTreeMap::new();
	let mut by_name = BTreeMap::new();
	for (&op, opname) in inp {
		if !leaves.contains(&op) {
			continue;
		}

		let mut name;
		macro_rules! put {
			($name:expr) => {
				name = $name;
				if let Some(prev) = by_name.insert(name.clone(), op) {
					panic!("Duplicate name in spec: {prev} and {op} are both named {name}");
				}
			};
		}

		let mut s = String::from("op");
		for b in op {
			write!(s, "{b:02X}").unwrap();
		}
		put!(s);

		for p in op.prefixes() {
			if let Some(s) = inp.get(&p)
				&& !s.is_empty()
			{
				let mut s = s.to_owned();
				s.push('_');
				for b in &op[p.len()..] {
					write!(s, "{b:02X}").unwrap();
				}
				put!(s);
			}
		}
		if !opname.is_empty() {
			put!(opname.clone());
		}

		names.insert(op, name);
	}
	(names, by_name)
}

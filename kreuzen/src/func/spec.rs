use std::fmt::Write;
use std::collections::{BTreeMap, BTreeSet};

mod opcode;
pub use opcode::Opcode;

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
		assert!(!ops.contains_key(&line.code), "Duplicate code in spec: {}", &line.code);
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

fn build_names(inp: &BTreeMap<Opcode, String>) -> (BTreeMap<Opcode, String>, BTreeMap<String, Opcode>) {
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
			if let Some(s) = inp.get(&p) && !s.is_empty() {
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

pub(crate) fn op_40(a: crate::types::Char) -> &'static [Part] {
	use Part::*;
	match a.0 {
		0xFE02..= 0xFE04 => &[F32, F32, F32, F32, F32, U8, Flags16, F32, F32, U8],
		0xFE05           => &[F32, F32, F32, F32,      U8, Flags16, F32, F32, U8, Str],
		0xFE15           => &[Dyn, Dyn, Dyn, Dyn,      U8, Flags16, F32, F32, U8],
		_                => &[F32, F32, F32, F32,      U8, Flags16, F32, F32, U8],
	}
}

pub(crate) fn op_98(a: u16) -> &'static [Part] {
	use Part::*;
	match a {
		1 => &[F32],
		2 => &[F32],
		6 => &[F32],
		7 => &[F32],
		3 => &[U16, U8],
		1000 => &[F32, U8], // a0100:TK_Enter2 says this is rotation
		1001 => &[F32, U8],
		2000 => &[U8, F32, U8],
		3000 => &[F32, F32, U16, F32],
		4000 => &[Char, F32, U16, U8],
		4001 => &[Str, F32, U16, U8],
		4002 => &[U16],
		5000 => &[F32],
		5001 => &[F32],
		5002 => &[F32],
		6000 => &[F32],
		6001 => &[F32],
		6500 => &[F32],
		7000 => &[U8],
		7001 => &[U8],
		8000 => &[Str, U8],
		9000 => &[F32],
		10000 => &[F32, F32, F32, F32, F32, F32, F32, F32],
		_ => &[],
	}
}

pub(crate) fn op_c0(a: u16) -> &'static [Part] {
	use Part::*;
	match a {
		1 => &[F32],
		3 => &[Str, F32, F32, F32, F32, F32, F32],
		4 => &[Str, U8],
		1000 | 1001 | 1003 => &[U16, U16],
		_ => &[],
	}
}

pub(crate) fn op_d2(a: i16) -> &'static [Part] {
	use Part::*;
	match a {
		0 => &[U8],
		3 => &[U8, U8, U32],
		-2 | -1 => &[Dyn],
		_ => &[],
	}
}

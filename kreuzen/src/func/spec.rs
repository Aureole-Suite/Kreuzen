use std::collections::BTreeMap;

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

	Label,
	Switch,

	_3E,
	_3F,
	_40,
	_4E,
	_6C,
	_79,
	_98,
	_AB01,
	_AB02,
	_C0,
	_D2,
}

#[derive(Debug)]
pub struct Spec {
	pub names: BTreeMap<Vec<u8>, String>,
	pub ops: [Option<Op>; 256],
}

impl Spec {
	pub fn parse(text: &str) -> Self {
		parse_spec(text)
	}
}

#[derive(Debug, Clone, Default)]
pub struct Op {
	pub parts: Vec<Part>,
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
	code: Vec<u8>,
	name: String,
	parts: Vec<Part>,
}

fn parse_line(line: &str) -> Option<Line> {
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
		assert!(!ops.contains_key(&line.code), "Duplicate code in spec: {}", hex::encode(&line.code));
		if !line.name.is_empty() {
			names.insert(line.code.clone(), line.name);
		}
		ops.insert(line.code, line.parts);
	}

	Spec {
		names,
		ops: build_ops(ops),
	}
}

fn build_ops(ops: BTreeMap<Vec<u8>, Vec<Part>>) -> [Option<Op>; 256] {
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

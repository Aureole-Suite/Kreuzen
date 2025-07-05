#![feature(if_let_guard)]
use std::{collections::HashMap, ops::Deref, sync::{LazyLock, Mutex}};

use arrayvec::ArrayVec;
use gospel::read::{Reader, Le as _};
use snafu::{OptionExt as _, ResultExt as _};

#[extend::ext]
impl<'a> Reader<'a> {
	fn str(&mut self) -> Result<String, gospel::read::Error> {
		let pos = self.pos();
		let cstr = self.cstr()?;
		cstr.to_str().map_err(|e| gospel::read::Error::Other {
			pos,
			source: Box::new(e),
		}).map(String::from)
	}

	fn ccstr(&mut self) -> Result<&'a [u8], gospel::read::Error> {
		let pos = self.remaining().windows(2).position(|w| w == [0, 0]);
		if let Some(pos) = pos {
			let s = self.slice(pos)?;
			self.check_u16(0).expect("already checked");
			Ok(s)
		} else {
			Err(gospel::read::Error::Other {
				pos: self.pos(),
				source: "not found".into(),
			})
		}
	}
}

#[derive(Debug, snafu::Snafu)]
pub enum ReadError {
	#[snafu(display("invalid read (at {location})"), context(false))]
	Read {
		source: gospel::read::Error,
		#[snafu(implicit)]
		location: snafu::Location,
	},
	#[snafu(display("could not read function '{name}'"))]
	Function {
		name: String,
		source: FunctionError,
	},
}

#[derive(Clone)]
struct Entry {
	name: String,
	start: usize,
}

impl std::fmt::Debug for Entry {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}@{}", self.name, self.start)
	}
}

enum Type {
	Normal,
	Table, // TODO separate each table
	Btlset,
	BookData,
	FcAuto,
	Underscore,
	Empty,
	Lambda,
	A0,
}

impl Type {
	fn from_name(name: &str) -> Self {
		if name.starts_with("_Lambda") {
			Type::Lambda
		} else if name.starts_with("_a0_") {
			Type::A0
		} else if name.starts_with("_") {
			Type::Underscore
		} else if name.is_empty() {
			Type::Empty
		} else if name.starts_with("BTLSET") {
			Type::Btlset
		} else if name.starts_with("BookData") {
			Type::BookData
		} else if name.starts_with("FC_auto") {
			Type::FcAuto
		} else if name.ends_with("Table") || name.ends_with("Data") {
			Type::Table
		} else if name.starts_with("AniBtlKisin") {
			Type::Table // todo
		} else {
			Type::Normal
		}
	}
}

#[derive(Debug, snafu::Snafu)]
pub enum FunctionError {
	#[snafu(display("invalid read (at {location})"), context(false))]
	Read {
		source: gospel::read::Error,
		#[snafu(implicit)]
		location: snafu::Location,
	},
	#[snafu(display("failed to read op at {pos:X}"))]
	Op {
		pos: usize,
		source: OpError,
	}
}

// This function corresponds to the /asm/ files. Cursed.
fn read_table(f: &mut Reader, n: usize) -> Result<Vec<Entry>, ReadError> {
	let mut starts = Vec::with_capacity(n);
	for _ in 0..n {
		starts.push(f.u32()? as usize);
	}
	let mut lengths = Vec::with_capacity(n);
	for _ in 0..n {
		lengths.push(f.u16()? as usize);
	}
	let mut names = Vec::with_capacity(n);
	#[expect(clippy::needless_range_loop)]
	for i in 0..n {
		assert_eq!(f.pos(), lengths[i]);
		names.push(f.str()?);
	}
	let mut entries = Vec::with_capacity(n);
	for i in 0..n {
		entries.push(Entry {
			name: names[i].to_owned(),
			start: starts[i],
		});
	}
	Ok(entries)
}

pub fn parse(data: &[u8]) -> Result<(), ReadError> {
	let mut f = Reader::new(data);
	f.check_u32(0x20)?;
	f.check_u32(0x20)?;
	let table_top = f.u32()? as usize;
	let table_size = f.u32()? as usize;
	let function_name_table_top = f.u32()? as usize;
	let nfunc = f.u32()? as usize;
	let asm_end = f.u32()? as usize;
	assert_eq!(table_top + table_size, function_name_table_top);
	assert_eq!(table_size, nfunc * 4);

	f.check_u32(0xABCDEF00)?;
	let name = f.str()?;

	// In most cases, there's an align 4 followed by `00 00 00 FF`. But not always.
	let mut aligned = 0;
	while f.pos() < table_top {
		f.align_zeroed(4)?;
		f.check_u32(0xFF000000)?;
		aligned += 1;
	};

	assert_eq!(f.pos(), table_top);
	let table = read_table(&mut f, nfunc)?;
	assert_eq!(f.pos(), asm_end);
	f.align_zeroed(4)?;

	println!();
	let ends = table.iter().map(|e| e.start).skip(1).chain([f.len()]);
	for (entry, end) in table.iter().zip(ends) {
		let _span = tracing::trace_span!("chunk", name = entry.name.as_str(), start = entry.start, end).entered();
		match Type::from_name(&entry.name) {
			Type::Normal | Type::Lambda => {
				println!("disassembling function {}, {:04X}", entry.name, entry.start);
				read_func(f.at(entry.start)?, end).context(FunctionSnafu { name: &entry.name })?;
			}
			_ => {}
		}
	}

	Ok(())
}

pub static COUNTS: LazyLock<Mutex<HashMap<String, usize>>> = LazyLock::new(Default::default);

fn read_func(mut f: Reader, end: usize) -> Result<(), FunctionError> {
	while !at_end(&mut f, end) {
		let pos = f.pos();
		match read_op(&mut f).context(OpSnafu { pos }) {
			Ok(op) => {
				// println!("{:05X} {op:?}", pos);
			}
			Err(e) => {
				for e in std::iter::successors(Some(&e as &dyn std::error::Error), |e| e.source()) {
					println!("{e}");
					if let Some(OpError::UnknownOp { code }) = e.downcast_ref().or_else(|| e.downcast_ref().map(Box::deref)) {
						let k = hex::encode_upper(code);
						*COUNTS.lock().unwrap().entry(k).or_default() += 1;
					}
					if let Some(ExprError::UnknownExpr { code, .. }) = e.downcast_ref().or_else(|| e.downcast_ref().map(Box::deref)) {
						let k = format!("expr {code:02X}");
						*COUNTS.lock().unwrap().entry(k).or_default() += 1;
					}
				}
				print!("{:#1X}", f.at(pos).unwrap().dump().num_width_as(0xFFFF).end(end));
				break;
			}
		}
	}
	Ok(())
}

#[derive(Clone, PartialEq)]
pub struct Op {
	pub op: ArrayVec<u8, 2>,
	pub line: u16,
	pub unk: u8,
	pub args: Vec<Arg>
}

#[derive(Clone, PartialEq, derive_more::From)]
pub enum Arg {
	Str(String),
	U8(u8),
	U16(u16),
	U32(u32),
	I8(i8),
	I16(i16),
	I32(i32),
	F32(f32),
	Expr(Expr),
	#[from(skip)]
	Label(u32),
	CallArg(CallArg),
	Dialogue(Dialogue),
}

impl Op {
	pub fn name(arg: &[u8]) -> Option<&'static str> {
		Some(match arg {
			[0x01] => "return",
			[0x02] => "call",
			[0x03] => "goto",
			[0x05] => "if",
			[0x06] => "switch",
			[0x16] => "sleep",
			_ => return None,
		})
	}
	
	pub fn push(&mut self, arg: impl Into<Arg>) {
		self.args.push(arg.into());
	}

	fn sub(&mut self, u8: u8) -> u8 {
		self.op.push(u8);
		u8
	}
}

impl std::fmt::Debug for Op {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		if self.line != 0 {
			write!(f, "{}@", self.line)?;
		}
		if let Some(name) = Self::name(&self.op) {
			write!(f, "{name}")?;
		} else {
			write!(f, "op")?;
			for byte in &self.op {
				write!(f, "{byte:02X}")?;
			}
		}
		if self.unk != 0xFF {
			write!(f, ":{}", self.unk)?;
		}
		write!(f, "(")?;
		for (i, arg) in self.args.iter().enumerate() {
			if i > 0 {
				write!(f, ", ")?;
			}
			arg.fmt(f)?;
		}
		write!(f, ")")
	}
}

impl std::fmt::Debug for Arg {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Arg::Str(s) => write!(f, "{s:?}"),
			Arg::U8(v) => write!(f, "{v}"),
			Arg::U16(v) => write!(f, "{v}"),
			Arg::U32(v) => write!(f, "{v}"),
			Arg::I8(v) => write!(f, "{v}"),
			Arg::I16(v) => write!(f, "{v}"),
			Arg::I32(v) => write!(f, "{v}"),
			Arg::F32(v) => write!(f, "{v:?}"),
			Arg::Expr(e) => write!(f, "{e:?}"),
			Arg::Label(l) => write!(f, "=> {l:08X}"),
			Arg::CallArg(ca) => write!(f, "{ca:?}"),
			Arg::Dialogue(d) => write!(f, "{d:?}"),
		}
	}
}

#[derive(Debug, snafu::Snafu)]
pub enum OpError {
	#[snafu(display("invalid read (at {location})"), context(false))]
	Read {
		source: gospel::read::Error,
		#[snafu(implicit)]
		location: snafu::Location,
	},
	#[snafu(display("unknown op {}", hex::encode_upper(code)))]
	UnknownOp { code: ArrayVec<u8, 2> },
	#[snafu(display("failed to read expr"))]
	Expr {
		source: ExprError,
	},
	#[snafu(display("failed to read dialogue"))]
	Dialogue {
		source: DialogueError,
	},
}

mod spec;

fn read_op(f: &mut Reader) -> Result<Op, OpError> {
	let pos = f.pos();
	let op = read_op2(f)?;
	if op.unk != 0xFF && pos + op.unk as usize != f.pos() && *op.op != [0x0A] && *op.op != [0x18] {
		println!("{} + {} != {} for {op:?}", pos, op.unk, f.pos());
		println!("{:#1X}", f.dump().start(pos).len(op.unk as usize));
	}
	Ok(op)
}

fn read_op2(f: &mut Reader) -> Result<Op, OpError> {
	let mut code = f.u8()?;
	let mut op = Op {
		op: ArrayVec::new(),
		line: 0,
		unk: 0xFF,
		args: Vec::new(),
	};
	op.op.push(code);
	if code == 0x00 || code == 0x01 {
		return Ok(op);
	}

	op.line = f.u16()?;
	f.check_u8(0)?;
	op.unk = f.u8()?;

	match spec::SPEC.exact_match(&op.op).map(|v| v.name.as_ref()) {
		Some("goto") => {
			op.push(Arg::Label(f.u32()?));
			return Ok(op);
		}
		Some("if") => {
			op.push(expr(f).context(ExprSnafu)?);
			op.push(Arg::Label(f.u32()?));
			return Ok(op);
		}
		Some("switch") => {
			op.push(expr(f).context(ExprSnafu)?);
			let n = f.u8()?;
			for _ in 0..n {
				op.push(f.i32()?);
				op.push(Arg::Label(f.u32()?));
			}
			op.push(Arg::Label(f.u32()?));
			return Ok(op);
		}
		_ => {}
	}

	let mut inc = spec::SPEC.inc_search();
	loop {
		let Some(ans) = inc.query(&code) else {
			return UnknownOpSnafu { code: op.op }.fail();
		};
		if let Some(spec) = inc.value() {
			read_parts(&mut op, f, &spec.parts)?;
		}
		if ans.is_prefix() {
			code = f.u8()?;
			op.op.push(code);
		} else {
			break;
		}
	}

	Ok(op)
}

fn read_parts(op: &mut Op, f: &mut Reader, part: &[spec::Part]) -> Result<(), OpError> {
	for p in part {
		read_part(op, f, p)?;
	}
	Ok(())
}

fn read_part(op: &mut Op, f: &mut Reader, part: &spec::Part) -> Result<(), OpError> {
	use spec::Part::*;
	match part {
		U8 => op.push(f.u8()?),
		U16 => op.push(f.u16()?),
		U32 => op.push(f.u32()?),
		I8 => op.push(f.i8()?),
		I16 => op.push(f.i16()?),
		I32 => op.push(f.i32()?),
		F32 => op.push(f.f32()?),
		Str => op.push(f.str()?),
		Expr => op.push(expr(f).context(ExprSnafu)?),
		Text => op.push(dialogue(f).context(DialogueSnafu)?),
		Dyn => op.push(call_arg(f)?),
		Dyn2 => op.push(call_arg2(f)?),
		Ndyn => {
			for _ in 0..f.u8()? {
				op.push(call_arg(f)?);
			}
		}

		_3E => {
			let Some(&Arg::U16(a)) = op.args.get(1) else {
				panic!("3E must have a U16 arg");
			};
			if a == 0xFE12 {
				read_parts(op, f, &[U8])?;
			} else if a == 0xFE13 {
				read_parts(op, f, &[F32])?;
			} else if a == 0xFFFF { // Not in SSD
				read_parts(op, f, &[U8, U8, U8])?;
			}
		}

		_40 => {
			let Some(&Arg::U16(a)) = op.args.get(1) else {
				panic!("40 must have a U16 arg");
			};
			if a == 0xFE15 {
				read_parts(op, f, &[Dyn, Dyn, Dyn, Dyn])?;
			} else {
				read_parts(op, f, &[F32, F32, F32, F32])?;
				if matches!(a, 0xFE02 | 0xFE03 | 0xFE04) {
					read_parts(op, f, &[F32])?;
				}
			}
			read_parts(op, f, &[U8, U16, F32, F32, U8])?;
			if a == 0xFE05 {
				read_parts(op, f, &[Str])?;
			}
		}
	}
	Ok(())
}

fn at_end(f: &mut Reader<'_>, end: usize) -> bool {
	let rest = &f.data()[f.pos()..end];
	rest.len() <= 3 && rest.iter().all(|&b| b == 0)
}

#[derive(Debug, Clone, PartialEq)]
pub enum CallArg {
	_11(u32, u8),
	_22(f32, f32),
	_33(f32, u8),
	_44(f32, u8, String),
	_55(u32, u8),
	_DD(String, String),
	_FF(u32, u8),
	_EE(f32, u8),
	Unknown(u8),
}

fn call_arg(f: &mut Reader) -> Result<CallArg, OpError> {
	Ok(match f.u8()? {
		0x11 => CallArg::_11(f.u32()?, f.u8()?),
		0x22 => CallArg::_22(f.f32()?, f.f32()?),
		0x33 => CallArg::_33(f.f32()?, f.u8()?),
		0x44 => CallArg::_44(f.f32()?, f.u8()?, String::new()),
		0x55 => CallArg::_55(f.u32()?, f.u8()?),
		0xDD => CallArg::_DD(String::new(), f.str()?),
		0xEE => CallArg::_EE(f.f32()?, f.u8()?),
		0xFF => CallArg::_FF(f.u32()?, f.u8()?),
		v => CallArg::Unknown(v),
	})
}

fn call_arg2(f: &mut Reader) -> Result<CallArg, OpError> {
	Ok(match f.u8()? {
		0x11 => CallArg::_11(f.u32()?, f.u8()?),
		0x22 => CallArg::_22(f.f32()?, f.f32()?),
		0x33 => CallArg::_33(f.f32()?, f.u8()?),
		0x44 => CallArg::_44(f.f32()?, f.u8()?, f.str()?),
		0x55 => CallArg::_55(f.u32()?, f.u8()?),
		0xDD => CallArg::_DD(f.str()?, f.str()?),
		0xEE => CallArg::_EE(f.f32()?, f.u8()?),
		0xFF => CallArg::_FF(f.u32()?, f.u8()?),
		v => CallArg::Unknown(v),
	})
}

#[derive(Debug, snafu::Snafu)]
pub enum ExprError {
	#[snafu(display("invalid read (at {location})"), context(false))]
	Read {
		source: gospel::read::Error,
		#[snafu(implicit)]
		location: snafu::Location,
	},
	#[snafu(display("unknown expr {code:02X} {stack:?}"))]
	UnknownExpr { code: u8, stack: Vec<Expr> },
	#[snafu(display("failed to read op at {pos:X}"))]
	ExprOp {
		pos: usize,
		#[snafu(source(from(OpError, Box::new)))]
		source: Box<OpError>,
	},
	#[snafu(display("empty stack"))]
	EmptyStack,
	#[snafu(display("overfull stack"))]
	OverfullStack { stack: Vec<Expr> },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
	Int(i32),
	Op(Op),
	Flag(u16),
	Var(u8),
	Attr(u8),
	CharAttr(u16, u8),
	Rand,
	Global(u8),
	_24(i32),
	_25(u16),
	Bin(BinOp, Box<Expr>, Box<Expr>),
	Un(UnOp, Box<Expr>),
	Ass(AssOp, Box<Expr>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)] // comment to trick rustfmt
#[derive(num_enum::TryFromPrimitive, num_enum::IntoPrimitive)]
#[repr(u8)]
pub enum BinOp {
	Eq = 0x02,      // ==
	Ne = 0x03,      // !=
	Lt = 0x04,      // <
	Gt = 0x05,      // >
	Le = 0x06,      // <=
	Ge = 0x07,      // >=
	BoolAnd = 0x09, // &&
	BitAnd = 0x0A,  // &
	BitOr = 0x0B,   // | and ||
	Add = 0x0C,     // +
	Sub = 0x0D,     // -
	BitXor = 0x0F,  // ^
	Mul = 0x10,     // *
	Div = 0x11,     // /
	Mod = 0x12,     // %
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)] // comment to trick rustfmt
#[derive(num_enum::TryFromPrimitive, num_enum::IntoPrimitive)]
#[repr(u8)]
pub enum UnOp {
	BoolNot = 0x08, // !
	Neg = 0x0E,     // -
	BitNot = 0x1D,  // ~
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)] // comment to trick rustfmt
#[derive(num_enum::TryFromPrimitive, num_enum::IntoPrimitive)]
#[repr(u8)]
pub enum AssOp {
	Ass = 0x13,    // =
	MulAss = 0x14, // *=
	DivAss = 0x15, // /=
	ModAss = 0x16, // %=
	AddAss = 0x17, // +=
	SubAss = 0x18, // -=
	AndAss = 0x19, // &=
	XorAss = 0x1A, // ^=
	OrAss = 0x1B,  // |=
}

fn expr(f: &mut Reader) -> Result<Expr, ExprError> {
	let mut stack = Vec::new();
	loop {
		let pos = f.pos();
		match f.u8()? {
			0x00 => stack.push(Expr::Int(f.i32()?)),
			0x01 => break,
			0x1C => stack.push(Expr::Op(read_op(f).context(ExprOpSnafu { pos })?)),
			0x1E => stack.push(Expr::Flag(f.u16()?)),
			0x1F => stack.push(Expr::Var(f.u8()?)),
			0x20 => stack.push(Expr::Attr(f.u8()?)),
			0x21 => stack.push(Expr::CharAttr(f.u16()?, f.u8()?)),
			0x22 => stack.push(Expr::Rand),
			0x23 => stack.push(Expr::Global(f.u8()?)),
			0x24 => stack.push(Expr::_24(f.i32()?)),
			0x25 => stack.push(Expr::_25(f.u16()?)),

			v if let Ok(v) = BinOp::try_from(v) => {
				let b = stack.pop().context(EmptyStackSnafu)?;
				let a = stack.pop().context(EmptyStackSnafu)?;
				stack.push(Expr::Bin(v, Box::new(a), Box::new(b)));
			}
			v if let Ok(v) = UnOp::try_from(v) => {
				let a = stack.pop().context(EmptyStackSnafu)?;
				stack.push(Expr::Un(v, Box::new(a)));
			}
			v if let Ok(v) = AssOp::try_from(v) => {
				let a = stack.pop().context(EmptyStackSnafu)?;
				stack.push(Expr::Ass(v, Box::new(a)));
			}

			code => return UnknownExprSnafu { code, stack }.fail(),
		}
	}
	if stack.len() == 1 {
		Ok(stack.pop().unwrap())
	} else {
		OverfullStackSnafu { stack }.fail()
	}
}

#[derive(Debug, snafu::Snafu)]
pub enum DialogueError {
	#[snafu(display("invalid read (at {location})"), context(false))]
	Read {
		source: gospel::read::Error,
		#[snafu(implicit)]
		location: snafu::Location,
	},
	#[snafu(display("invalid string: {text:?}"))]
	String {
		text: String,
	},
	#[snafu(display("unknown dialogue control byte {byte:02X}"))]
	BadControl {
		byte: u8,
	},
}

#[derive(Debug, Clone, PartialEq)]
pub struct Dialogue(pub Vec<DialoguePart>);

#[derive(Clone, PartialEq)]
pub enum DialoguePart {
	String(String),
	Line,
	Page,
	_03,
	_09,
	_0B,
	_10(u16),
	_11(u32),
	_12(u32),
	_17(u16),
	_19(u16),
}

impl std::fmt::Debug for DialoguePart {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::String(s) => s.fmt(f),
			Self::Line => write!(f, "Line"),
			Self::Page => write!(f, "Page"),
			Self::_03 => write!(f, "_03"),
			Self::_09 => write!(f, "_09"),
			Self::_0B => write!(f, "_0B"),
			Self::_10(v) => f.debug_tuple("_10").field(v).finish(),
			Self::_11(v) => f.debug_tuple("_11").field(v).finish(),
			Self::_12(v) => f.debug_tuple("_12").field(v).finish(),
			Self::_17(v) => f.debug_tuple("_17").field(v).finish(),
			Self::_19(v) => f.debug_tuple("_19").field(v).finish(),
		}
	}
}

fn dialogue(f: &mut Reader) -> Result<Dialogue, DialogueError> {
	let mut out = Vec::new();
	let mut scratch = Vec::new();
	loop {
		let byte = f.u8()?;
		if byte >= 0x20 {
			scratch.push(byte);
		} else {
			if !scratch.is_empty() {
				match String::from_utf8(scratch) {
					Ok(text) => out.push(DialoguePart::String(text)),
					Err(e) => {
						let text = String::from_utf8_lossy(&e.into_bytes()).into_owned();
						return StringSnafu { text }.fail();
					}
				}
				scratch = Vec::new();
			}
			match byte {
				0x00 => break,
				0x01 => out.push(DialoguePart::Line),
				0x02 => out.push(DialoguePart::Page),
				0x03 => out.push(DialoguePart::_03),
				0x09 => out.push(DialoguePart::_09),
				0x0B => out.push(DialoguePart::_0B),
				0x10 => out.push(DialoguePart::_10(f.u16()?)),
				0x11 => out.push(DialoguePart::_11(f.u32()?)),
				0x12 => out.push(DialoguePart::_12(f.u32()?)),
				0x17 => out.push(DialoguePart::_17(f.u16()?)),
				0x19 => out.push(DialoguePart::_19(f.u16()?)),
				byte => return BadControlSnafu { byte }.fail(),
			}
		}
	}
	Ok(Dialogue(out))
}

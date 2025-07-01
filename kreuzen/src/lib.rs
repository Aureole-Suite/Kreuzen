use std::{collections::HashMap, sync::{LazyLock, Mutex}};

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
		} else if name.starts_with("BTLSET") {
			Type::Btlset
		} else if name.starts_with("BookData") {
			Type::BookData
		} else if name.starts_with("FC_auto") {
			Type::FcAuto
		} else if name.ends_with("Table") || name.ends_with("Data") {
			Type::Table
		} else if name.starts_with("AniBtlKisinKamae") {
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
				println!("{}", entry.name);
				read_func(f.at(entry.start)?, end).context(FunctionSnafu { name: &entry.name })?;
			}
			_ => {}
		}
	}

	Ok(())
}

pub static COUNTS: LazyLock<Mutex<HashMap<ArrayVec<u8, 2>, usize>>> = LazyLock::new(Default::default);

fn read_func(mut f: Reader, end: usize) -> Result<(), FunctionError> {
	while !at_end(&mut f, end) {
		let pos = f.pos();
		match read_op(&mut f).context(OpSnafu { pos }) {
			Ok(op) => {
				println!("{:05X} {op:?}", pos);
			}
			Err(e) => {
				if let FunctionError::Op{source:OpError::UnknownOp { code }, ..} = e {
					let mut op = ArrayVec::new();
					op.push(code);
					*COUNTS.lock().unwrap().entry(op).or_default() += 1;
				} else if let FunctionError::Op{source:OpError::UnknownSub { code, sub }, ..} = e {
					let mut op = ArrayVec::new();
					op.push(code);
					op.push(sub);
					*COUNTS.lock().unwrap().entry(op).or_default() += 1;
				}
				for e in snafu::ErrorCompat::iter_chain(&e) {
					println!("{e}");
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
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
	_0(i32),
	_1c(Op), // op
	_1e(u16),
	_1f(u8),
	_20(u8),
	_21(u16, u8),
	_23(u8),
	_24(i32),
	_25(u16),
	Bin(u8, Box<Expr>, Box<Expr>),
	Un(u8, Box<Expr>),
}

impl Op {
	pub fn name(arg: &[u8]) -> Option<&'static str> {
		Some(match arg {
			[0x01] => "return",
			[0x02] => "call",
			[0x03] => "goto",
			[0x05] => "if",
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
			write!(f, ":{:04X}", self.unk)?;
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
	#[snafu(display("unknown op {code:02X}"))]
	UnknownOp { code: u8 },
	#[snafu(display("unknown op {code:02X}{sub:02X}"))]
	UnknownSub { code: u8, sub: u8 },
	#[snafu(display("failed to read expr"))]
	Expr {
		source: ExprError,
	},
}

fn read_op(f: &mut Reader) -> Result<Op, OpError> {
	let pos = f.pos();
	let code = f.u8()?;
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

	match code {
		0x00 => unreachable!(),
		0x01 => unreachable!(),
		0x02 => {
			op.push(f.u8()?);
			op.push(f.str()?);
			let n = f.u8()?;
			for _ in 0..n {
				op.push(call_arg(f)?);
			}
		}
		0x03 => {
			op.push(Arg::Label(f.u32()?));
		}
		0x05 => {
			op.push(expr(f).context(ExprSnafu)?);
			op.push(Arg::Label(f.u32()?));
		}
		0x2C => {
			op.push(f.u16()?);
			op.push(f.str()?);
			op.push(f.u8()?);
			op.push(f.u8()?);
			op.push(f.u8()?);
			op.push(f.u8()?);
			op.push(f.u8()?);
			op.push(f.f32()?);
			op.push(f.f32()?);
			op.push(f.f32()?);
			op.push(f.f32()?);
			op.push(f.u8()?);
			op.push(f.u8()?);
		}
		0x2D => {
			op.push(f.u16()?);
			op.push(f.u32()?);
			op.push(f.u8()?);
		}
		0x32 => match op.sub(f.u8()?) {
			0x0A => {
				op.push(f.u16()?);
				op.push(f.u8()?);
				op.push(f.str()?);
				op.push(f.u32()?);
			}
			sub => return UnknownSubSnafu { code, sub }.fail(),
		}
		0x3B => match op.sub(f.u8()?) {
			0 | 0x32 => {
				op.push(call_arg2(f)?);
				op.push(call_arg2(f)?);
				op.push(call_arg2(f)?);
				op.push(f.f32()?);
				op.push(call_arg2(f)?);
				op.push(f.u16()?);
				op.push(f.u16()?);
				op.push(f.f32()?);
				op.push(f.f32()?);
				op.push(f.f32()?);
				op.push(call_arg2(f)?);
				op.push(f.str()?);
				op.push(call_arg2(f)?);
				op.push(call_arg2(f)?);
				op.push(f.u16()?);
				op.push(f.u16()?);
				op.push(f.u16()?);
				op.push(f.u16()?);
				op.push(f.u16()?);
				op.push(f.u16()?);
				op.push(f.f32()?);
			}
			0x3A => {
				op.push(f.u16()?);
				op.push(call_arg2(f)?);
				op.push(call_arg2(f)?);
				op.push(call_arg2(f)?);
				op.push(call_arg2(f)?);
			}
			0x3E | 0x3F => {
				op.push(f.u16()?);
			}
			sub => return UnknownSubSnafu { code, sub }.fail(),
		}
		0x3C => match op.sub(f.u8()?) {
			1 => {
				op.push(f.u16()?);
				op.push(f.str()?);
				op.push(f.str()?);
			}
			3 => {
				op.push(f.u16()?);
				op.push(f.str()?);
				op.push(f.str()?);
				op.push(f.str()?);
				op.push(f.str()?);
				op.push(f.str()?);
			}
			4 => {
				op.push(f.u16()?);
				op.push(f.str()?);
			}
			5 => {
				op.push(f.u16()?);
				op.push(f.str()?);
				op.push(f.str()?);
				op.push(f.str()?);
				op.push(f.str()?);
				op.push(f.str()?);
			}
			sub => return UnknownSubSnafu { code, sub }.fail(),
		}
		0x7A => match op.sub(f.u8()?) {
			0 => {
				op.push(f.str()?);
			}
			1 | 2 => {
				op.push(f.u16()?);
				op.push(f.str()?);
			}
			3 => {
				op.push(f.str()?);
			}
			sub => return UnknownSubSnafu { code, sub }.fail(),
		}
		code => return UnknownOpSnafu { code }.fail(),
	}
	Ok(op)
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

fn expr(f: &mut Reader) -> Result<Expr, ExprError> {
	let mut stack = Vec::new();
	loop {
		let pos = f.pos();
		match f.u8()? {
			0x00 => stack.push(Expr::_0(f.i32()?)),
			0x01 => break,
			0x1c => {
				let op = read_op(f).context(ExprOpSnafu { pos })?;
				stack.push(Expr::_1c(op));
			}
			0x1e => stack.push(Expr::_1e(f.u16()?)),
			0x1f => stack.push(Expr::_1f(f.u8()?)),
			0x20 => stack.push(Expr::_20(f.u8()?)),
			0x21 => stack.push(Expr::_21(f.u16()?, f.u8()?)),
			0x23 => stack.push(Expr::_23(f.u8()?)),
			0x24 => stack.push(Expr::_24(f.i32()?)),
			0x25 => stack.push(Expr::_25(f.u16()?)),

			v@(0x08) => {
				let a = stack.pop().context(EmptyStackSnafu)?;
				stack.push(Expr::Un(v, Box::new(a)));
			}
			v@(0x09 | 0x0B | 0x28) => {
				let a = stack.pop().context(EmptyStackSnafu)?;
				let b = stack.pop().context(EmptyStackSnafu)?;
				stack.push(Expr::Bin(v, Box::new(b), Box::new(a)));
			}
			code => return UnknownExprSnafu { code, stack }.fail(),
		}
	}
	if stack.len() == 1 {
		Ok(stack.pop().unwrap())
	} else {
		panic!("expected 1 item on stack, got {stack:?}");
	}
}

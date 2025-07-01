use std::ffi::{CStr, CString};

use gospel::read::{Reader, Le as _};
use snafu::{OptionExt as _, ResultExt as _};

#[extend::ext]
impl<'a> Reader<'a> {
	fn str(&mut self) -> Result<&'a str, gospel::read::Error> {
		let pos = self.pos();
		let cstr = self.cstr()?;
		cstr.to_str().map_err(|e| gospel::read::Error::Other {
			pos,
			source: Box::new(e),
		})
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
				print!("{:64}\t{:#1.32X}", entry.name, f.at(entry.start).unwrap().dump().num_width_as(0xFFFFF).end(end));
				read_func(f.at(entry.start)?, end).context(FunctionSnafu { name: &entry.name })?;
			}
			_ => {}
		}
	}

	Ok(())
}

fn read_func(mut f: Reader, end: usize) -> Result<(), FunctionError> {
	while !at_end(&mut f, end) {
		let pos = f.pos();
		match read_op(&mut f).context(OpSnafu { pos }) {
			Ok(()) => {}
			Err(e) => {
				if !matches!(e, FunctionError::Op{source:OpError::UnknownOp { .. }, ..}) {
					for e in snafu::ErrorCompat::iter_chain(&e) {
						println!("{e}");
					}
				}
				print!("{:#1X}", f.at(pos).unwrap().dump().num_width_as(0xFFFF).end(end));
				break;
			}
		}
	}
	Ok(())
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
	#[snafu(display("failed to read expr"))]
	Expr {
		source: ExprError,
	},
}

fn read_op(f: &mut Reader) -> Result<(), OpError> {
	let pos = f.pos();
	let op = f.u8()?;
	if op == 0x00 {
		println!("{pos:04X}:nop");
		return Ok(());
	}
	if op == 0x01 {
		println!("{pos:04X}:return");
		return Ok(());
	}

	let line = f.u16()?;
	f.check_u8(0)?;
	let x = f.u8()?;
	let h = format!("{pos:04X}:{line:04X}@{x:02X}  ");

	match op {
		0x00 => unreachable!(),
		0x02 => {
			let a = f.u8()?;
			let b = f.str()?;
			let n = f.u8()?;
			let mut args = Vec::with_capacity(n as usize);
			for _ in 0..n {
				args.push(call_arg(f)?);
			}
			println!("{h}Call({a} {b:?} {args:?}");
		}
		0x03 => {
			let b = f.u32()?;
			println!("{h}goto => {b:08X}");
		}
		0x05 => {
			let expr = expr(f).context(ExprSnafu)?;
			let addr = f.u32()?;
			println!("{h}if {expr:?} => {addr:08X}");
		}
		0x3C => match f.u8()? {
			1 => {
				let a = f.u16()?;
				let s1 = f.str()?;
				let s2 = f.str()?;
				println!("{h}3C:1({a} {s1:?} {s2:?})");
			}
			3 => {
				let a = f.u16()?;
				let s1 = f.str()?;
				let s2 = f.str()?;
				let s3 = f.str()?;
				let s4 = f.str()?;
				let s5 = f.str()?;
				println!("{h}3C:3({a} {s1:?} {s2:?} {s3:?} {s4:?} {s5:?})");
			}
			4 => {
				let a = f.u16()?;
				let s1 = f.str()?;
				println!("{h}3C:4({a} {s1:?})");
			}
			5 => {
				let a = f.u16()?;
				let s1 = f.str()?;
				let s2 = f.str()?;
				let s3 = f.str()?;
				let s4 = f.str()?;
				let s5 = f.str()?;
				println!("{h}3C:5({a} {s1:?} {s2:?} {s3:?} {s4:?} {s5:?})");
			}
			code => return UnknownOpSnafu { code }.fail(),
		}
		0x7A => match f.u8()? {
			0 => {
				let s1 = f.str()?;
				println!("{h}7A:0({s1:?})");
			}
			1 => {
				let s1 = f.u16()?;
				let s2 = f.str()?;
				println!("{h}7A:1({s1} {s2:?})");
			}
			2 => {
				let s1 = f.u16()?;
				let s2 = f.str()?;
				println!("{h}7A:2({s1} {s2:?})");
			}
			3 => {
				let s1 = f.str()?;
				println!("{h}7A:3({s1})");
			}
			code => return UnknownOpSnafu { code }.fail(),
		}
		code => return UnknownOpSnafu { code }.fail(),
	}
	Ok(())
}

fn at_end(f: &mut Reader<'_>, end: usize) -> bool {
	let rest = &f.data()[f.pos()..end];
	rest.len() <= 3 && rest.iter().all(|&b| b == 0)
}

#[derive(Debug, Clone, PartialEq)]
enum CallArg {
	_55(u32, u8),
	_11(u32, u8),
	_22(f32, f32),
	_33(f32, u8),
	_44(f32, u8), // sometimes a string
	_DD(String), // sometimes a second string
	_FF(u32, u8),
	_EE(f32, u8),
	Unknown(u8),
}

fn call_arg(f: &mut Reader) -> Result<CallArg, OpError> {
	Ok(match f.u8()? {
		0x11 => CallArg::_11(f.u32()?, f.u8()?),
		0x22 => CallArg::_22(f.f32()?, f.f32()?),
		0x33 => CallArg::_33(f.f32()?, f.u8()?),
		0x44 => CallArg::_44(f.f32()?, f.u8()?), // also a string sometimes?
		0x55 => CallArg::_55(f.u32()?, f.u8()?),
		0xDD => CallArg::_DD(f.str()?.to_owned()), // sometimes a second string
		0xEE => CallArg::_EE(f.f32()?, f.u8()?),
		0xFF => CallArg::_FF(f.u32()?, f.u8()?),
		v => CallArg::Unknown(v),
	})
}

#[derive(Debug, Clone, PartialEq)]
enum Expr {
	_0(i32),
	_1c(()), // op
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

#[derive(Debug, snafu::Snafu)]
pub enum ExprError {
	#[snafu(display("invalid read (at {location})"), context(false))]
	Read {
		source: gospel::read::Error,
		#[snafu(implicit)]
		location: snafu::Location,
	},
	#[snafu(display("unknown expr {code:02X}"))]
	UnknownExpr { code: u8 },
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
			v@(0x28) => {
				let a = stack.pop().context(EmptyStackSnafu)?;
				let b = stack.pop().context(EmptyStackSnafu)?;
				stack.push(Expr::Bin(v, Box::new(b), Box::new(a)));
			}
			code => return UnknownExprSnafu { code }.fail(),
		}
	}
	if stack.len() == 1 {
		Ok(stack.pop().unwrap())
	} else {
		panic!("expected 1 item on stack, got {stack:?}");
	}
}

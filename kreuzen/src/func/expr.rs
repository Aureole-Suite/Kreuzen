use gospel::read::Le as _;
use snafu::{OptionExt as _, ResultExt as _};

#[derive(Debug, snafu::Snafu)]
pub enum ReadError {
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
		#[snafu(source(from(super::read::OpReadError, Box::new)))]
		source: Box<super::read::OpReadError>,
	},
	#[snafu(display("empty stack"))]
	EmptyStack,
	#[snafu(display("overfull stack"))]
	OverfullStack { stack: Vec<Expr> },
}

#[derive(Clone, PartialEq)]
pub enum Expr {
	Int(i32),
	Op(super::Op),
	Flag(u16),
	Var(u8),
	Attr(u8),
	CharAttr(crate::Char, u8),
	Rand,
	Global(u8),
	_24(i32),
	_25(u16),
	Bin(BinOp, Box<Expr>, Box<Expr>),
	Un(UnOp, Box<Expr>),
	Ass(AssOp, Box<Expr>),
}

impl std::fmt::Debug for Expr {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Int(a) => a.fmt(f),
			Self::Op(a) => a.fmt(f),
			Self::Flag(a) => f.debug_tuple("Flag").field(a).finish(),
			Self::Var(a) => f.debug_tuple("Var").field(a).finish(),
			Self::Attr(a) => f.debug_tuple("Attr").field(a).finish(),
			Self::CharAttr(a, arg1) => f.debug_tuple("CharAttr").field(a).field(arg1).finish(),
			Self::Rand => write!(f, "Rand"),
			Self::Global(a) => f.debug_tuple("Global").field(a).finish(),
			Self::_24(a) => f.debug_tuple("_24").field(a).finish(),
			Self::_25(a) => f.debug_tuple("_25").field(a).finish(),
			Self::Bin(op, a, b) => f.debug_tuple(format!("{op:?}").as_str()).field(a).field(b).finish(),
			Self::Un(op, a) => f.debug_tuple(format!("{op:?}").as_str()).field(a).finish(),
			Self::Ass(op, a) => f.debug_tuple(format!("{op:?}").as_str()).field(a).finish(),
		}
	}
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

impl Expr {
	pub fn read(f: &mut crate::VReader) -> Result<Expr, ReadError> {
		let mut stack = Vec::new();
		loop {
			let pos = f.pos();
			match f.u8()? {
				0x00 => stack.push(Expr::Int(f.i32()?)),
				0x01 => break,
				0x1C => stack.push(Expr::Op(super::read::read_raw_op(f).context(ExprOpSnafu { pos })?)),
				0x1E => stack.push(Expr::Flag(f.u16()?)),
				0x1F => stack.push(Expr::Var(f.u8()?)),
				0x20 => stack.push(Expr::Attr(f.u8()?)),
				0x21 => stack.push(Expr::CharAttr(crate::Char(f.u16()?), f.u8()?)),
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
}

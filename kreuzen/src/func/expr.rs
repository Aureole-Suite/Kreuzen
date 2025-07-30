use gospel::{read::Le as _, write::Le as _};
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

#[derive(Debug, snafu::Snafu)]
pub enum WriteError {
	#[snafu(transparent, context(false))]
	OpWrite {
		#[snafu(source(from(super::write::OpWriteError, Box::new)))]
		source: Box<super::write::OpWriteError>,
	},
}

#[derive(Clone, PartialEq)]
pub enum Expr {
	Int(i32),
	Op(super::Op),
	Flag(crate::types::Flag),
	Var(crate::types::Var),
	Attr(crate::types::Attr),
	CharAttr(crate::types::CharAttr),
	Rand,
	Global(crate::types::Global),
	SystemFlags(crate::types::Flags32),
	NumReg(crate::types::NumReg),
	Bin(BinOp, Box<Expr>, Box<Expr>),
	Un(UnOp, Box<Expr>),
	Ass(AssOp, Box<Expr>),
}

impl std::fmt::Debug for Expr {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Int(a) => a.fmt(f),
			Self::Op(a) => a.fmt(f),
			Self::Flag(a) => a.fmt(f),
			Self::Var(a) => a.fmt(f),
			Self::Attr(a) => a.fmt(f),
			Self::CharAttr(a) => a.fmt(f),
			Self::Rand => write!(f, "Rand"),
			Self::Global(a) => a.fmt(f),
			Self::SystemFlags(a) => a.fmt(f),
			Self::NumReg(a) => a.fmt(f),
			Self::Bin(op, a, b) => {
				write!(f, "(")?;
				a.fmt(f)?;
				write!(f, " {} ", op.as_str())?;
				b.fmt(f)?;
				write!(f, ")")
			}
			Self::Un(op, a) => {
				write!(f, "{}", op.as_str())?;
				a.fmt(f)
			}
			Self::Ass(op, a) => {
				write!(f, "{} ", op.as_str())?;
				a.fmt(f)
			}
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
	Or = 0x0B,      // | and ||
	Add = 0x0C,     // +
	Sub = 0x0D,     // -
	Xor = 0x0F,     // ^
	Mul = 0x10,     // *
	Div = 0x11,     // /
	Mod = 0x12,     // %
}

impl BinOp {
	fn as_str(self) -> &'static str {
		match self {
			BinOp::Eq => "==",
			BinOp::Ne => "!=",
			BinOp::Lt => "<",
			BinOp::Gt => ">",
			BinOp::Le => "<=",
			BinOp::Ge => ">=",
			BinOp::BoolAnd => "&&",
			BinOp::BitAnd => "&",
			BinOp::Or => "||",
			BinOp::Add => "+",
			BinOp::Sub => "-",
			BinOp::Xor => "^",
			BinOp::Mul => "*",
			BinOp::Div => "/",
			BinOp::Mod => "%",
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)] // comment to trick rustfmt
#[derive(num_enum::TryFromPrimitive, num_enum::IntoPrimitive)]
#[repr(u8)]
pub enum UnOp {
	BoolNot = 0x08, // !
	Neg = 0x0E,     // -
	BitNot = 0x1D,  // ~
}

impl UnOp {
	fn as_str(self) -> &'static str {
		match self {
			UnOp::BoolNot => "!",
			UnOp::Neg => "-",
			UnOp::BitNot => "~",
		}
	}
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

impl AssOp {
	fn as_str(self) -> &'static str {
		match self {
			AssOp::Ass => "=",
			AssOp::MulAss => "*=",
			AssOp::DivAss => "/=",
			AssOp::ModAss => "%=",
			AssOp::AddAss => "+=",
			AssOp::SubAss => "-=",
			AssOp::AndAss => "&=",
			AssOp::XorAss => "^=",
			AssOp::OrAss => "|=",
		}
	}
}

impl Expr {
	pub(crate) fn read(f: &mut crate::VReader) -> Result<Expr, ReadError> {
		let mut stack = Vec::new();
		loop {
			let pos = f.pos();
			match f.u8()? {
				0x00 => stack.push(Expr::Int(f.i32()?)),
				0x01 => break,
				0x1C => stack.push(Expr::Op(super::read::read_raw_op(f).context(ExprOpSnafu { pos })?)),
				0x1E => stack.push(Expr::Flag(f.u16()?.into())),
				0x1F => stack.push(Expr::Var(f.u8()?.into())),
				0x20 => stack.push(Expr::Attr(f.u8()?.into())),
				0x21 => stack.push(Expr::CharAttr((f.u16()?.into(), f.u8()?).into())),
				0x22 => stack.push(Expr::Rand),
				0x23 => stack.push(Expr::Global(f.u8()?.into())),
				0x24 => stack.push(Expr::SystemFlags(f.u32()?.into())),
				0x25 => {
					stack.push(Expr::NumReg(f.u8()?.into()));
					f.check_u8(0)?; // Reg is 2 bytes here but 1 in op0E, weird
				}

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

	pub(crate) fn write(&self, f: &mut crate::VWriter) -> Result<(), WriteError> {
		self.write0(f)?;
		f.u8(0x01);
		Ok(())
	}

	fn write0(&self, f: &mut crate::VWriter) -> Result<(), WriteError> {
		match self {
			Expr::Int(i) => { f.u8(0x00); f.i32(*i); }
			Expr::Op(op) => { f.u8(0x1C); super::write::write_raw_op(f, op)?; }
			Expr::Flag(i) => { f.u8(0x1E); f.u16(i.0); }
			Expr::Var(i) => { f.u8(0x1F); f.u8(i.0); }
			Expr::Attr(i) => { f.u8(0x20); f.u8(i.0); }
			Expr::CharAttr(i) => { f.u8(0x21); f.u16(i.0.0); f.u8(i.1); }
			Expr::Rand => { f.u8(0x22); }
			Expr::Global(i) => { f.u8(0x23); f.u8(i.0); },
			Expr::SystemFlags(i) => { f.u8(0x24); f.u32(i.0); }
			Expr::NumReg(i) => { f.u8(0x25); f.u8(i.0); f.u8(0); }
			Expr::Bin(op, a, b) => { a.write0(f)?; b.write0(f)?; f.u8(*op as u8); }
			Expr::Un(op, a) => { a.write0(f)?; f.u8(*op as u8); }
			Expr::Ass(op, a) => { a.write0(f)?; f.u8(*op as u8); }
		}
		Ok(())
	}
}

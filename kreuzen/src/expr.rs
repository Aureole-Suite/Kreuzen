use gospel::read::Le as _;
use rootcause::prelude::ResultExt as _;
use crate::{io::CReader, types::*};

#[rustfmt::skip]
#[derive(Clone, PartialEq, derive_more::Debug, derive_more::From)]
pub enum Expr {
	#[debug("{_0:?}")] Int(i32),
	#[debug("{_0:?}")] Op(crate::code::Op),
	#[debug("{_0:?}")] Flag(Flag),
	#[debug("{_0:?}")] Var(Var),
	#[debug("{_0:?}")] Attr(Attr),
	#[debug("{_0:?}")] CharAttr(CharAttr),
	Rand,
	#[debug("{_0:?}")] Global(Global),
	#[from(ignore)] #[debug("{_0:?}")] SystemFlags(Flags32),
	#[debug("{_0:?}")] NumReg(NumReg),
	#[from(ignore)] #[debug("({_1:?} {_0} {_2:?})")] Bin(BinOp, Box<Expr>, Box<Expr>),
	#[from(ignore)] #[debug("{_0}{_1:?}")] Un(UnOp, Box<Expr>),
	#[from(ignore)] #[debug("{_0:?} {_1:?}")] Ass(AssOp, Box<Expr>),
}

#[rustfmt::skip]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[derive(derive_more::TryFrom, derive_more::Display)]
#[try_from(repr)]
#[repr(u8)]
pub enum BinOp {
	#[display("==")] Eq = 0x02,
	#[display("!=")] Ne = 0x03,
	#[display("<")] Lt = 0x04,
	#[display(">")] Gt = 0x05,
	#[display("<=")] Le = 0x06,
	#[display(">=")] Ge = 0x07,
	#[display("&&")] BoolAnd = 0x09,
	#[display("&")] BitAnd = 0x0A,
	#[display("||")] Or = 0x0B, // also |
	#[display("+")] Add = 0x0C,
	#[display("-")] Sub = 0x0D,
	#[display("^")] Xor = 0x0F,
	#[display("*")] Mul = 0x10,
	#[display("/")] Div = 0x11,
	#[display("%")] Mod = 0x12,
}

#[rustfmt::skip]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[derive(derive_more::TryFrom, derive_more::Display)]
#[try_from(repr)]
#[repr(u8)]
pub enum UnOp {
	#[display("!")] BoolNot = 0x08,
	#[display("-")] Neg = 0x0E,
	#[display("~")] BitNot = 0x1D,
}

#[rustfmt::skip]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[derive(derive_more::TryFrom, derive_more::Display)]
#[try_from(repr)]
#[repr(u8)]
pub enum AssOp {
	#[display("=")] Ass = 0x13,
	#[display("*=")] MulAss = 0x14,
	#[display("/=")] DivAss = 0x15,
	#[display("%=")] ModAss = 0x16,
	#[display("+=")] AddAss = 0x17,
	#[display("-=")] SubAss = 0x18,
	#[display("&=")] AndAss = 0x19,
	#[display("^=")] XorAss = 0x1A,
	#[display("|=")] OrAss = 0x1B,
}

struct StackView;
impl rootcause::handlers::AttachmentHandler<Vec<Expr>> for StackView {
	fn display(value: &Vec<Expr>, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
		if value.is_empty() {
			writeln!(f, "Stack: (empty)")?;
		}
		for val in value {
			writeln!(f, "Stack:")?;
			writeln!(f, "  {val:?}")?;
		}
		Ok(())
	}

	fn debug(value: &Vec<Expr>, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
		std::fmt::Debug::fmt(value, f)
	}
}

impl Expr {
	pub(crate) fn read(f: &mut CReader) -> rootcause::Result<Expr> {
		let pos = f.pos();
		let mut stack = Vec::new();
		read_inner(f, &mut stack)
			.attach_custom_with::<StackView, _, _>(|| std::mem::take(&mut stack))
			.context_with(|| format!("failed to read expr at {pos:04X}"))?;

		if stack.len() == 1 {
			Ok(stack.pop().unwrap())
		} else {
			Err(rootcause::report!("stack is overfull").attach_custom::<StackView, _>(stack))
		}
	}
}

fn read_inner(f: &mut CReader, stack: &mut Vec<Expr>) -> Result<(), rootcause::Report> {
	loop {
		match f.u8()? {
			0x00 => stack.push(Expr::Int(f.i32()?)),
			0x01 => break,
			0x1C => {
				match crate::code::read_op(f)? {
					crate::code::FlatOp::Op(op) => stack.push(Expr::Op(op)),
					op => rootcause::bail!("expr can't contain {op:?}"),
				}
			}
			0x1E => stack.push(Flag(f.u16()?).into()),
			0x1F => stack.push(Var(f.u8()?).into()),
			0x20 => stack.push(Attr(f.u8()?).into()),
			0x21 => stack.push(CharAttr(Char(f.u16()?), f.u8()?).into()),
			0x22 => stack.push(Expr::Rand),
			0x23 => stack.push(Global(f.u8()?).into()),
			0x24 => stack.push(Expr::SystemFlags(f.u32()?.into())),
			0x25 => {
				stack.push(NumReg(f.u8()?).into());
				f.check_u8(0)?; // Reg is 2 bytes here but 1 in op0E, weird
			}

			v if let Ok(v) = BinOp::try_from(v) => {
				if stack.len() < 2 {
					rootcause::bail!("not enough args for binary {v}");
				}
				let b = stack.pop().unwrap();
				let a = stack.pop().unwrap();
				stack.push(Expr::Bin(v, Box::new(a), Box::new(b)));
			}
			v if let Ok(v) = UnOp::try_from(v) => {
				if stack.is_empty() {
					rootcause::bail!("not enough args for unary {v}");
				}
				let a = stack.pop().unwrap();
				stack.push(Expr::Un(v, Box::new(a)));
			}
			v if let Ok(v) = AssOp::try_from(v) => {
				if stack.is_empty() {
					rootcause::bail!("not enough args for {v}");
				}
				let a = stack.pop().unwrap();
				stack.push(Expr::Ass(v, Box::new(a)));
			}

			code => rootcause::bail!("unknown expr op: {code:02X}")
		}
	}
	Ok(())
}

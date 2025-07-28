use std::sync::LazyLock;
use arrayvec::ArrayVec;

pub mod read;
pub mod write;
pub mod expr;
pub mod dial;

pub use expr::Expr;
pub use dial::Dialogue;

mod spec;
use spec::Spec;

pub static SPEC: LazyLock<Spec> = LazyLock::new(|| Spec::parse(include_str!("../../ed85.txt")));

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct OpMeta {
	pub line: u16,
	pub width: u8,
}

impl OpMeta {
	fn fmt<'a, 'b>(&self, f: &'a mut std::fmt::Formatter<'b>) -> Result<&'a mut std::fmt::Formatter<'b>, std::fmt::Error> {
		write!(f, "{self:?}:")?;
		Ok(f)
	}
}

impl std::fmt::Debug for OpMeta {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", self.line)?;
		if self.width != 0xFF {
			write!(f, "~")?;
		}
		Ok(())
	}
}

impl Default for OpMeta {
	fn default() -> Self {
		OpMeta { line: 0, width: 0xFF }
	}
}

#[derive(Clone, PartialEq)]
pub struct Op {
	pub code: ArrayVec<u8, 4>,
	pub meta: OpMeta,
	pub args: Vec<Arg>
}

#[derive(Clone, PartialEq)]
pub enum Stmt {
	Op(Op),
	If(OpMeta, Expr, Vec<Stmt>, Option<(OpMeta, Vec<Stmt>)>),
	While(OpMeta, Expr, Vec<Stmt>, OpMeta),
	Break(OpMeta),
	Continue(OpMeta),
	Switch(OpMeta, Expr, Vec<(Case, Vec<Stmt>)>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Case {
	Default,
	Case(i32),
	// There's a freaky switch in c0400:ronald_setting that has a switch with bodies but no cases.
	None,
}

impl std::fmt::Debug for Stmt {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Op(o) => o.fmt(f),
			Self::If(m, e, then, els) => {
				m.fmt(f)?;
				let mut tup = f.debug_tuple("If");
				tup.field(e);
				tup.field(then);
				tup.finish()?;
				if let Some((m2, els)) = els {
					f.write_str(" ")?;
					m2.fmt(f)?;
					f.write_str("else ")?;
					if let [stmt@Stmt::If(..)] = els.as_slice() {
						stmt.fmt(f)?;
					} else {
						els.fmt(f)?;
					}
				}
				Ok(())
			},
			Self::While(arg0, arg1, arg2, arg3) => arg0.fmt(f)?.debug_tuple("While").field(arg1).field(arg2).field(arg3).finish(),
			Self::Break(arg0) => arg0.fmt(f)?.debug_tuple("Break").finish(),
			Self::Continue(arg0) => arg0.fmt(f)?.debug_tuple("Continue").finish(),
			Self::Switch(arg0, arg1, arg2) => arg0.fmt(f)?.debug_tuple("Switch").field(arg1).field(arg2).finish(),
		}
	}
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

	Char(crate::types::Char),
	Item(crate::types::Item),
	Magic(crate::types::Magic),
	Flag(crate::types::Flag),
	Global(crate::types::Global),
	Var(crate::types::Var),
	Attr(crate::types::Attr),
	CharAttr(crate::types::CharAttr),
	Flags16(crate::types::Flags16),
	Flags32(crate::types::Flags32),

	Expr(expr::Expr),
	Dyn(Dyn),
	Dialogue(dial::Dialogue),
}

impl std::fmt::Debug for Arg {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Arg::Str(v) => v.fmt(f),
			Arg::U8(v) => v.fmt(f),
			Arg::U16(v) => v.fmt(f),
			Arg::U32(v) => v.fmt(f),
			Arg::I8(v) => v.fmt(f),
			Arg::I16(v) => v.fmt(f),
			Arg::I32(v) => v.fmt(f),
			Arg::F32(v) => v.fmt(f),
			Arg::Char(v) => v.fmt(f),
			Arg::Item(v) => v.fmt(f),
			Arg::Magic(v) => v.fmt(f),
			Arg::Flag(v) => v.fmt(f),
			Arg::Global(v) => v.fmt(f),
			Arg::Var(v) => v.fmt(f),
			Arg::Attr(v) => v.fmt(f),
			Arg::CharAttr(v) => v.fmt(f),
			Arg::Flags16(v) => v.fmt(f),
			Arg::Flags32(v) => v.fmt(f),
			Arg::Expr(v) => v.fmt(f),
			Arg::Dyn(v) => v.fmt(f),
			Arg::Dialogue(v) => v.fmt(f),
		}
	}
}

impl Op {
	pub fn push(&mut self, arg: impl Into<Arg>) {
		self.args.push(arg.into());
	}
}

pub struct OpName<'a> {
	code: &'a [u8],
}

impl std::fmt::Display for OpName<'_> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		let mut prefix = 0;
		for i in 0..=self.code.len() {
			if let Some(s) = SPEC.names.get(&self.code[..i]) {
				f.write_str(s)?;
				prefix = i;
			}
		}
		if prefix == 0 {
			write!(f, "op")?;
		} else if prefix < self.code.len() {
			write!(f, "_")?;
		}
		for byte in &self.code[prefix..] {
			write!(f, "{byte:02X}")?;
		}
		Ok(())
	}
}

impl std::fmt::Debug for Op {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		self.meta.fmt(f)?;
		write!(f, "{}", OpName { code: &self.code })?;
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

#[derive(Clone, PartialEq)]
pub enum Dyn {
	Var(crate::types::Var),
	_33(u32),
	_44(u32),
	_55(u32),
	_DD(String),
	_EE(f32),
	_FF(i32),
}

impl std::fmt::Debug for Dyn {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Var(arg0) => arg0.fmt(f),
			Self::_33(arg0) => f.debug_tuple("Dyn33").field(arg0).finish(),
			Self::_44(arg0) => f.debug_tuple("Dyn44").field(arg0).finish(),
			Self::_55(arg0) => f.debug_tuple("Dyn55").field(arg0).finish(),
			Self::_DD(arg0) => f.debug_tuple("DynDD").field(arg0).finish(),
			Self::_EE(arg0) => f.debug_tuple("DynEE").field(arg0).finish(),
			Self::_FF(arg0) => f.debug_tuple("DynFF").field(arg0).finish(),
		}
	}
}

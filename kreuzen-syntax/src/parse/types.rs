use kreuzen::code::Arg;
use kreuzen::types::*;

use super::parser::{Error, Parser, Result};

/// Context-free values, mirroring the `Print` impls. Values that need the op
/// spec (ops, exprs, statements) are parsed by functions instead.
///
/// A failed parse may leave the cursor mid-value; callers that want to try
/// alternatives must go through `Alt` (or `Parser::test`), which restores it.
pub trait Parse: Sized {
	fn parse(p: &mut Parser) -> Result<Self>;
}

impl Parser<'_, '_> {
	pub fn parse<T: Parse>(&mut self) -> Result<T> {
		T::parse(self)
	}
}

macro_rules! parse_int {
	($($t:ty),*) => {
		$(
			impl Parse for $t {
				fn parse(p: &mut Parser) -> Result<Self> {
					let span = p.next_span();
					let v = p.int()?;
					<$t>::try_from(v).map_err(|_| {
						p.errors.error("value out of range", span);
						Error
					})
				}
			}
		)*
	};
}

parse_int!(u8, u16, u32, i8, i16, i32, i64);

/// Also accepts `inf`, `-inf` and `NaN`, as printed by f32's Debug.
impl Parse for f32 {
	fn parse(p: &mut Parser) -> Result<Self> {
		p.alt()
			.test(|p| p.float())
			.test_kw("inf", |_| Ok(f32::INFINITY))
			.test_kw("NaN", |_| Ok(f32::NAN))
			.test(|p| {
				p.punct('-')?;
				p.keyword("inf")?;
				Ok(f32::NEG_INFINITY)
			})
			.finish()
	}
}

impl Parse for String {
	fn parse(p: &mut Parser) -> Result<Self> {
		p.string().map(str::to_owned)
	}
}

macro_rules! parse_tuple {
	($($t:ident)*) => {
		#[expect(non_snake_case)]
		impl<$($t: Parse,)+> Parse for ($($t,)+) {
			fn parse(p: &mut Parser) -> Result<Self> {
				p.delim('(', |p| {
					$(let $t = p.parse::<$t>()?;)+
					Ok(($($t,)+))
				})
			}
		}
	};
}

parse_tuple!(A);
parse_tuple!(A B);
parse_tuple!(A B C);
parse_tuple!(A B C D);
parse_tuple!(A B C D E);

impl<T: Parse, const N: usize> Parse for [T; N] {
	fn parse(p: &mut Parser) -> Result<Self> {
		p.delim('(', |p| {
			let mut out = Vec::with_capacity(N);
			for _ in 0..N {
				out.push(p.parse::<T>()?);
			}
			out.try_into().map_err(|_| Error)
		})
	}
}

/// Greedy repetition: parses elements until one fails.
impl<T: Parse> Parse for Vec<T> {
	fn parse(p: &mut Parser) -> Result<Self> {
		let mut out = Vec::new();
		while let Ok(v) = p.parse::<T>() {
			out.push(v);
		}
		Ok(out)
	}
}

/// `name[...]` where the contents are parsed by `f`.
pub fn bracket<T>(p: &mut Parser, name: &'static str, f: impl FnOnce(&mut Parser) -> Result<T>) -> Result<T> {
	p.cursor.keyword(name)?;
	p.delim('[', f)
}

macro_rules! parse_bracket {
	($($ty:ident=> $name:literal,)*) => {
		$(
			impl Parse for $ty {
				fn parse(p: &mut Parser) -> Result<Self> {
					bracket(p, $name, |p| p.parse()).map($ty)
				}
			}
		)*
	};
}

parse_bracket!(
	Item => "item",
	Battle => "battle",
	Magic => "magic",
	Sound => "sound",
	Music  => "music",
	Flag  => "flag",
	Global => "global",
	Var => "var",
	FuncArg => "func_arg",
	NumReg => "num_reg",
	StrReg => "str_reg",
	Attr => "attr",
	SystemFlags => "system",
);

impl Parse for Flags8 {
	fn parse(p: &mut Parser) -> Result<Self> {
		p.parse::<u8>().map(Flags8)
	}
}

impl Parse for Flags16 {
	fn parse(p: &mut Parser) -> Result<Self> {
		p.parse::<u16>().map(Flags16)
	}
}

impl Parse for Flags32 {
	fn parse(p: &mut Parser) -> Result<Self> {
		p.parse::<u32>().map(Flags32)
	}
}

impl Parse for Char {
	fn parse(p: &mut Parser) -> Result<Self> {
		bracket(p, "char", |p| {
			p.alt()
				.test_kw("self", |_| Ok(Char(0xFFFE)))
				.test_kw("null", |_| Ok(Char(0xFFFF)))
				.test(|p| p.parse().map(Char))
				.finish()
		})
	}
}

/// `char[...].N`
impl Parse for CharAttr {
	fn parse(p: &mut Parser) -> Result<Self> {
		let c = p.parse()?;
		p.glued_punct('.')?;
		let a = p.parse()?;
		Ok(CharAttr(c, a))
	}
}

/// `btlset[N]:battle[M]`, the syntax for `Arg::Battle`.
pub fn battle_arg(p: &mut Parser) -> Result<Arg> {
	let a = bracket(p, "btlset", |p| p.parse())?;
	p.glued_punct(':')?;
	let b = p.parse()?;
	Ok(Arg::Battle(a, b))
}

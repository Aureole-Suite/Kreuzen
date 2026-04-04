#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Char(pub u16);

struct Tagged<A, B>(A, Option<B>);

impl<A, B> std::fmt::Debug for Tagged<A, B>
where
	A: std::fmt::Debug,
	B: std::fmt::Display,
{
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{:?}", self.0)?;
		if let Some(v) = &self.1 {
			write!(f, ": {v}")?;
		}
		Ok(())
	}
}

impl Char {
	pub fn name(&self) -> Option<&'static str> {
		// Wonder if these are game specific. They probably are.
		Some(match self.0 {
			0xF016 => "field",
			0xF028 => "burst0",
			0xF029 => "burst1",
			0xF02A => "burst2",
			0xF02B => "burst3",
			0xF02C => "skit0",
			0xF02D => "skit1",
			0xF043 => "enemy0",
			0xF044 => "enemy1",
			0xF045 => "enemy2",
			0xF046 => "enemy3",
			0xF047 => "enemy4",
			0xF048 => "enemy5",
			0xF049 => "enemy6",
			0xF04A => "enemy7",
			0xF093 => "kisin0",
			0xF094 => "kisin1",
			0xF095 => "kisin2",

			0xFD00 => "battle0",
			0xFD01 => "battle1",
			0xFD02 => "battle2",
			0xFD03 => "battle3",
			0xFD04 => "battle4",
			0xFD05 => "battle5",
			0xFD06 => "battle6",
			0xFD07 => "battle7",
			0xFD08 => "battle8",
			0xFD09 => "battle9",
			0xFD0A => "battle10",
			0xFD0B => "battle11",
			0xFD0C => "battle12",
			0xFD0D => "battle13",
			0xFD0E => "battle14",
			0xFD0F => "battle15",
			0xFD10 => "battle16",
			0xFD11 => "battle17",
			0xFD12 => "battle18",
			0xFD13 => "battle19",

			// FE12 and FE13 are special cased in op3E
			// FE02..=FE05 and FE15 are special cased in OP40
			0xFFD8 => "ally_loop",

			0xFFFB => "target",
			0xFFFE => "self",
			0xFFFF => "null",

			_ => return None,
		})
	}
}

impl std::fmt::Debug for Char {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.debug_tuple("Char")
			.field(&std::fmt::from_fn(|f| {
				if self.0 >= 0xF000 {
					write!(f, "0x{:04X}", self.0)?;
				} else {
					write!(f, "{}", self.0)?;
				}
				if let Some(v) = self.name() {
					write!(f, ": {v}")?;
				}
				Ok(())
			}))
			.finish()
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Item(pub u16);
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Magic(pub u16);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Flag(pub u16);
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Global(pub u8);
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Var(pub u8);
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct FuncArg(pub u8);
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct NumReg(pub u8);
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct StrReg(pub u8);
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Attr(pub u8);
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct CharAttr(pub Char, pub u8);

impl CharAttr {
	pub fn name(n: u8) -> Option<&'static str> {
		Some(match n {
			8 => "id",
			_ => return None,
		})
	}
}

impl std::fmt::Debug for CharAttr {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.debug_tuple("CharAttr")
			.field(&Tagged(self.0.0, self.0.name()))
			.field(&Tagged(self.1, CharAttr::name(self.1)))
			.finish()
	}
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, derive_more::From, derive_more::Debug)]
#[debug("0x{_0:02X}")]
pub struct Flags8(pub u8);
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, derive_more::From, derive_more::Debug)]
#[debug("0x{_0:04X}")]
pub struct Flags16(pub u16);
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, derive_more::From, derive_more::Debug)]
#[debug("0x{_0:08X}")]
pub struct Flags32(pub u32);

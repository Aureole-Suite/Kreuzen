use derive_more::{From, Into};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, From, Into)]
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
		Some(match self.0 {
			0 => "Rean",
			1 => "Alisa",
			2 => "Elliot",
			3 => "Laura",
			4 => "Machias",
			5 => "Emma",
			6 => "Jusis",
			7 => "Fie",
			8 => "Gaius",
			9 => "Millium",
			10 => "Juna",
			11 => "Kurt",
			12 => "Altina",
			13 => "Musse",
			14 => "Ash",
			15 => "Sara",
			16 => "Aurelia",
			17 => "Agate",
			18 => "Angelica",
			19 => "Olivier",
			20 => "Tita",
			21 => "Tio",
			22 => "Sharon",
			23 => "Crow",
			24 => "Vita",
			25 => "Toval",
			26 => "Estelle",
			27 => "Joshua",
			28 => "Renne",
			29 => "Lloyd",
			30 => "Elie",
			31 => "Randy",
			32 => "Roselia",
			33 => "George",
			34 => "Arseid",
			35 => "Duvalie",
			36 => "Celine",
			37 => "Towa",
			38 => "Elise",
			39 => "Alfin",
			40 => "Noel",
			41 => "Wazy",
			42 => "Rixia",
			43 => "Arios",
			44 => "Rufus",
			45 => "Lapis",
			46 => "Swin",
			47 => "Nadia",
			48 => "Claire",
			49 => "Lechter",
			50 => "McBurn",
			51 => "Cedric",
			52 => "KeA",
			53 => "Scherazard",
			55 => "Meltgunner",
			101 => "Ines",
			102 => "Ennea",
			103 => "Xeno",
			104 => "Leonidas",
			122 => "Patrick",
			150 => "Wayne",
			200 => "Jessica",

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

			// FE12 and FE13 are special cased in op3E
			// FE02..=FE05 and FE15 are special cased in OP40

			0xFFD8 => "ally_loop",

			0xFFFB => "target",
			0xFFFE => "self",
			0xFFFF => "null",

			_ => return None
		})
	}
}

impl std::fmt::Debug for Char {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.debug_tuple("Char")
			.field(&Tagged(self.0, self.name()))
			.finish()
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, From, Into)]
pub struct Item(pub u16);
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, From, Into)]
pub struct Magic(pub u16);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, From, Into)]
pub struct Flag(pub u16);
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, From, Into)]
pub struct Global(pub u8);
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, From, Into)]
pub struct Var(pub u8);
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, From, Into)]
pub struct NumReg(pub u8);
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, From, Into)]
pub struct StrReg(pub u8);
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, From, Into)]
pub struct Attr(pub u8);
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, From, Into)]
pub struct CharAttr(pub Char, pub u8);

impl CharAttr {
	pub fn name(n: u8) -> Option<&'static str> {
		Some(match n {
			8 => "id",
			_ => return None
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

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, From, Into)]
pub struct Flags16(pub u16);
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, From, Into)]
pub struct Flags32(pub u32);

impl std::fmt::Debug for Flags16 {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "0x{:04X}", self.0)
	}
}

impl std::fmt::Debug for Flags32 {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "0x{:08X}", self.0)
	}
}


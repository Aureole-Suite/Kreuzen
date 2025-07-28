use derive_more::{From, Into};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, From, Into)]
pub struct Char(pub u16);

impl std::fmt::Debug for Char {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		if let Some(name) = self.name() {
			write!(f, "Char({i}: {name})", i = self.0)
		} else {
			write!(f, "Char({i})", i = self.0)
		}
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

			0xFFFB => "target",
			0xFFFE => "self",
			0xFFFF => "null",

			_ => return None
		})
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, From, Into)]
pub struct Item(pub u16);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, From, Into)]
pub struct Flag(pub u16);
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, From, Into)]
pub struct Global(pub u8);
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, From, Into)]
pub struct Var(pub u8);
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, From, Into)]
pub struct Attr(pub u8);
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, From, Into)]
pub struct CharAttr(pub Char, pub u8);

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


mod opcode;
use std::collections::BTreeMap;
use std::sync::LazyLock;

use crate::Game;
pub use opcode::Opcode;

mod parse;
use parse::{Lines, parse_lines, parse_spec};

macro_rules! spec {
	($($name:ident),* $(,)?) => {
		#[cfg(test)]
		mod parse_test {
			use super::*;
			$(#[test] fn $name() {
				LazyLock::force(&lines::$name);
			})*
		}

		#[allow(non_upper_case_globals)]
		#[cfg(not(feature = "live"))]
		mod text {
			$(pub static $name: &str =
				include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/../spec/", stringify!($name), ".txt"));)*
		}

		#[allow(non_upper_case_globals)]
		#[cfg(feature = "live")]
		mod text {
			use super::*;
			$(pub static $name: LazyLock<String> = LazyLock::new(|| {
				std::fs::read_to_string(concat!(env!("CARGO_MANIFEST_DIR"), "/../spec/", stringify!($name), ".txt"))
					.unwrap()
			});)*
		}

		fn text_for(name: &str) -> Option<&'static str> {
			match name {
				$(stringify!($name) => Some(&text::$name),)*
				_ => None,
			}
		}

		#[allow(non_upper_case_globals)]
		mod lines {
			use super::*;
			$(pub static $name: LazyLock<Lines> = LazyLock::new(|| parse_lines(stringify!($name)));)*
		}

		fn lines_for(name: &str) -> Option<&'static Lines> {
			match name {
				$(stringify!($name) => Some(&lines::$name),)*
				_ => None,
			}
		}

		#[allow(non_upper_case_globals)]
		mod specs {
			use super::*;
			$(pub static $name: LazyLock<Spec> = LazyLock::new(|| parse_spec(&lines::$name));)*
		}
	};
}

spec! {
	cs1, cs1_1, cs1_2, cs1_3, cs1_menu,
	cs2, cs2_1, cs2_menu,
	cs3, cs3_1, cs3_2, cs3_3,
	cs4, cs4_1,
	reverie, reverie_1,
	tx,
}

pub fn for_game(game: Game, variant: u8) -> &'static Spec {
	match game {
		Game::Cs1 if variant == 0 => &specs::cs1,
		Game::Cs1 if variant == 1 => &specs::cs1_1,
		Game::Cs1 if variant == 2 => &specs::cs1_2,
		Game::Cs1 if variant == 3 => &specs::cs1_3,
		Game::Cs1 if variant == 100 => &specs::cs1_menu,
		Game::Cs2 if variant == 0 => &specs::cs2,
		Game::Cs2 if variant == 1 => &specs::cs2_1,
		Game::Cs2 if variant == 100 => &specs::cs2_menu,
		Game::Cs3 if variant == 0 => &specs::cs3,
		Game::Cs3 if variant == 1 => &specs::cs3_1,
		Game::Cs3 if variant == 2 => &specs::cs3_2,
		Game::Cs3 if variant == 3 => &specs::cs3_3,
		Game::Cs4 if variant == 0 => &specs::cs4,
		Game::Cs4 if variant == 1 => &specs::cs4_1,
		Game::Reverie if variant == 0 => &specs::reverie,
		Game::Reverie if variant == 1 => &specs::reverie_1,
		Game::Tx => &specs::tx,
		_ => panic!("Unsupported game or variant: {game:?}/{variant}"),
	}
}

#[allow(non_camel_case_types)]
#[derive(Debug, Clone, PartialEq, Eq, derive_more::FromStr)]
pub enum Part {
	U8,
	U16,
	U32,
	I8,
	I16,
	I32,
	F32,
	Str,

	Char,
	Item,
	Battle,
	Magic,
	Flag,
	Global,
	Var,
	FuncArg,
	NumReg,
	StrReg,
	Attr,
	CharAttr,
	Flags8,
	Flags16,
	Flags32,

	Sound,
	Music,

	Expr,
	Text,
	Dyn,
	Ndyn,
	Dyn_Char,
	Dyn_Sound,

	Cs1_36,
	Cs1_3C,

	Cs2_37,

	Tx_3C,
	Tx_isforceload,
	Tx_2F,

	Cs3_98,
	Cs3_c0,

	Cs4_40,
	Cs4_wtf_are_you_doing,

	Rev_3E,
	Rev_79,
	Rev_D2,
	Rev_E002,

	Print,
	Fail,
}

#[derive(Debug)]
pub struct Spec {
	pub ops: [Option<Op>; 256],
	pub by_name: BTreeMap<String, Opcode>,
}

#[derive(Debug, Clone, Default)]
pub struct Op {
	pub parts: Vec<Part>,
	pub name: String,
	child_keys: Vec<u8>,
	children: Vec<Op>,
}

impl Op {
	pub fn has_children(&self) -> bool {
		!self.child_keys.is_empty()
	}

	pub fn child(&self, key: u8) -> Option<&Op> {
		assert_eq!(self.child_keys.len(), self.children.len());
		if self.child_keys.is_empty() {
			return None;
		}
		let index = self.child_keys.binary_search(&key).ok()?;
		self.children.get(index)
	}
}

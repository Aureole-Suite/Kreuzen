use gospel::read::Le as _;
use gospel::write::{Le as _, Writer};
use rootcause::option_ext::OptionExt as _;

use crate::Game;
use crate::io::{CReader, OData};

#[derive(Debug, Clone, PartialEq)]
pub struct FieldMonster {
	pub a: u32,
	pub b: u16,
	pub c: u16,
	pub floats: Vec<f32>, // In Tx, floats[0] is between a and b
}

pub(crate) fn read(f: &mut CReader) -> rootcause::Result<FieldMonster> {
	let a = f.u32()?;
	let mut floats = Vec::new();
	if f.game == Game::Tx {
		floats.push(f.f32()?);
	}
	let b = f.u16()?;
	let c = f.u16()?;
	while !f.remaining().is_empty() {
		floats.push(f.f32()?);
	}
	Ok(FieldMonster { a, b, c, floats })
}

pub(crate) fn write(d: &OData, data: &FieldMonster) -> rootcause::Result<Writer> {
	let mut f = Writer::new();
	f.u32(data.a);
	let mut floats = data.floats.iter().copied();
	if d.game == Game::Tx {
		f.f32(floats.next().context("tx floats must be nonempty")?);
	}
	f.u16(data.b);
	f.u16(data.c);
	for v in floats {
		f.f32(v);
	}
	Ok(f)
}

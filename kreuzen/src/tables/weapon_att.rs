use gospel::read::Le as _;
use gospel::write::{Le as _, Writer};

use crate::io::{CReader, OData};

#[derive(Debug, Clone, PartialEq)]
pub struct WeaponAtt {
	pub slash: u8,
	pub thrust: u8,
	pub pierce: u8,
	pub strike: u8,
}

pub(crate) fn read(f: &mut CReader) -> rootcause::Result<WeaponAtt> {
	Ok(WeaponAtt {
		slash: f.u8()?,
		thrust: f.u8()?,
		pierce: f.u8()?,
		strike: f.u8()?,
	})
}

pub(crate) fn write(_d: &OData, table: &WeaponAtt) -> rootcause::Result<Writer> {
	let mut f = Writer::new();
	f.u8(table.slash);
	f.u8(table.thrust);
	f.u8(table.pierce);
	f.u8(table.strike);
	Ok(f)
}

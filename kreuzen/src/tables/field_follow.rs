use gospel::read::Le as _;
use gospel::write::{Le as _, Writer};

use crate::io::{CReader, OData};

#[derive(Debug, Clone, PartialEq)]
pub struct FieldFollow {
	pub a: f32,
	pub b: f32,
	pub c: f32,
	pub d: f32,
	pub e: f32,
}

pub(crate) fn read(f: &mut CReader) -> rootcause::Result<FieldFollow> {
	Ok(FieldFollow {
		a: f.f32()?,
		b: f.f32()?,
		c: f.f32()?,
		d: f.f32()?,
		e: f.f32()?,
	})
}

pub(crate) fn write(_d: &OData, data: &FieldFollow) -> rootcause::Result<Writer> {
	let mut f = Writer::new();
	f.f32(data.a);
	f.f32(data.b);
	f.f32(data.c);
	f.f32(data.d);
	f.f32(data.e);
	Ok(f)
}

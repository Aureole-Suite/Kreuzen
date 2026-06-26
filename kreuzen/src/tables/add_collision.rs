use gospel::read::Le as _;
use gospel::write::{Le as _, Writer};

use crate::io::{CReader, OData};

#[derive(Debug, Clone, PartialEq)]
pub struct Collision {
	pub a: u32,
	pub b: [f32; 5],
}

pub(crate) fn read(f: &mut CReader) -> rootcause::Result<Vec<Collision>> {
	let n = f.u8()? as usize;
	let mut out = Vec::with_capacity(n);
	for _ in 0..n {
		out.push(Collision {
			a: f.u32()?,
			b: [f.f32()?, f.f32()?, f.f32()?, f.f32()?, f.f32()?],
		});
	}
	Ok(out)
}

pub(crate) fn write(_d: &OData, table: &[Collision]) -> rootcause::Result<Writer> {
	let mut f = Writer::new();
	let n = u8::try_from(table.len())
		.map_err(|_| rootcause::report!("AddCollision too large: {}", table.len()))?;
	f.u8(n);
	for c in table {
		f.u32(c.a);
		for &v in &c.b {
			f.f32(v);
		}
	}
	Ok(f)
}

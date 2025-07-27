use super::*;

#[derive(Debug, snafu::Snafu)]
pub enum ReadError {
	#[snafu(display("invalid read (at {location})"), context(false))]
	Read {
		source: gospel::read::Error,
		#[snafu(implicit)]
		location: snafu::Location,
	},
}

#[derive(Debug, Clone, PartialEq)]
pub struct FieldMonsterData {
	pub a: u32,
	pub b: u16,
	pub c: u16,
	pub d: f32,
	pub e: f32,
	pub f: f32,
	pub g: f32,
	pub h: Option<f32>,
}

pub(crate) fn read(f: &mut VReader) -> Result<FieldMonsterData, ReadError> {
	Ok(FieldMonsterData {
		a: f.u32()?,
		b: f.u16()?,
		c: f.u16()?,
		d: f.f32()?,
		e: f.f32()?,
		f: f.f32()?,
		g: f.f32()?,
		h: f.f32().ok(),
	})
}

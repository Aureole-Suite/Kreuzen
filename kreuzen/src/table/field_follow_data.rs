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
pub struct FieldFollowData {
	pub a: f32,
	pub b: f32,
	pub c: f32,
	pub d: f32,
	pub e: f32,
}

pub(crate) fn read(f: &mut VReader) -> Result<FieldFollowData, ReadError> {
	Ok(FieldFollowData {
		a: f.f32()?,
		b: f.f32()?,
		c: f.f32()?,
		d: f.f32()?,
		e: f.f32()?,
	})
}

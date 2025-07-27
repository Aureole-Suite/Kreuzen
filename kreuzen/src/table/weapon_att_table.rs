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
pub struct WeaponAtt {
	pub slash: u8,
	pub thrust: u8,
	pub pierce: u8,
	pub strike: u8,
}

pub(crate) fn read(f: &mut VReader) -> Result<WeaponAtt, ReadError> {
	Ok(WeaponAtt {
		slash: f.u8()?,
		thrust: f.u8()?,
		pierce: f.u8()?,
		strike: f.u8()?,
	})
}

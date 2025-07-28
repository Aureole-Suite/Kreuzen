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

#[derive(Debug, snafu::Snafu)]
pub enum WriteError {
	#[snafu(transparent, context(false))]
	Value { source: crate::ValueError },
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

pub(crate) fn write(f: &mut VWriter, data: &WeaponAtt) -> Result<(), WriteError> {
	f.u8(data.slash);
	f.u8(data.thrust);
	f.u8(data.pierce);
	f.u8(data.strike);
	Ok(())
}

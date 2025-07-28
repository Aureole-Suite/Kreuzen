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

pub(crate) fn read(f: &mut VReader) -> Result<String, ReadError> {
	let s = f.str()?;
	Ok(s)
}

pub(crate) fn write(f: &mut VWriter, s: &str) -> Result<(), WriteError> {
	f.str(s)?;
	Ok(())
}

use super::*;

#[derive(Debug, snafu::Snafu)]
pub enum ReadError {
	#[snafu(display("invalid read (at {location})"), context(false))]
	Read {
		source: gospel::read::Error,
		#[snafu(implicit)]
		location: snafu::Location,
	},
	#[snafu(display("style name mismatch: {a:?} != {b:?}"))]
	StyleMismatch { a: String, b: String },
}

#[derive(Debug, snafu::Snafu)]
pub enum WriteError {
	#[snafu(transparent, context(false))]
	Value { source: crate::ValueError },
}

pub(crate) fn read(f: &mut VReader) -> Result<String, ReadError> {
	let a = f.sstr(64)?;
	let b = f.sstr(64)?;
	ensure!(a == b, StyleMismatchSnafu { a, b });
	Ok(a)
}

pub(crate) fn write(f: &mut VWriter, s: &str) -> Result<(), WriteError> {
	f.sstr(64, s)?;
	f.sstr(64, s)?;
	Ok(())
}

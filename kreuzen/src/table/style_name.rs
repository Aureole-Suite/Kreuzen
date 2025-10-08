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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StyleName(pub String, pub String);

pub(crate) fn read(f: &mut VReader) -> Result<StyleName, ReadError> {
	let a = f.sstr(64)?;
	let b = f.sstr(64)?;
	Ok(StyleName(a, b))
}

pub(crate) fn write(f: &mut VWriter, s: &StyleName) -> Result<(), WriteError> {
	f.sstr(64, &s.0)?;
	f.sstr(64, &s.1)?;
	Ok(())
}

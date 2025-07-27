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

pub(crate) fn read(f: &mut VReader) -> Result<String, ReadError> {
	let s = f.str()?;
	Ok(s)
}

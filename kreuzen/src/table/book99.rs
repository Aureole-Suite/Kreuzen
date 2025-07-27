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

pub(crate) fn read(f: &mut VReader) -> Result<u16, ReadError> {
	let n = f.u16()?;
	f.check_u16(1)?;
	Ok(n)
}

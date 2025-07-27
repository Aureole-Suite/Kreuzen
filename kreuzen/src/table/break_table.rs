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

pub(crate) fn read(f: &mut VReader) -> Result<Vec<(u16, u16)>, ReadError> {
	let mut table = Vec::new();
	loop {
		let id = f.u16()?;
		if id == 0xFFFF {
			break;
		}
		let value = f.u16()?;
		table.push((id, value));
	}
	f.check_u16(1)?;
	Ok(table)
}

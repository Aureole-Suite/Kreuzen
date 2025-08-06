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

pub(crate) fn read(f: &mut VReader) -> Result<Vec<(u16, u16)>, ReadError> {
	let mut table = Vec::new();
	let end = match f.game {
		crate::Game::Ed84 => (0, 0),
		crate::Game::Ed85 => (0xFFFF, 1),
	};
	loop {
		let id = f.u16()?;
		if id == end.0 {
			break;
		}
		let value = f.u16()?;
		table.push((id, value));
	}
	f.check_u16(end.1)?;
	Ok(table)
}

pub(crate) fn write(f: &mut VWriter, table: &[(u16, u16)]) -> Result<(), WriteError> {
	for &(id, value) in table {
		f.u16(id);
		f.u16(value);
	}
	f.u16(0xFFFF);
	f.u16(1);
	Ok(())
}

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
pub struct Part {
	pub id: u32,
	pub a: String,
	pub b: String,
}

pub(crate) fn read(f: &mut VReader) -> Result<Vec<Part>, ReadError> {
	let mut parts = Vec::new();
	loop {
		let id = f.u32()?;
		if id == 0xFFFF {
			break;
		}
		let a = f.sstr(32)?;
		let b = f.sstr(32)?;
		parts.push(Part { id, a, b });
	}
	f.check(&[0; 64])?;
	Ok(parts)
}

pub(crate) fn write(f: &mut VWriter, parts: &[Part]) -> Result<(), WriteError> {
	for part in parts {
		f.u32(part.id);
		f.sstr(32, &part.a)?;
		f.sstr(32, &part.b)?;
	}
	f.u32(0xFFFF);
	f.slice(&[0; 64]);
	Ok(())
}

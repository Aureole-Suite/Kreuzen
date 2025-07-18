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
pub struct Part {
	pub id: u32,
	pub a: String,
	pub b: String,
}

pub fn read(f: &mut VReader) -> Result<Vec<Part>, ReadError> {
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

use super::*;

#[derive(Debug, snafu::Snafu)]
pub enum ReadError {
	#[snafu(display("invalid read (at {location})"), context(false))]
	Read {
		source: gospel::read::Error,
		#[snafu(implicit)]
		location: snafu::Location,
	},
	#[snafu(display("bad summon kind: {kind:X} for {b}, {c:?}"))]
	BadKind { kind: u16, b: u16, c: String },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Summon {
	pub kind: u16,
	pub b: u16,
	pub c: String,
}

pub fn read(f: &mut VReader) -> Result<Vec<Summon>, ReadError> {
	let mut table = Vec::new();
	while !f.remaining().is_empty() {
		let kind = f.u16()?;
		if kind == 0xFFFF {
			break;
		}
		let b = f.u16()?;
		let c = f.sstr(32)?;
		if kind.count_ones() != 1 {
			return BadKindSnafu { kind, b, c }.fail();
		}
		let kind = kind.trailing_zeros() as u16;
		table.push(Summon { kind, b, c });
	}
	f.check(&[0; 34])?;
	Ok(table)
}


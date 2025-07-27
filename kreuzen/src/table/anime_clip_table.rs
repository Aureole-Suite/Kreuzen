use super::*;

#[derive(Debug, snafu::Snafu)]
pub enum ReadError {
	#[snafu(display("invalid read (at {location})"), context(false))]
	Read {
		source: gospel::read::Error,
		#[snafu(implicit)]
		location: snafu::Location,
	},
	#[snafu(display("bad anime clip kind: {kind:X} for {a:?}, {b:?}"))]
	BadKind { kind: u32, a: String, b: String },
}

#[derive(Debug, Clone, PartialEq)]
pub struct AnimeClip {
	pub kind: u32,
	pub a: String,
	pub b: String,
}

pub(crate) fn read(f: &mut VReader) -> Result<Vec<AnimeClip>, ReadError> {
	let mut table = Vec::new();
	loop {
		let kind = f.u32()?;
		if kind == 0 {
			break;
		}
		let a = f.sstr(32)?;
		let b = f.sstr(32)?;
		if kind.count_ones() != 1 {
			return BadKindSnafu { kind, a, b }.fail();
		}
		let kind = kind.trailing_zeros();
		table.push(AnimeClip { kind, a, b });
	}
	f.check_u16(0)?;
	Ok(table)
}

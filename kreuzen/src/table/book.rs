use super::*;

#[derive(Debug, snafu::Snafu)]
pub enum ReadError {
	#[snafu(display("invalid read (at {location})"), context(false))]
	Read {
		source: gospel::read::Error,
		#[snafu(implicit)]
		location: snafu::Location,
	},
	#[snafu(display("bad book kind: {kind:04X}"))]
	BadBook { kind: u16 },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Book {
	TitlePage(String, [u16; 10], String),
	Page(String),
}

pub(crate) fn read(f: &mut VReader) -> Result<Book, ReadError> {
	let b = match f.u16()? {
		0 => {
			let s = f.str()?;
			Book::Page(s)
		}
		1 => {
			f.check_u16(0)?;
			let title = f.sstr(16)?;
			let data = [f.u16()?, f.u16()?, f.u16()?, f.u16()?, f.u16()?, f.u16()?, f.u16()?, f.u16()?, f.u16()?, f.u16()?];
			let text = f.str()?;
			Book::TitlePage(title, data, text)
		}
		kind => return BadBookSnafu { kind }.fail(),
	};
	Ok(b)
}


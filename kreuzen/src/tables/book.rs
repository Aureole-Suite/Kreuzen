use gospel::read::Le as _;
use gospel::write::{Le as _, Writer};

use crate::io::{CReader, OData, WriterExt as _};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Book {
	/// `BookData<XX>_99` — book table-of-contents. Stores a single u16 (book count),
	/// followed by a `0x0001` marker.
	Header(u16),
	/// `BookData<XX>_<MM>` where the first short is `> 0`. Has a title and a fixed
	/// chunk of metadata, plus the actual page text.
	TitlePage {
		title: String,
		data: [u16; 10],
		text: String,
	},
	/// `BookData<XX>_<MM>` where the first short is `0`. Body text only, possibly empty.
	Page(String),
	Empty,
}

pub(crate) fn read(f: &mut CReader, name: &str) -> rootcause::Result<Book> {
	if name.ends_with("_99") {
		let n = f.u16()?;
		f.check_u16(1)?;
		return Ok(Book::Header(n));
	}

	Ok(match f.u16()? {
		0 if f.remaining().is_empty() => Book::Empty,
		0 => Book::Page(f.str()?),
		1 => {
			f.check_u16(0)?;
			let title = f.sstr(16)?;
			#[rustfmt::skip]
			let data = [
				f.u16()?, f.u16()?, f.u16()?, f.u16()?, f.u16()?,
				f.u16()?, f.u16()?, f.u16()?, f.u16()?, f.u16()?,
			];
			let text = f.str()?;
			Book::TitlePage { title, data, text }
		}
		n => rootcause::bail!("unexpected control {n} in BookData"),
	})
}

pub(crate) fn write(d: &OData, book: &Book) -> rootcause::Result<Writer> {
	let mut f = Writer::new();
	match book {
		Book::Header(n) => {
			f.u16(*n);
			f.u16(1);
		}
		Book::TitlePage { title, data, text } => {
			f.u16(1);
			f.u16(0);
			f.sstr(16, d.enc, title)?;
			for &v in data {
				f.u16(v);
			}
			f.str(d.enc, text)?;
		}
		Book::Page(text) => {
			f.u16(0);
			f.str(d.enc, text)?;
		}
		Book::Empty => {
			f.u16(0);
		}
	}
	Ok(f)
}

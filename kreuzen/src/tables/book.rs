use gospel::read::Le as _;
use gospel::write::{Le as _, Writer};

use crate::io::{CReader, OData, WriterExt as _};
use crate::text::Text;

#[derive(Debug, Clone, PartialEq)]
pub struct Book {
	pub pages: Vec<(Option<TitlePage>, Text)>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BookData {
	Header(u16),
	TitlePage(TitlePage, String),
	Page(String),
	Empty,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TitlePage {
	pub title: String,
	pub data: [u16; 10],
}

pub(crate) fn read(f: &mut CReader, name: &str) -> rootcause::Result<BookData> {
	if name.ends_with("_99") {
		let n = f.u16()?;
		f.check_u16(1)?;
		return Ok(BookData::Header(n));
	}

	Ok(match f.u16()? {
		0 if f.remaining().is_empty() => BookData::Empty,
		0 => BookData::Page(f.str()?),
		1 => {
			f.check_u16(0)?;
			let title = f.sstr(16)?;
			#[rustfmt::skip]
			let data = [
				f.u16()?, f.u16()?, f.u16()?, f.u16()?, f.u16()?,
				f.u16()?, f.u16()?, f.u16()?, f.u16()?, f.u16()?,
			];
			let text = f.str()?;
			BookData::TitlePage(TitlePage { title, data }, text)
		}
		n => rootcause::bail!("unexpected control {n} in BookData"),
	})
}

pub(crate) fn write(d: &OData, book: &BookData) -> rootcause::Result<Writer> {
	let mut f = Writer::new();
	match book {
		BookData::Header(n) => {
			f.u16(*n);
			f.u16(1);
		}
		BookData::TitlePage(title, text) => {
			f.u16(1);
			f.u16(0);
			f.sstr(16, d.enc, &title.title)?;
			for &v in &title.data {
				f.u16(v);
			}
			f.str(d.enc, text)?;
		}
		BookData::Page(text) => {
			f.u16(0);
			f.str(d.enc, text)?;
		}
		BookData::Empty => {
			f.u16(0);
		}
	}
	Ok(f)
}

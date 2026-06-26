use gospel::write::Writer;

use crate::io::{CReader, OData, WriterExt as _};

#[derive(Debug, Clone, PartialEq)]
pub struct StyleName(pub String, pub String);

pub(crate) fn read(f: &mut CReader) -> rootcause::Result<StyleName> {
	Ok(StyleName(f.sstr(64)?, f.sstr(64)?))
}

pub(crate) fn write(d: &OData, s: &StyleName) -> rootcause::Result<Writer> {
	let mut f = Writer::new();
	f.sstr(64, d.enc, &s.0)?;
	f.sstr(64, d.enc, &s.1)?;
	Ok(f)
}

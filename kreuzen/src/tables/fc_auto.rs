use gospel::write::Writer;

use crate::io::{CReader, OData, WriterExt as _};

pub(crate) fn read(f: &mut CReader) -> rootcause::Result<String> {
	f.str()
}

pub(crate) fn write(d: &OData, s: &str) -> rootcause::Result<Writer> {
	let mut f = Writer::new();
	f.str(d.enc, s)?;
	Ok(f)
}

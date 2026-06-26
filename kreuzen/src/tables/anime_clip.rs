use gospel::read::Le as _;
use gospel::write::{Le as _, Writer};

use crate::io::{CReader, OData, WriterExt as _};

#[derive(Debug, Clone, PartialEq)]
pub struct AnimeClip {
	pub kind: u32, // In all known files this is a power of two. But I'll keep it as-is for experimentation.
	pub a: String,
	pub b: String,
}

pub(crate) fn read(f: &mut CReader) -> rootcause::Result<Vec<AnimeClip>> {
	let mut table = Vec::new();
	loop {
		let kind = f.u32()?;
		if kind == 0 {
			f.check_u16(0)?;
			break;
		}
		let a = f.sstr(32)?;
		let b = f.sstr(32)?;
		table.push(AnimeClip { kind, a, b });
	}
	Ok(table)
}

pub(crate) fn write(d: &OData, table: &[AnimeClip]) -> rootcause::Result<Writer> {
	let mut f = Writer::new();
	for clip in table {
		f.u32(clip.kind);
		f.sstr(32, d.enc, &clip.a)?;
		f.sstr(32, d.enc, &clip.b)?;
	}
	f.u32(0);
	f.u16(0);
	Ok(f)
}

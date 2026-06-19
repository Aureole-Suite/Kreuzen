use std::borrow::Cow;

use crate::{Enc, Game};
use gospel::read::Reader;
use gospel::write::{Writer, Label};

fn encode(enc: Enc, s: &str) -> rootcause::Result<Vec<u8>> {
	match enc {
		Enc::Utf8 => Ok(s.as_bytes().to_vec()),
		Enc::Sjis => match falcom_sjis::encode(s) {
			Ok(bytes) => Ok(bytes),
			Err(pos) => rootcause::bail!("invalid Shift-JIS at byte {pos}: {s:?}"),
		}
	}
}

#[derive(Debug, derive_more::Deref, derive_more::DerefMut)]
pub struct VReader<'a> {
	pub game: Game,
	pub enc: Enc,
	#[deref]
	#[deref_mut]
	pub reader: Reader<'a>,
}

impl<'a> VReader<'a> {
	pub fn str(&mut self) -> rootcause::Result<String> {
		let cstr = self.cstr()?;
		let s = self.decode(cstr.to_bytes())?;
		Ok(s)
	}

	pub fn sstr(&mut self, s: usize) -> rootcause::Result<String> {
		let pos = self.pos();
		let str = self.slice(s)?;
		let len = str.iter().position(|&b| b == 0).unwrap_or(s);
		let cstr = &str[..len];
		let s = self.decode(cstr)?;
		if !str[len..].iter().all(|&b| b == 0) {
			rootcause::bail!("Nonzero padding on sized string at {pos:X}: {s:?}");
		}
		Ok(s)
	}

	pub fn decode(&self, bytes: &[u8]) -> rootcause::Result<String> {
		match self.enc {
			Enc::Utf8 => match String::from_utf8_lossy(bytes) {
				Cow::Borrowed(text) => Ok(text.to_owned()),
				Cow::Owned(e) => {
					if let Ok(mut s) = falcom_sjis::decode(bytes){
						tracing::warn!("Invalid UTF-8 in text, but valid Shift-JIS: {s:?}");
						s.insert(0, '\u{FFFD}');
						Ok(s)
					} else {
						rootcause::bail!("Invalid UTF-8 in text: {e:?}");
					}
				}
			}
			Enc::Sjis => match falcom_sjis::decode(bytes) {
				Ok(text) => Ok(text),
				Err(_) => rootcause::bail!("Invalid Shift-JIS in text: {e:?}", e = falcom_sjis::decode_lossy(bytes)),
			}
		}
	}

	pub fn rewind(&mut self) {
		self.reader.seek(self.reader.pos() - 1).ok();
	}
}

#[derive(Debug, derive_more::Deref, derive_more::DerefMut)]
pub struct CReader<'a, 'b> {
	#[deref]
	#[deref_mut]
	pub reader: &'b mut VReader<'a>,
	pub scena: &'b str,
	pub variant: u8,
}

pub struct OData {
	pub start: Label,
	pub game: Game,
	pub enc: Enc,
	pub variant: u8,
}

pub trait WriterExt {
	fn str(&mut self, enc: Enc, s: &str) -> rootcause::Result<()>;
	fn sstr(&mut self, len: usize, enc: Enc, s: &str) -> rootcause::Result<()>;
}

impl WriterExt for Writer {
	fn str(&mut self, enc: Enc, s: &str) -> rootcause::Result<()> {
		let bytes = encode(enc, s)?;
		if bytes.contains(&0) {
			rootcause::bail!("string contains NUL: {s:?}");
		}
		self.slice(&bytes);
		self.slice(&[0]);
		Ok(())
	}

	fn sstr(&mut self, len: usize, enc: Enc, s: &str) -> rootcause::Result<()> {
		let bytes = encode(enc, s)?;
		if bytes.contains(&0) {
			rootcause::bail!("string contains NUL: {s:?}");
		}
		if bytes.len() > len {
			rootcause::bail!("string too long for sstr({len}): {s:?} encodes to {} bytes", bytes.len());
		}
		self.slice(&bytes);
		self.slice(&vec![0; len - bytes.len()]);
		Ok(())
	}
}

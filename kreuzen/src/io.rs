use std::borrow::Cow;

use crate::{Enc, Game};
use gospel::read::Reader;

#[derive(Debug, derive_more::Deref, derive_more::DerefMut)]
pub struct VReader<'a> {
	pub game: Game,
	pub enc: Enc,
	#[deref]
	#[deref_mut]
	pub reader: Reader<'a>,
}

impl<'a> VReader<'a> {
	pub fn str(&mut self) -> eyre::Result<String> {
		let cstr = self.cstr()?;
		let s = self.decode(cstr.to_bytes())?;
		Ok(s)
	}

	pub fn sstr(&mut self, s: usize) -> eyre::Result<String> {
		let pos = self.pos();
		let str = self.slice(s)?;
		let len = str.iter().position(|&b| b == 0).unwrap_or(s);
		let cstr = &str[..len];
		let s = self.decode(cstr)?;
		if !str[len..].iter().all(|&b| b == 0) {
			eyre::bail!("Nonzero padding on sized string at {pos:X}: {s:?}");
		}
		Ok(s)
	}

	pub fn decode(&self, bytes: &[u8]) -> eyre::Result<String> {
		match self.enc {
			Enc::Utf8 => match String::from_utf8_lossy(bytes) {
				Cow::Borrowed(text) => Ok(text.to_owned()),
				Cow::Owned(e) => {
					eyre::bail!("Invalid UTF-8 in text: {e:?}");
				}
			}
			Enc::Sjis => match falcom_sjis::decode(bytes) {
				Ok(text) => Ok(text),
				Err(_) => eyre::bail!("Invalid Shift-JIS in text: {e:?}", e = falcom_sjis::decode_lossy(bytes)),
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
	pub oddness: u32,
	pub entry: &'b str,
	pub entry_start: usize,
}

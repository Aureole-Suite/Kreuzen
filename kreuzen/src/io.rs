use gospel::read::Reader;
use crate::{Enc, Game};

pub struct VReader<'a> {
	pub game: Game,
	pub enc: Enc,
	pub reader: Reader<'a>,
}

impl<'a> std::ops::Deref for VReader<'a> {
	type Target = Reader<'a>;

	fn deref(&self) -> &Self::Target {
		&self.reader
	}
}

impl<'a> std::ops::DerefMut for VReader<'a> {
	fn deref_mut(&mut self) -> &mut Self::Target {
		&mut self.reader
	}
}

impl<'a> VReader<'a> {
	pub fn str(&mut self) -> Result<String, gospel::read::Error> {
		let pos = self.pos();
		let cstr = self.cstr()?;
		let s = cstr.to_str().map_err(|e| gospel::read::Error::Other {
			pos,
			source: Box::new(e),
		})?;
		Ok(String::from(s))
	}

	pub fn sstr(&mut self, s: usize) -> Result<String, gospel::read::Error> {
		let pos = self.pos();
		let str = self.slice(s)?;
		let len = str.iter().position(|&b| b == 0).unwrap_or(s);
		let cstr = &str[..len];
		let s = std::str::from_utf8(cstr).map_err(|e| gospel::read::Error::Other {
			pos,
			source: Box::new(e),
		})?;
		if !str[len..].iter().all(|&b| b == 0) {
			return Err(gospel::read::Error::Other {
				pos,
				source: format!(
					"nonzero padding on sized string: {:?}",
					String::from_utf8_lossy(str)
				)
				.into(),
			});
		}
		Ok(String::from(s))
	}
}


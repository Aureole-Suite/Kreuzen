use gospel::read::{Le as _, Reader};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd)]
pub enum Game {
	Cs1,
	Cs2,
	Cs3,
	Cs4,
	Reverie,
	// I believe Tx should be supported too but I don't own it
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Enc {
	Sjis,
	Utf8,
}

pub struct Config {
	game: Game,
	enc: Enc,
}

impl Config {
	pub fn new(game: Game, enc: Enc) -> Self {
		Self { game, enc }
	}
}


#[extend::ext]
impl<'a> Reader<'a> {
	fn str(&mut self) -> Result<String, gospel::read::Error> {
		let pos = self.pos();
		let cstr = self.cstr()?;
		let s = cstr.to_str().map_err(|e| gospel::read::Error::Other {
			pos,
			source: Box::new(e),
		})?;
		Ok(String::from(s))
	}

	fn sstr(&mut self, s: usize) -> Result<String, gospel::read::Error> {
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
				source: format!("nonzero padding on sized string: {:?}", String::from_utf8_lossy(str)).into(),
			});
		}
		Ok(String::from(s))
	}
}

pub struct Scena {}

pub fn parse(config: &Config, bytes: &[u8]) -> eyre::Result<Scena> {
	let mut f = Reader::new(bytes);
	f.check_u32(0x20)?;
	f.check_u32(0x20)?;
	let table_top = f.u32()? as usize;
	let table_size = f.u32()? as usize;
	let function_name_table_top = f.u32()? as usize;
	let nfunc = f.u32()? as usize;
	let asm_end = f.u32()? as usize;
	eyre::ensure!(table_top + table_size == function_name_table_top);
	eyre::ensure!(table_size == nfunc * 4);

	f.check_u32(0xABCDEF00)?;
	let script_name = f.str()?;

	let pad = f.slice(table_top - f.pos())?;
	if config.game < Game::Cs4 {
		eyre::ensure!(pad.is_empty());
	}

	eyre::ensure!(f.pos() == table_top);
	let table = read_asm(&mut f, nfunc)?;
	eyre::ensure!(f.pos() == asm_end);

	let mut iter = table.iter().map(|e| e.1).chain([f.len()]);
	let first = iter.next().unwrap(); // chain ensures it's nonempty
	eyre::ensure!(first >= f.pos());
	let pad2 = f.slice(first - f.pos())?;

	for (&(ref name, start), end) in table.iter().zip(iter) {
		let _span = tracing::error_span!("chunk", name = name.as_str(), start=format_args!("{start:X}")).entered();
		eyre::ensure!(f.pos() == start);
		eyre::ensure!(start <= end && end <= f.len());
		let slice = f.slice(end - start)?;
	}
	eyre::ensure!(f.pos() == f.len());

	Ok(Scena {})
}

// This function corresponds to the /asm/ files. Cursed.
fn read_asm(f: &mut Reader, n: usize) -> eyre::Result<Vec<(String, usize)>> {
	let mut starts = Vec::with_capacity(n);
	for _ in 0..n {
		starts.push(f.u32()? as usize);
	}
	let mut lengths = Vec::with_capacity(n);
	for _ in 0..n {
		lengths.push(f.u16()? as usize);
	}
	let mut names = Vec::with_capacity(n);
	#[expect(clippy::needless_range_loop)]
	for i in 0..n {
		assert_eq!(f.pos(), lengths[i]);
		names.push(f.str()?);
	}
	let mut entries = Vec::with_capacity(n);
	for i in 0..n {
		entries.push((names[i].to_owned(), starts[i]));
	}
	Ok(entries)
}

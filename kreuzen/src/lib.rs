#![feature(if_let_guard, split_as_slice)]
use gospel::read::{Reader, Le as _};
use snafu::ResultExt as _;

pub mod func;
pub mod table;

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
		let mut iter = str.split(|b| *b == 0);
		let cstr = iter.next().unwrap();
		let s = std::str::from_utf8(cstr).map_err(|e| gospel::read::Error::Other {
			pos,
			source: Box::new(e),
		})?;
		if !iter.as_slice().iter().all(|&b| b == 0) {
			return Err(gospel::read::Error::Other {
				pos,
				source: format!("nonzero padding on sized string: {:?}", String::from_utf8_lossy(str)).into(),
			});
		}
		Ok(String::from(s))
	}
}

#[derive(Debug, snafu::Snafu)]
pub enum ReadError {
	#[snafu(display("invalid read (at {location})"), context(false))]
	Read {
		source: gospel::read::Error,
		#[snafu(implicit)]
		location: snafu::Location,
	},
	#[snafu(display("could not read function '{name}'"))]
	Function { name: String, source: func::FunctionError },
	#[snafu(display("could not read effect '{name}'"))]
	Effect { name: String, source: table::TableError<table::EffectError> },
	#[snafu(display("could not read fc '{name}'"))]
	Fc { name: String, source: gospel::read::Error },
	#[snafu(display("could not read book data '{name}'"))]
	BookData { name: String, source: table::BookError },
	#[snafu(display("could not read book metadata '{name}'"))]
	BookData99 { name: String, source: table::BookError },
}

#[derive(Clone)]
struct Entry {
	name: String,
	start: usize,
}

impl std::fmt::Debug for Entry {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}@{}", self.name, self.start)
	}
}

enum Type {
	Normal,
	Table, // TODO separate each table
	StyleName,
	AddCollision,
	Btlset,
	BookData,
	BookData99,
	FcAuto,
	Effect,
	Empty,
}

impl Type {
	fn from_name(name: &str) -> Self {
		if name.starts_with("_")
		&& !name.starts_with("_a0_")
		&& !name.starts_with("_a1_")
		&& !name.starts_with("_a2_")
		&& !name.starts_with("_Lambda") {
			Type::Effect
		} else if name.is_empty() {
			Type::Empty
		} else if name.starts_with("BTLSET") {
			Type::Btlset
		} else if name.starts_with("BookData") {
			if name.ends_with("_99") {
				Type::BookData99
			} else {
				Type::BookData
			}
		} else if name.starts_with("FC_auto") {
			Type::FcAuto
		} else if name == "AddCollision" {
			Type::AddCollision
		} else if name.starts_with("StyleName") {
			Type::StyleName
		} else if name.ends_with("Table") || name.ends_with("Data") {
			Type::Table
		} else {
			Type::Normal
		}
	}
}

// This function corresponds to the /asm/ files. Cursed.
fn read_table(f: &mut Reader, n: usize) -> Result<Vec<Entry>, ReadError> {
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
		entries.push(Entry {
			name: names[i].to_owned(),
			start: starts[i],
		});
	}
	Ok(entries)
}

#[derive(Debug, Clone, derive_more::Deref, derive_more::DerefMut)]
pub struct VReader<'a> {
	#[deref]
	#[deref_mut]
	reader: Reader<'a>,
	version: u32,
	name: &'a str,
	func: &'a str,
}

pub fn parse(data: &[u8]) -> Result<(), ReadError> {
	let mut f = Reader::new(data);
	f.check_u32(0x20)?;
	f.check_u32(0x20)?;
	let table_top = f.u32()? as usize;
	let table_size = f.u32()? as usize;
	let function_name_table_top = f.u32()? as usize;
	let nfunc = f.u32()? as usize;
	let asm_end = f.u32()? as usize;
	assert_eq!(table_top + table_size, function_name_table_top);
	assert_eq!(table_size, nfunc * 4);

	f.check_u32(0xABCDEF00)?;
	let name = f.str()?;

	// In most cases, there's an align 4 followed by `00 00 00 FF`. But not always.
	let mut version = 0;
	while f.pos() < table_top {
		f.align_zeroed(4)?;
		f.check_u32(0xFF000000)?;
		version += 1;
	};
	let _span = tracing::error_span!("file", name = name.as_str(), version).entered();

	if version == 0 {
		tracing::warn!("skipping version 0 file");
		return Ok(());
	}

	assert_eq!(f.pos(), table_top);
	let table = read_table(&mut f, nfunc)?;
	assert_eq!(f.pos(), asm_end);
	f.align_zeroed(4)?;

	let ends = table.iter().map(|e| e.start).skip(1).chain([f.len()]);
	for (entry, end) in table.iter().zip(ends) {
		let _span = tracing::error_span!("chunk", name = entry.name.as_str()).entered();
		let mut vr = VReader {
			reader: Reader::new(&data[..end]).at(entry.start)?,
			version,
			name: &name,
			func: &entry.name,
		};
		if let Err(e) = read_entry(&mut vr) {
			tracing::error!("{e}\n{:#X}", vr.dump().start(entry.start));
		}
	}

	Ok(())
}

fn read_entry(f: &mut VReader) -> Result<(), ReadError> {
	match Type::from_name(f.func) {
		Type::Normal => {
			func::read_func(f).context(FunctionSnafu { name: f.func })?;
		}
		Type::Table => {}
		Type::StyleName => {}
		Type::AddCollision => {}
		Type::Btlset => {}
		Type::BookData => {
			// println!("{}:{}", name, entry.name);
			table::read_book(f).context(BookDataSnafu { name: f.func })?;
		}
		Type::BookData99 => {
			table::read_book99(f).context(BookData99Snafu { name: f.func })?;
		}
		Type::FcAuto => {
			table::read_fc(f).context(FcSnafu { name: f.func })?;
		}
		Type::Effect => {
			table::read_effect(f).context(EffectSnafu { name: f.func })?;
		}
		Type::Empty => { }
	}
	Ok(())
}

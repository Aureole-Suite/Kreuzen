#![feature(if_let_guard)]
use gospel::read::{Reader, Le as _};
use snafu::ResultExt as _;

pub mod func;

#[extend::ext]
impl<'a> Reader<'a> {
	fn str(&mut self) -> Result<String, gospel::read::Error> {
		let pos = self.pos();
		let cstr = self.cstr()?;
		cstr.to_str().map_err(|e| gospel::read::Error::Other {
			pos,
			source: Box::new(e),
		}).map(String::from)
	}

	fn ccstr(&mut self) -> Result<&'a [u8], gospel::read::Error> {
		let pos = self.remaining().windows(2).position(|w| w == [0, 0]);
		if let Some(pos) = pos {
			let s = self.slice(pos)?;
			self.check_u16(0).expect("already checked");
			Ok(s)
		} else {
			Err(gospel::read::Error::Other {
				pos: self.pos(),
				source: "not found".into(),
			})
		}
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
	Function {
		name: String,
		source: func::FunctionError,
	},
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
	FcAuto,
	Underscore,
	Empty,
}

impl Type {
	fn from_name(name: &str) -> Self {
		if name.starts_with("_") && !name.starts_with("_a0_") && !name.starts_with("_Lambda") {
			Type::Underscore
		} else if name.is_empty() {
			Type::Empty
		} else if name.starts_with("BTLSET") {
			Type::Btlset
		} else if name.starts_with("BookData") {
			Type::BookData
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
		let vr = VReader {
			reader: f.at(entry.start)?,
			version,
		};
		match Type::from_name(&entry.name) {
			Type::Normal => {
				func::read_func(vr, end, &entry.name).context(FunctionSnafu { name: &entry.name })?;
			}
			_ => {}
		}
	}

	Ok(())
}

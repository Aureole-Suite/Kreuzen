#![feature(if_let_guard, split_as_slice)]
use gospel::read::{Reader, Le as _};
use snafu::{ensure, ResultExt as _};

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
	#[snafu(display("failed to read entry '{name}'"))]
	EntrySnafu {
		name: String,
		source: EntryError,
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
	StyleName,
	Btlset,
	BookData,
	BookData99,
	FcAuto,
	Effect,

	Empty,
	ActionTable,
	AddCollision,
	AlgoTable,
	AnimeClipTable,
	BreakTable,
	FieldFollowData,
	FieldMonsterData,
	PartTable,
	ReactionTable,
	SummonTable,
	WeaponAttTable,
}


impl Type {
	fn from_name(name: &str) -> Self {
		if name.starts_with("_a0_")
		|| name.starts_with("_a1_")
		|| name.starts_with("_a2_")
		|| name.starts_with("_Lambda") {
			Type::Normal
		} else if name.starts_with("_") {
			Type::Effect
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
		} else if name.starts_with("StyleName") {
			Type::StyleName
		} else {
			match name {
				"" => Type::Empty,
				"AddCollision" => Type::AddCollision,
				"ActionTable" => Type::ActionTable,
				"AlgoTable" => Type::AlgoTable,
				"AnimeClipTable" => Type::AnimeClipTable,
				"BreakTable" => Type::BreakTable,
				"FieldFollowData" => Type::FieldFollowData,
				"FieldMonsterData" => Type::FieldMonsterData,
				"PartTable" => Type::PartTable,
				"ReactionTable" => Type::ReactionTable,
				"SummonTable" => Type::SummonTable,
				"WeaponAttTable" => Type::WeaponAttTable,
				_ => Type::Normal, // Default to normal if no match
			}
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

	let mut items = Vec::new();
	let ends = table.iter().map(|e| e.start).skip(1).chain([f.len()]);
	for (entry, end) in table.iter().zip(ends) {
		let _span = tracing::error_span!("chunk", name = entry.name.as_str()).entered();
		let mut vr = VReader {
			reader: Reader::new(&data[..end]).at(entry.start)?,
			version,
			name: &name,
			func: &entry.name,
		};
		match read_entry(&mut vr) {
			Ok(v) => items.push((entry.name.clone(), v)),
			Err(e) => {
				tracing::error!("{e}\n{:#X}", vr.dump().start(entry.start));
				for e in std::iter::successors(Some(&e as &dyn std::error::Error), |e| e.source()) {
					tracing::error!("{e}");
				}
			}
		}
	}
	tracing::trace!("{:#?}", items);

	Ok(())
}

#[derive(Debug, snafu::Snafu)]
pub enum EntryError {
	#[snafu(display("no terminator"))]
	BadTerminator,
	#[snafu(display("{n} bytes of trailing data"))]
	TrailingData { n: usize },

	#[snafu(display("could not read function"))]
	Function { source: func::FunctionError },
	#[snafu(display("could not read effect"))]
	Effect { source: table::EffectError },
	#[snafu(display("could not read FcAuto"))]
	Fc { source: gospel::read::Error },
	#[snafu(display("could not read BookData"))]
	BookData { source: table::BookError },
	#[snafu(display("could not read book BookData99"))]
	BookData99 { source: table::BookError },
	#[snafu(display("could not read BTLSET"))]
	Btlset { source: table::BtlsetError },
	#[snafu(display("could not read StyleName"))]
	StyleName { source: table::StyleNameError },

	#[snafu(display("could not read ActionTable"))]
	ActionTable { source: gospel::read::Error },
	#[snafu(display("could not read AddCollision"))]
	AddCollision { source: gospel::read::Error },
	#[snafu(display("could not read AlgoTable"))]
	AlgoTable { source: gospel::read::Error },
	#[snafu(display("could not read AnimeClipTable"))]
	AnimeClipTable { source: table::AnimeClipError },
}


#[derive(Debug, Clone)]
pub enum Item {
	Func(()),
	Effect(Vec<table::Effect>),
	Fc(String),
	BookPage(table::Book),
	BookMetadata(u16),
	Btlset(table::Btlset),
	StyleName(String),

	ActionTable(Vec<table::Action>),
	AddCollision(Vec<table::Collision>),
	AlgoTable(Vec<table::Algo>),
	AnimeClipTable(Vec<table::AnimeClip>),

	Unknown,
}

fn read_entry(f: &mut VReader) -> Result<Item, EntryError> {
	let mut data = f.reader.data();
	while data.last() == Some(&0) {
		data = &data[..data.len() - 1]; // Remove trailing zeroes
	}
	ensure!(data.last() == Some(&1), BadTerminatorSnafu);
	data = &data[..data.len() - 1];
	ensure!(data.len() >= f.reader.pos(), BadTerminatorSnafu);
	f.reader = Reader::new(data).at(f.reader.pos()).unwrap();
	let item = match Type::from_name(f.func) {
		Type::Normal => Item::Func(func::read_func(f).context(FunctionSnafu)?),
		Type::Effect => Item::Effect(table::read_effect(f).context(EffectSnafu)?),
		Type::FcAuto => Item::Fc(table::read_fc(f).context(FcSnafu)?),
		Type::BookData => Item::BookPage(table::read_book(f).context(BookDataSnafu)?),
		Type::BookData99 => Item::BookMetadata(table::read_book99(f).context(BookData99Snafu)?),
		Type::Btlset => Item::Btlset(table::read_btlset(f).context(BtlsetSnafu)?),
		Type::StyleName => Item::StyleName(table::read_style_name(f).context(StyleNameSnafu)?),

		Type::Empty => return Ok(Item::Unknown),
		Type::ActionTable => Item::ActionTable(table::read_action_table(f).context(ActionTableSnafu)?),
		Type::AddCollision => Item::AddCollision(table::read_add_collision(f).context(AddCollisionSnafu)?),
		Type::AlgoTable => Item::AlgoTable(table::read_algo_table(f).context(AlgoTableSnafu)?),
		Type::AnimeClipTable => Item::AnimeClipTable(table::read_anime_clip_table(f).context(AnimeClipTableSnafu)?),
		Type::BreakTable => return Ok(Item::Unknown),
		Type::FieldFollowData => return Ok(Item::Unknown),
		Type::FieldMonsterData => return Ok(Item::Unknown),
		Type::PartTable => return Ok(Item::Unknown),
		Type::ReactionTable => return Ok(Item::Unknown),
		Type::SummonTable => return Ok(Item::Unknown),
		Type::WeaponAttTable => return Ok(Item::Unknown),
	};
	ensure!(f.remaining().is_empty(), TrailingDataSnafu { n: f.remaining().len() });
	Ok(item)
}

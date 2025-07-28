#![feature(let_chains, if_let_guard, split_as_slice)]
use gospel::read::{Reader, Le as _};
use gospel::write::{Writer, Label, Le as _};
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
pub enum ValueError {
	#[snafu(display("string contains null byte: {string:?}"))]
	NullByte { string: String },
	#[snafu(display("usize does not fit in target type: {value}"))]
	Usize { value: usize },
	#[snafu(display("string {string:?} exceeds maximum length {len}"))]
	SstrLen { string: String, len: usize },
}

#[extend::ext]
impl Writer {
	fn str(&mut self, string: &str) -> Result<(), ValueError> {
		snafu::ensure!(!string.chars().any(|c| c == '\0'), NullByteSnafu { string });
		self.slice(string.as_bytes());
		self.u8(0);
		Ok(())
	}

	fn sstr(&mut self, len: usize, string: &str) -> Result<(), ValueError> {
		snafu::ensure!(string.len() <= len, SstrLenSnafu { string, len });
		snafu::ensure!(!string.chars().any(|c| c == '\0'), NullByteSnafu { string });
		self.slice(string.as_bytes());
		for _ in string.len()..len {
			self.u8(0);
		}
		Ok(())
	}

	fn usize16(&mut self, value: usize) -> Result<(), ValueError> {
		if let Ok(v) = u16::try_from(value) {
			self.u16(v);
			Ok(())
		} else {
			Err(ValueError::Usize { value })
		}
	}

	fn usize32(&mut self, value: usize) -> Result<(), ValueError> {
		if let Ok(v) = u32::try_from(value) {
			self.u32(v);
			Ok(())
		} else {
			Err(ValueError::Usize { value })
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
	#[snafu(display("failed to read entry '{name}'"))]
	EntryRead {
		name: String,
		source: EntryReadError,
	},
	#[snafu(display("cannot handle version {version}"))]
	BadVersion { version: u32 },
}

#[derive(Debug, snafu::Snafu)]
pub enum WriteError {
	#[snafu(transparent, context(false))]
	Write { source: gospel::write::Error },
	#[snafu(transparent, context(false))]
	Value { source: ValueError },
	#[snafu(display("failed to write entry '{name}'"))]
	EntryWrite {
		name: String,
		source: EntryWriteError,
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
struct VReader<'a> {
	#[deref]
	#[deref_mut]
	reader: Reader<'a>,
	version: u32,
}

#[derive(derive_more::Deref, derive_more::DerefMut)]
struct VWriter {
	#[deref]
	#[deref_mut]
	writer: Writer,
	start: Label,
	version: u32,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Scena {
	pub name: String,
	pub version: u32,
	pub items: Vec<(String, Item)>,
}

pub fn read(data: &[u8]) -> Result<Scena, ReadError> {
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

	let mut version = 0;
	while f.pos() < table_top {
		f.align_zeroed(4)?;
		f.check_u32(0xFF000000)?;
		version += 1;
	};
	let _span = tracing::error_span!("read", name = name.as_str(), version).entered();

	if version != 1 && version != 2 {
		return Err(ReadError::BadVersion { version });
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
		};
		match read_entry(&mut vr, &entry.name) {
			Ok(v) => items.push((entry.name.clone(), v)),
			Err(e) => {
				tracing::error!("{e}\n{:#X}", vr.dump().start(entry.start));
				for e in std::iter::successors(Some(&e as &dyn std::error::Error), |e| e.source()) {
					tracing::error!("{e}");
				}
			}
		}
	}

	Ok(Scena { name, version, items })
}

pub fn write(scena: &Scena) -> Result<Vec<u8>, WriteError> {
	let _span = tracing::error_span!("write", name = scena.name.as_str(), version = scena.version).entered();

	let mut f = Writer::new();
	let start = f.here();
	f.u32(0x20);
	f.u32(0x20);
	let table_top = f.ptr32(start);
	f.usize32(scena.items.len() * 4)?;
	let function_name_table_top = f.ptr32(start);
	f.usize32(scena.items.len())?;
	let asm_end = f.ptr32(start);
	f.u32(0xABCDEF00);
	assert!(f.len() == 0x20);

	f.str(&scena.name)?;
	for _ in 0..scena.version {
		f.align(4);
		f.u32(0xFF000000);
	}

	let mut labels = Vec::with_capacity(scena.items.len());
	let mut starts = Writer::new();
	let mut name_starts = Writer::new();
	let mut names = Writer::new();
	for (name, _) in &scena.items {
		labels.push(starts.ptr32(start));
		name_starts.label16(start, names.here());
		names.str(name)?;
	}

	f.place(table_top);
	f += starts;
	f.place(function_name_table_top);
	f += name_starts;
	f += names;
	f.place(asm_end);

	assert_eq!(scena.items.len(), labels.len());
	for (label, (name, item)) in labels.into_iter().zip(&scena.items) {
		let _span = tracing::error_span!("chunk", name = name.as_str()).entered();
		let mut vw = VWriter {
			writer: Writer::new(),
			start,
			version: scena.version,
		};
		let align = write_entry(&mut vw, item).context(EntryWriteSnafu { name })?;
		f.align(align);
		f.place(label);
		f += vw.writer;
		f.u8(0x01);
	}

	Ok(f.finish()?)
}

#[derive(Debug, Clone, PartialEq)]
pub enum Item {
	Func(Vec<func::Stmt>),
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
	BreakTable(Vec<(u16, u16)>),
	FieldFollowData(table::FieldFollowData),
	FieldMonsterData(table::FieldMonsterData),
	PartTable(Vec<table::Part>),
	ReactionTable(Vec<table::Reaction>),
	SummonTable(Vec<table::Summon>),
	WeaponAttTable(table::WeaponAtt),

	Empty,
}

#[derive(Debug, snafu::Snafu)]
pub enum EntryReadError {
	#[snafu(display("no terminator"))]
	BadTerminator,
	#[snafu(display("{n} bytes of trailing data"))]
	TrailingData { n: usize },

	#[snafu(display("could not read function"), context(false))]
	Function { source: func::read::ReadError },
	#[snafu(display("could not read effect"), context(false))]
	Effect { source: table::effect::ReadError },
	#[snafu(display("could not read FcAuto"), context(false))]
	FcAuto { source: table::fc_auto::ReadError },
	#[snafu(display("could not read BookData"), context(false))]
	BookData { source: table::book::ReadError },
	#[snafu(display("could not read book BookData99"), context(false))]
	BookData99 { source: table::book99::ReadError },
	#[snafu(display("could not read BTLSET"), context(false))]
	Btlset { source: table::btlset::ReadError },
	#[snafu(display("could not read StyleName"), context(false))]
	StyleName { source: table::style_name::ReadError },

	#[snafu(display("could not read ActionTable"), context(false))]
	ActionTable { source: table::action_table::ReadError },
	#[snafu(display("could not read AddCollision"), context(false))]
	AddCollision { source: table::add_collision::ReadError },
	#[snafu(display("could not read AlgoTable"), context(false))]
	AlgoTable { source: table::algo_table::ReadError },
	#[snafu(display("could not read AnimeClipTable"), context(false))]
	AnimeClipTable { source: table::anime_clip_table::ReadError },
	#[snafu(display("could not read BreakTable"), context(false))]
	BreakTable { source: table::break_table::ReadError },
	#[snafu(display("could not read FieldFollowData"), context(false))]
	FieldFollowData { source: table::field_follow_data::ReadError },
	#[snafu(display("could not read FieldMonsterData"), context(false))]
	FieldMonsterData { source: table::field_monster_data::ReadError },
	#[snafu(display("could not read PartTable"), context(false))]
	PartTable { source: table::part_table::ReadError },
	#[snafu(display("could not read ReactionTable"), context(false))]
	ReactionTable { source: table::reaction_table::ReadError },
	#[snafu(display("could not read SummonTable"), context(false))]
	SummonTable { source: table::summon_table::ReadError },
	#[snafu(display("could not read WeaponAttTable"), context(false))]
	WeaponAttTable { source: table::weapon_att_table::ReadError },
}

fn read_entry(f: &mut VReader, name: &str) -> Result<Item, EntryReadError> {
	let mut data = f.reader.data();
	while data.last() == Some(&0) {
		data = &data[..data.len() - 1]; // Remove trailing zeroes
	}
	ensure!(data.last() == Some(&1), BadTerminatorSnafu);
	data = &data[..data.len() - 1];
	ensure!(data.len() >= f.reader.pos(), BadTerminatorSnafu);
	f.reader = Reader::new(data).at(f.reader.pos()).unwrap();

	let item = match Type::from_name(name) {
		Type::Normal => Item::Func(func::read::read(f)?),
		Type::Effect => Item::Effect(table::effect::read(f)?),
		Type::FcAuto => Item::Fc(table::fc_auto::read(f)?),
		Type::BookData => Item::BookPage(table::book::read(f)?),
		Type::BookData99 => Item::BookMetadata(table::book99::read(f)?),
		Type::Btlset => Item::Btlset(table::btlset::read(f)?),
		Type::StyleName => Item::StyleName(table::style_name::read(f)?),

		Type::Empty => {
			if !f.remaining().is_empty() {
				Item::Btlset(table::btlset::read(f)?)
			} else {
				Item::Empty
			}
		},
		Type::ActionTable => Item::ActionTable(table::action_table::read(f)?),
		Type::AddCollision => Item::AddCollision(table::add_collision::read(f)?),
		Type::AlgoTable => Item::AlgoTable(table::algo_table::read(f)?),
		Type::AnimeClipTable => Item::AnimeClipTable(table::anime_clip_table::read(f)?),
		Type::BreakTable => Item::BreakTable(table::break_table::read(f)?),
		Type::FieldFollowData => Item::FieldFollowData(table::field_follow_data::read(f)?),
		Type::FieldMonsterData => Item::FieldMonsterData(table::field_monster_data::read(f)?),
		Type::PartTable => Item::PartTable(table::part_table::read(f)?),
		Type::ReactionTable => Item::ReactionTable(table::reaction_table::read(f)?),
		Type::SummonTable => Item::SummonTable(table::summon_table::read(f)?),
		Type::WeaponAttTable => Item::WeaponAttTable(table::weapon_att_table::read(f)?),
	};

	ensure!(f.remaining().is_empty(), TrailingDataSnafu { n: f.remaining().len() });
	Ok(item)
}

#[derive(Debug, snafu::Snafu)]
pub enum EntryWriteError {
	#[snafu(display("could not write function"), context(false))]
	Function { source: func::write::WriteError },
	#[snafu(display("could not write effect"), context(false))]
	Effect { source: table::effect::WriteError },
	#[snafu(display("could not write FcAuto"), context(false))]
	FcAuto { source: table::fc_auto::WriteError },
	#[snafu(display("could not write BookData"), context(false))]
	BookData { source: table::book::WriteError },
	#[snafu(display("could not write book BookData99"), context(false))]
	BookData99 { source: table::book99::WriteError },
	#[snafu(display("could not write BTLSET"), context(false))]
	Btlset { source: table::btlset::WriteError },
	#[snafu(display("could not write StyleName"), context(false))]
	StyleName { source: table::style_name::WriteError },

	#[snafu(display("could not write ActionTable"), context(false))]
	ActionTable { source: table::action_table::WriteError },
	#[snafu(display("could not write AddCollision"), context(false))]
	AddCollision { source: table::add_collision::WriteError },
	#[snafu(display("could not write AlgoTable"), context(false))]
	AlgoTable { source: table::algo_table::WriteError },
	#[snafu(display("could not write AnimeClipTable"), context(false))]
	AnimeClipTable { source: table::anime_clip_table::WriteError },
	#[snafu(display("could not write BreakTable"), context(false))]
	BreakTable { source: table::break_table::WriteError },
	#[snafu(display("could not write FieldFollowData"), context(false))]
	FieldFollowData { source: table::field_follow_data::WriteError },
	#[snafu(display("could not write FieldMonsterData"), context(false))]
	FieldMonsterData { source: table::field_monster_data::WriteError },
	#[snafu(display("could not write PartTable"), context(false))]
	PartTable { source: table::part_table::WriteError },
	#[snafu(display("could not write ReactionTable"), context(false))]
	ReactionTable { source: table::reaction_table::WriteError },
	#[snafu(display("could not write SummonTable"), context(false))]
	SummonTable { source: table::summon_table::WriteError },
	#[snafu(display("could not write WeaponAttTable"), context(false))]
	WeaponAttTable { source: table::weapon_att_table::WriteError },
}

fn write_entry(f: &mut VWriter, item: &Item) -> Result<usize, EntryWriteError> {
	match item {
		Item::Func(i) => func::write::write(f, i)?,
		Item::Effect(i) => table::effect::write(f, i)?,
		Item::Fc(i) => table::fc_auto::write(f, i)?,
		Item::BookPage(i) => table::book::write(f, i)?,
		Item::BookMetadata(i) => table::book99::write(f, *i)?,
		Item::Btlset(i) => table::btlset::write(f, i)?,
		Item::StyleName(i) => table::style_name::write(f, i)?,

		Item::Empty => {}
		Item::ActionTable(i) => table::action_table::write(f, i)?,
		Item::AddCollision(i) => table::add_collision::write(f, i)?,
		Item::AlgoTable(i) => table::algo_table::write(f, i)?,
		Item::AnimeClipTable(i) => table::anime_clip_table::write(f, i)?,
		Item::BreakTable(i) => table::break_table::write(f, i)?,
		Item::FieldFollowData(i) => table::field_follow_data::write(f, i)?,
		Item::FieldMonsterData(i) => table::field_monster_data::write(f, i)?,
		Item::PartTable(i) => table::part_table::write(f, i)?,
		Item::ReactionTable(i) => table::reaction_table::write(f, i)?,
		Item::SummonTable(i) => table::summon_table::write(f, i)?,
		Item::WeaponAttTable(i) => table::weapon_att_table::write(f, i)?,
	}

	let align = match item {
		Item::Effect(_) => 16,
		Item::Fc(_) => 16,
		_ => 4,
	};
	Ok(align)
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Char(pub u16);
impl std::fmt::Debug for Char {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self.0 {
			0xF016 => write!(f, "field"),
			0xF028 => write!(f, "burst0"),
			0xF029 => write!(f, "burst1"),
			0xF02A => write!(f, "burst2"),
			0xF02B => write!(f, "burst3"),
			0xF02C => write!(f, "skit0"),
			0xF02D => write!(f, "skit1"),
			0xF093 => write!(f, "kisin0"),
			0xF094 => write!(f, "kisin1"),
			0xF095 => write!(f, "kisin2"),

			0xFD00 => write!(f, "battle0"),
			0xFD01 => write!(f, "battle1"),
			0xFD02 => write!(f, "battle2"),
			0xFD03 => write!(f, "battle3"),
			0xFD04 => write!(f, "battle4"),
			0xFD05 => write!(f, "battle5"),
			0xFD06 => write!(f, "battle6"),
			0xFD07 => write!(f, "battle7"),

			// FE12 and FE13 are special cased in op3E
			// FE02..=FE05 and FE15 are special cased in OP40

			0xFFFB => write!(f, "target"),
			0xFFFE => write!(f, "self"),
			0xFFFF => write!(f, "null"),

			c => write!(f, "ch{c}"),
		}
	}
}

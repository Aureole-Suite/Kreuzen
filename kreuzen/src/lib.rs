use gospel::read::{Le as _, Reader};
use gospel::write::{Label, Le as _, Writer};

use crate::code::FlatOp;
use crate::io::{CReader, WriterExt as _};

pub mod code;
pub mod decompile;
pub mod expr;
mod io;
pub mod spec;
pub mod sugar;
pub mod tables;
pub mod text;
pub mod types;

mod split;

macro_rules! ensure_ {
	($cond:expr) => {
		if !$cond {
			rootcause::bail!("{}", stringify!($cond));
		}
	};
	($cond:expr, $($arg:tt)*) => {
		if !$cond {
			rootcause::bail!($($arg)*);
		}
	};
	(let $pat:pat = $val:expr) => {
		let $pat = $val else {
			rootcause::bail!("{}", stringify!(let $pat = $cond));
		};
	};
	(let $pat:pat = $val:expr, $($arg:tt)*) => {
		let $pat = $val else {
			rootcause::bail!($($arg)*);
		};
	};
}
use ensure_ as ensure;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd)]
pub enum Game {
	Cs1,
	Cs2,
	Cs3,
	Cs4,
	Reverie,
	Tx,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Enc {
	Sjis,
	Utf8,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ScenaInfo {
	pub name: String,
	pub game: Game,
	pub enc: Enc,
	pub oddness: u8,
	pub variant: u8,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Scena {
	pub info: ScenaInfo,
	pub chunks: Vec<Chunk>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Chunk {
	Function(Function),
	Table(tables::Table),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
	pub name: String,
	pub body: Body,
	pub preload: Vec<code::preload::Preload>,
	pub shadow: Vec<code::shadow::Shadow>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Body {
	Flat(Vec<FlatOp>),
	Tree(Vec<decompile::Stmt>),
}

pub fn decompile(scena: &Scena) -> rootcause::Result<Scena> {
	let mut chunks = Vec::with_capacity(scena.chunks.len());
	let mut errors = rootcause::report_collection::ReportCollection::new();
	for c in &scena.chunks {
		match c {
			Chunk::Function(Function { name, body: Body::Flat(body), preload, shadow }) => {
				let _span = tracing::error_span!("decompile", %name).entered();
				let body = match crate::decompile::decompile(body) {
					Ok(b) => b,
					Err(e) => {
						errors.push(e.context(format!("error decompiling chunk {name}")).into_cloneable());
						continue;
					}
				};
				chunks.push(Chunk::Function(Function {
					name: name.clone(),
					body: Body::Tree(body),
					preload: preload.clone(),
					shadow: shadow.clone(),
				}))
			}
			c => chunks.push(c.clone()),
		}
	}
	if !errors.is_empty() {
		return Err(errors.context("error decompiling chunks").into());
	}
	Ok(Scena { info: scena.info.clone(), chunks })
}

pub fn compile(scena: &Scena) -> rootcause::Result<Scena> {
	let mut chunks = Vec::with_capacity(scena.chunks.len());
	let mut errors = rootcause::report_collection::ReportCollection::new();
	for c in &scena.chunks {
		match c {
			Chunk::Function(Function { name, body: Body::Tree(body), preload, shadow }) => {
				let _span = tracing::error_span!("compile", %name).entered();
				let body = match crate::decompile::compile(body) {
					Ok(b) => b,
					Err(e) => {
						errors.push(e.context(format!("error decompiling chunk {name}")).into_cloneable());
						continue;
					}
				};
				chunks.push(Chunk::Function(Function {
					name: name.clone(),
					body: Body::Flat(body),
					preload: preload.clone(),
					shadow: shadow.clone(),
				}))
			}
			c => chunks.push(c.clone()),
		}
	}
	if !errors.is_empty() {
		return Err(errors.context("error compiling chunks").into());
	}
	Ok(Scena { info: scena.info.clone(), chunks })
}

pub fn read(game: Game, enc: Enc, bytes: &[u8]) -> rootcause::Result<Scena> {
	let mut f = Reader::new(bytes);
	f.check_u32(0x20)?;
	let name_start = f.u32()? as usize;
	let table_top = f.u32()? as usize;
	let table_size = f.u32()? as usize;
	let function_name_table_top = f.u32()? as usize;
	let nfunc = f.u32()? as usize;
	let asm_end = f.u32()? as usize;
	crate::ensure!(table_top + table_size == function_name_table_top);
	crate::ensure!(table_size == nfunc * 4);

	f.check_u32(0xABCDEF00)?;
	let old_cs1 = name_start != 0x20;
	let script_name = f.at(name_start)?.cstr()?;
	if !script_name.to_bytes().iter().all(|b| (0x20..=0x7E).contains(b)) {
		rootcause::bail!("invalid name: {script_name:?}");
	}
	let name = std::str::from_utf8(script_name.to_bytes()).unwrap().to_owned();

	if !old_cs1 {
		f.cstr()?;
	};

	let (game, enc, variant) = resolve_game(&name, game, enc, old_cs1);

	let mut oddness = match game {
		Game::Cs4 if f.pos() == table_top => 1,
		Game::Cs4 => {
			f.align_zeroed(4)?;
			0
		}
		Game::Reverie if f.pos() == table_top => 1,
		Game::Reverie => {
			f.align_zeroed(4)?;
			if f.check_u32(0xFF000000).is_err() {
				tracing::warn!("unusual header, possibly from Decompiler2");
			}
			if f.check_u32(0xFF000000).is_ok() { 2 } else { 0 }
		}
		_ => 0,
	};

	crate::ensure!(f.pos() == table_top);
	let (names, starts) = read_asm(&mut f, nfunc)?;
	if old_cs1 {
		crate::ensure!(game == Game::Cs1);
		crate::ensure!(f.pos() == name_start);
		f.cstr()?;
	}
	crate::ensure!(f.pos() == asm_end);

	let mut iter = starts.iter().copied().chain([f.len()]);
	let first = iter.next().unwrap(); // chain ensures it's nonempty
	crate::ensure!(first >= f.pos());
	let pos = f.pos();
	let pad = f.slice(first - pos)?;
	crate::ensure!(pad.iter().all(|b| *b == 0));

	let mut f = CReader {
		game,
		enc,
		scena: &name,
		variant,
		outline_start: f.len(),
		reader: f,
	};

	let ranges = starts.iter().copied().zip(iter).collect::<Vec<_>>();
	let split = split::parse(&names);
	let mut chunks = Vec::with_capacity(split.entries.len());
	let mut errors = rootcause::report_collection::ReportCollection::new();
	for entry in split.entries {
		let _span = tracing::error_span!("entry", name=%entry.name).entered();
		match read_chunk(&mut f, &ranges, &entry) {
			Ok(chunk) => chunks.push(chunk),
			Err(e) => errors.push(e.context(format!("error parsing chunk {}", entry.name)).into_cloneable()),
		}
	}

	if let Some(i) = split.charater_section {
		crate::ensure!(game == Game::Reverie);
		crate::ensure!(oddness == 0);
		oddness = 3;
		match read_subchunk(&mut f, ranges[i], |_| Ok(())) {
			Ok(()) => {}
			Err(e) => errors.push(e.context("error parsing charater section".to_owned()).into_cloneable()),
		}
	}

	if !errors.is_empty() {
		return Err(errors.context("error parsing chunks").into());
	}

	Ok(Scena {
		info: ScenaInfo { name, game, enc, oddness, variant },
		chunks,
	})
}

pub fn write(scena: &Scena) -> rootcause::Result<Vec<u8>> {
	let start = Label::new();

	let mut errors = rootcause::report_collection::ReportCollection::new();
	let mut chunks = Vec::new();
	let mut chunk = |align: usize, name: &str, raw: bool, body: rootcause::Result<Writer>| match body {
		Ok(mut body) => {
			if !raw {
				body.u8(1);
			}
			chunks.push((Label::new(), align, name.to_owned(), body))
		}
		Err(e) => errors.push(e.context(format!("error writing chunk {}", name)).into_cloneable()),
	};

	let d = io::OData {
		start,
		game: scena.info.game,
		enc: scena.info.enc,
		variant: scena.info.variant,
	};

	for c in &scena.chunks {
		match c {
			Chunk::Function(function) => {
				let align = match (function.name.as_str(), scena.info.game) {
					("Init", Game::Cs1) if scena.info.name == "effect" => 16,
					_ => 4,
				};
				crate::ensure!(let Body::Flat(ops) = &function.body, "must compile before writing");
				chunk(align, &function.name, true, code::write(&d, ops));
			}
			Chunk::Table(t) => {
				let (align, f) = tables::write(&d, t)?;
				chunk(align, t.name(), false, Ok(f));
			}
		}
	}
	for c in &scena.chunks {
		if let Chunk::Function(function) = c
			&& !function.preload.is_empty()
		{
			let f = code::preload::write(&d, &function.preload);
			chunk(16, &format!("_{}", function.name), false, f);
		}
	}
	if scena.info.game == Game::Reverie && scena.info.oddness == 3 {
		chunk(4, "_a0_CharaterSection", false, Ok(Writer::new()));
	}
	for c in &scena.chunks {
		match c {
			Chunk::Function(function) => {
				for (i, shadow) in function.shadow.iter().enumerate() {
					let code = code::shadow::flatten(shadow);
					chunk(4, &format!("_a{i}_{}", function.name), true, code::write(&d, &code));
				}
			}
			Chunk::Table(t) => {
				if scena.info.game == Game::Reverie && tables_are_shadowed(&scena.info.name) {
					let empty = code::shadow::Shadow { line: 0, ops: vec![] };
					let code = code::shadow::flatten(&empty);
					chunk(4, &format!("_a0_{}", t.name()), true, code::write(&d, &code));
				}
			}
		}
	}

	if !errors.is_empty() {
		return Err(errors.context("error writing chunks").into());
	}

	let mut f = Writer::new();
	f.place(start);
	f.u32(0x20);
	let name_start = f.ptr32(start);
	let table_top = f.ptr32(start);
	f.u32(chunks.len() as u32 * 4);
	let function_name_table_top = f.ptr32(start);
	f.u32(chunks.len() as u32);
	let asm_end = f.ptr32(start);

	f.u32(0xABCDEF00);
	let old_cs1 = scena.info.game == Game::Cs1 && (1..100).contains(&scena.info.variant);
	if !old_cs1 {
		f.place(name_start);
		f.str(scena.info.enc, &scena.info.name)?;
	}

	match (scena.info.game, scena.info.oddness) {
		(Game::Cs4, 0) => f.align(4),
		(Game::Reverie, 0 | 3) => {
			f.align(4);
			f.u32(0xFF000000);
		}
		(Game::Reverie, 2) => {
			f.align(4);
			f.u32(0xFF000000);
			f.u32(0xFF000000);
		}
		_ => {}
	}

	{
		// write_asm
		f.place(table_top);
		for c in &chunks {
			f.label32(start, c.0);
		}
		f.place(function_name_table_top);
		let mut name_pos = Vec::with_capacity(chunks.len());
		for _ in &chunks {
			name_pos.push(f.ptr16(start));
		}
		for (c, l) in chunks.iter().zip(name_pos) {
			f.place(l);
			f.str(scena.info.enc, &c.2)?;
		}
	}

	if old_cs1 {
		f.place(name_start);
		f.str(scena.info.enc, &scena.info.name)?;
	}
	f.place(asm_end);

	for c in chunks {
		f.align(c.1);
		f.place(c.0);
		f += c.3;
	}

	Ok(f.finish()?)
}

fn tables_are_shadowed(name: &str) -> bool {
	const HASNT: &[&str] = &["alchr023", "alchr113_1", "alchr352_0", "alchr353_0", "alrob022_c10"];
	const HAS: &[&str] = &["synp05", "synp06"];
	(name.starts_with("al") && !HASNT.contains(&name)) || HAS.contains(&name)
}

fn resolve_game(n: &str, mut game: Game, mut enc: Enc, old_cs1: bool) -> (Game, Enc, u8) {
	let cs1_special = ["mon022_c00", "mon022_c01", "mon070_c00", "mon118_c00"];
	let cs2_special = ["e2230", "e4501", "e4701", "m5010"];
	let cs3_special_1 = ["mon037_c00", "mon042_c00", "mon042_c01", "mon046_c00", "ply000", "ply001"];
	let cs3_special_2 = ["mon_template", "chr_enemy_template"];
	let cs3_special_3 = ["mon000s", "rob013_c00"];
	let cs4_is_cs3 = [
		"mon027_c00",
		"mon093",
		"npcx00",
		"npcx02",
		"npcx03",
		"npcx04",
		"vehicle",
		"alchr034",
		"alchr034_0",
		"almon006_c03",
		"almon452_0",
		"almon452_1",
		"btl0922",
		"a0102",
		"a0104",
		"a0106",
		"a0108",
		"a2050",
		"tk_bike",
	];
	let cs4_special = ["rob030"];
	let rev_is_cs4 = [
		"mon027_c00",
		"mon093",
		"mon426",
		"npcx00",
		"npcx02",
		"npcx03",
		"npcx04",
		"rob030",
		"a0106",
		"chr970_c00",
	];
	let cs1_menu = [
		"battle_menu",
		"camp_menu",
		"camp_menu_v",
		"note_menu",
		"note_menu_v",
		"shop_menu",
		"shop_menu_v",
		"title_menu",
		"title_menu_v",
	];

	let cs1_sjis = ["almon146", "almon148_c00", "almon143_c00", "almon118", "almon046_c02"];

	let cs3_special = cs3_special_1.contains(&n) || cs3_special_2.contains(&n) || cs3_special_3.contains(&n);

	if game == Game::Cs1 && cs1_sjis.contains(&n) {
		enc = Enc::Sjis;
	}

	if game <= Game::Cs2 && n == "mon999"
		|| game == Game::Cs2 && n == "title"
		|| game == Game::Tx && n == "a1019"
		|| game == Game::Cs1 && n == "t0600"
	{
		enc = Enc::Sjis;
	}

	if game == Game::Cs2 && n == "t4720" {
		game = Game::Cs1
	}

	if game == Game::Reverie && (rev_is_cs4.contains(&n) || cs3_special) {
		game = Game::Cs4
	}

	if game == Game::Cs4 && (cs4_is_cs3.contains(&n) || cs3_special) {
		game = Game::Cs3
	}

	let variant = match game {
		Game::Cs1 if cs1_menu.contains(&n) => 100,
		Game::Cs1 if n == "npcx01" => 3,
		Game::Cs1 if cs1_special.contains(&n) => 2,
		Game::Cs1 if old_cs1 => 1,
		Game::Cs1 => 0,
		Game::Cs2 if cs1_menu.contains(&n) => 100,
		Game::Cs2 if cs2_special.contains(&n) => 1,
		Game::Cs2 => 0,
		Game::Tx => 0,
		Game::Cs3 if cs3_special_3.contains(&n) => 3,
		Game::Cs3 if cs3_special_2.contains(&n) => 2,
		Game::Cs3 if cs3_special_1.contains(&n) => 1,
		Game::Cs3 => 0,
		Game::Cs4 if cs4_special.contains(&n) => 1,
		Game::Cs4 => 0,
		Game::Reverie if n == "chr003_mg16" => 1,
		Game::Reverie => 0,
	};
	(game, enc, variant)
}

fn read_chunk(cr: &mut CReader, ranges: &[(usize, usize)], e: &split::Entry) -> rootcause::Result<Chunk> {
	let mut shadow = Vec::with_capacity(e.shadow.len());
	for (a, &s) in e.shadow.iter().enumerate() {
		let _span = tracing::error_span!("shadow", a).entered();
		let code = code::read_code_chunk(cr, ranges[s])?;
		shadow.push(code::shadow::parse(&code)?);
	}
	if let Some(table) = read_subchunk(cr, ranges[e.main], |f| tables::read(f, &e.name))? {
		let shadow = match shadow.as_slice() {
			[] => false,
			[s] if s.ops.is_empty() => true,
			_ => rootcause::bail!("unexpected shadows for table chunk {:?}", e.name),
		};
		if shadow && !(cr.game == Game::Reverie && tables_are_shadowed(cr.scena)) {
			tracing::warn!("expected table to not have shadow");
		}
		if !shadow && (cr.game == Game::Reverie && tables_are_shadowed(cr.scena)) {
			tracing::warn!("expected table to have shadow");
		}
		Ok(Chunk::Table(table))
	} else {
		let body = Body::Flat(code::read_code_chunk(cr, ranges[e.main])?);
		let preload = if let Some(i) = e.preload {
			let _span = tracing::error_span!("preload").entered();
			let v = read_subchunk(cr, ranges[i], code::preload::read)?;
			if v.is_empty() {
				tracing::warn!("preload is empty");
			}
			v
		} else {
			Vec::new()
		};
		Ok(Chunk::Function(Function { name: e.name.clone(), body, preload, shadow }))
	}
}

// This function corresponds to the /asm/ files. Cursed.
fn read_asm(f: &mut Reader, n: usize) -> rootcause::Result<(Vec<String>, Vec<usize>)> {
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

		let name = f.cstr()?;
		if !name.to_bytes().iter().all(|b| (0x20..=0x7E).contains(b)) {
			rootcause::bail!("invalid name: {name:?}");
		}

		names.push(std::str::from_utf8(name.to_bytes()).unwrap().to_owned());
	}
	Ok((names, starts))
}

fn read_subchunk<T>(f: &mut CReader, s: (usize, usize), body: impl FnOnce(&mut CReader) -> rootcause::Result<T>) -> rootcause::Result<T> {
	let (start, end) = s;
	let d = f.data();
	crate::ensure!(start <= end && end <= d.len());
	let mut actual_end = end;
	while actual_end > start && d[actual_end - 1] == 0 {
		actual_end -= 1;
	}
	if actual_end > start && d[actual_end - 1] == 1 {
		actual_end -= 1;
	} else {
		tracing::warn!("chunk does not end on 01");
	}
	let mut g = CReader {
		reader: Reader::new(&d[..actual_end]).at(start)?,
		..*f
	};
	let v = body(&mut g)?;
	if g.pos() != actual_end {
		tracing::warn!("Expected table to end at {actual_end:X} but ended at {:X}", g.pos());
	}
	f.seek(end)?;
	Ok(v)
}

use gospel::read::{Le as _, Reader};
mod io;
use io::VReader;

use crate::io::CReader;

mod code;
mod spec;
mod types;

mod split;

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

#[derive(Debug, Clone)]
pub struct Scena {
	pub name: String,
	pub oddness: u32,
	pub chunks: Vec<Chunk>,
	pub charater_section: Option<Code>,
}

#[derive(Debug, Clone)]
pub struct Chunk {
	pub name: String,
	pub func: CodeOrTable,
	pub preload: Option<Opaque>,
	pub shadow: Vec<Code>,
}

#[derive(Debug, Clone)]
pub enum CodeOrTable {
	Code(Code),
	Table(Opaque),
}

#[derive(Clone)]
pub struct Opaque {
	pub bytes: Vec<u8>,
}

#[derive(Debug, Clone)]
pub struct Code {
	pub ops: Vec<code::FlatOp>,
}

impl std::fmt::Debug for Opaque {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "[{} bytes]", self.bytes.len())
	}
}

pub fn parse(game: Game, enc: Enc, bytes: &[u8]) -> eyre::Result<Scena> {
	let mut f = VReader {
		game,
		enc,
		reader: Reader::new(bytes),
	};
	f.check_u32(0x20)?;
	let mut oddness = 0;
	let name_start = f.u32()? as usize;
	let table_top = f.u32()? as usize;
	let table_size = f.u32()? as usize;
	let function_name_table_top = f.u32()? as usize;
	let nfunc = f.u32()? as usize;
	let asm_end = f.u32()? as usize;
	eyre::ensure!(table_top + table_size == function_name_table_top);
	eyre::ensure!(table_size == nfunc * 4);

	f.check_u32(0xABCDEF00)?;
	let script_name = if name_start == 0x20 {
		f.str()?
	} else {
		oddness += 1;
		String::new()
	};

	match f.game {
		Game::Cs4 => {
			if f.pos() != table_top {
				f.align_zeroed(4)?;
			} else {
				oddness += 1;
			}
		}
		Game::Reverie => {
			if f.pos() != table_top {
				f.align_zeroed(4)?;
				f.check_u32(0xFF000000)?;
				if f.check_u32(0xFF000000).is_ok() {
					oddness += 2;
				}
			} else {
				oddness += 1;
			}
		}
		_ => {}
	}

	eyre::ensure!(f.pos() == table_top);
	let (names, starts) = read_asm(&mut f, nfunc)?;
	let script_name = if name_start == 0x20 {
		script_name
	} else {
		eyre::ensure!(f.game == Game::Cs1);
		eyre::ensure!(f.pos() == name_start);
		f.str()?
	};
	eyre::ensure!(f.pos() == asm_end);

	let mut iter = starts.iter().copied().chain([f.len()]);
	let first = iter.next().unwrap(); // chain ensures it's nonempty
	eyre::ensure!(first >= f.pos());
	if !names.is_empty() {
		f.align_zeroed(4)?;
	}
	let pos = f.pos();
	let pad = f.slice(first - pos)?;
	eyre::ensure!(pad.iter().all(|b| *b == 0));
	eyre::ensure!(pad.len().is_multiple_of(4));
	let pad = pad.len() / 4;
	if pad != 0 {
		eyre::ensure!(oddness == 0);
		eyre::ensure!(pad > 0);
		oddness += pad as u32;
	}
	
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

	let n = script_name.as_str();
	let cs3_special = cs3_special_1.contains(&n) || cs3_special_2.contains(&n) || cs3_special_3.contains(&n);

	if f.game <= Game::Cs2 && n == "mon999"
		|| f.game == Game::Cs2 && n == "title"
		|| f.game == Game::Tx && n == "a1019"
		|| f.game == Game::Cs1 && n == "t0600"
	{
		f.enc = Enc::Sjis;
	}

	if f.game == Game::Cs2 && n == "t4720" {
		f.game = Game::Cs1
	}

	if f.game == Game::Reverie && (rev_is_cs4.contains(&n) || cs3_special) {
		f.game = Game::Cs4
	}

	if f.game == Game::Cs4 && (cs4_is_cs3.contains(&n) || cs3_special) {
		f.game = Game::Cs3
	}

	let variant = match f.game {
		Game::Cs1 if cs1_menu.contains(&n) => 100,
		Game::Cs1 if cs1_special.contains(&n) => 3,
		Game::Cs1 if n == "npcx01" => 2,
		Game::Cs1 if oddness == 1 => 1,
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
		Game::Reverie if oddness == 2 => 1,
		_ => 0,
	};

	let tables = [
		"",
		"ActionTable",
		"AddCollision",
		"AlgoTable",
		"AnimeClipTable",
		"FieldMonsterData",
		"PartTable",
		"ReactionTable",
		"SummonTable",
		"ConditionTable",
		"BreakTable",
		"WeaponAttTable",
		"FieldFollowData",
		"ShinigPomBtlset",
	];

	let mut cr = CReader {
		reader: &mut f,
		scena: &script_name,
		variant,
		oddness,
	};

	let ranges = starts.iter().copied().zip(iter).collect::<Vec<_>>();
	let split = split::parse(&names);
	let mut chunks = Vec::with_capacity(split.entries.len());
	for e in split.entries {
		let _span = tracing::error_span!("entry", name=%e.name).entered();

		let is_table = tables.contains(&e.name.as_str())
			|| e.name.starts_with("FC_auto")
			|| e.name.starts_with("BookData")
			|| e.name.starts_with("BTLSET")
			|| e.name.starts_with("StyleName");
		let func = if !is_table {
			CodeOrTable::Code(decompile(&mut cr, ranges[e.main])?)
		} else {
			CodeOrTable::Table(read_table(&mut cr, ranges[e.main])?)
		};

		let preload = if let Some(i) = e.preload {
			let _span = tracing::error_span!("preload").entered();
			Some(read_table(&mut cr, ranges[i])?)
		} else {
			None
		};

		let mut shadow = Vec::with_capacity(e.shadow.len());
		for (a, &s) in e.shadow.iter().enumerate() {
			let _span = tracing::error_span!("shadow", a).entered();
			shadow.push(decompile(&mut cr, ranges[s])?);
		}

		chunks.push(Chunk {
			name: e.name,
			func,
			preload,
			shadow,
		});
	}

	let charater_section = if let Some(i) = split.charater_section {
		let _span = tracing::error_span!("charater section").entered();
		Some(decompile(&mut cr, ranges[i])?)
	} else {
		None
	};

	Ok(Scena {
		name: script_name,
		oddness,
		chunks,
		charater_section,
	})
}

// This function corresponds to the /asm/ files. Cursed.
fn read_asm(f: &mut VReader, n: usize) -> eyre::Result<(Vec<String>, Vec<usize>)> {
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
	Ok((names, starts))
}

fn decompile(f: &mut CReader, s: (usize, usize)) -> eyre::Result<Code> {
	let (start, end) = s;
	eyre::ensure!(start <= end && end <= f.reader.len());
	f.seek(start)?;
	let ops = code::decompile(f, end)?;
	Ok(Code { ops })
}

fn read_table(f: &mut CReader, s: (usize, usize)) -> eyre::Result<Opaque> {
	let (start, end) = s;
	eyre::ensure!(start <= end && end <= f.reader.len());
	f.seek(start)?;
	let bytes = f.reader.slice(end - start)?.to_vec();
	Ok(Opaque { bytes })
}

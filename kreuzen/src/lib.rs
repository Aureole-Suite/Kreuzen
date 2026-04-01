#![expect(unused)]
use gospel::read::{Le as _, Reader};
mod io;
use io::VReader;

use crate::io::CReader;

mod code;
mod spec;
mod types;

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

pub struct Scena {
	pub name: String,
	pub oddness: u32,
	pub chunks: Vec<(String, Vec<u8>)>,
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

	let mut chunks = Vec::with_capacity(names.len());

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
	if f.game == Game::Cs2 && n == "t4720" {
		f.game = Game::Cs1
	}

	if f.game == Game::Cs4 && (cs4_is_cs3.contains(&n) || cs3_special_1.contains(&n) || cs3_special_2.contains(&n) || cs3_special_3.contains(&n)) {
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
		_ => 0,
	};

	for (name, (start, end)) in names.into_iter().zip(starts.iter().copied().zip(iter)) {
		let _span =
			tracing::error_span!("chunk", %name, start = format_args!("{start:X}")).entered();
		eyre::ensure!(f.pos() == start);
		eyre::ensure!(start <= end && end <= f.len());
		let mut cr = CReader {
			reader: &mut f,
			scena: &script_name,
			variant,
			oddness,
			entry: &name,
			entry_start: start,
		};
		match read_entry(&mut cr, end) {
			Ok(it) => it,
			Err(e) => {
				tracing::error!("{e:#}");
				f.seek(end)?;
				continue;
			}
		};

	}
	eyre::ensure!(f.pos() == f.len());

	Ok(Scena {
		name: script_name,
		oddness,
		chunks,
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

fn read_entry(f: &mut CReader, end: usize) -> eyre::Result<()> {
	let prefixes = ["Ani", "TK_"];
	let fixed = ["Init", "PreInit", "Reinit"];
	let tables = ["AnimeClipTable"];
	let has_prefix = prefixes.iter().any(|p| f.entry.starts_with(p));
	let is_table = tables.contains(&f.entry);
	let is_fixed = fixed.contains(&f.entry);
	let is_weird = match f.game {
		Game::Reverie => matches!(f.scena,
			| "mon042_c00" | "mon042_c01" | "mon037_c00"
			| "mon000s" | "rob013_c00"
			| "ply000" | "ply001"
			| "mon_template" | "chr_enemy_template"),
		_ => false,
	};
	if (has_prefix || is_fixed) && !is_table && !is_weird {
		// These ones are very likely to be code, so we'll start with them
		let code = code::decompile(f, end)?;
		f.seek(end)?;
	} else {
		let pos = f.pos();
		let slice = f.slice(end - pos)?;
	}
	Ok(())
}

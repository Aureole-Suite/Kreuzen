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
	// I believe Tx should be supported too but I don't own it
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

	for (name, (start, end)) in names.into_iter().zip(starts.iter().copied().zip(iter)) {
		let _span =
			tracing::error_span!("chunk", %name, start = format_args!("{start:X}")).entered();
		eyre::ensure!(f.pos() == start);
		eyre::ensure!(start <= end && end <= f.len());
		let mut cr = CReader {
			reader: &mut f,
			scena: &script_name,
			oddness,
			entry: &name,
			entry_start: start,
		};
		match read_entry(&mut cr, end) {
			Ok(it) => it,
			Err(e) => {
				tracing::error!("{e:#}");
				let pos = f.pos();
				f.slice(end - pos)?;
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
	let is_menu = f.scena.contains("_menu");
	if (has_prefix || is_fixed) && !is_table && !is_menu {
		// These ones are very likely to be code, so we'll start with them
		let code = code::decompile(f, end)?;
		let pos = f.pos();
		let slice = f.slice(end - pos)?;
	} else {
		let pos = f.pos();
		let slice = f.slice(end - pos)?;
	}
	Ok(())
}

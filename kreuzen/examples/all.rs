use kreuzen::{Enc, Game};
use kreuzen_syntax::Print as _;
use std::cell::Cell;
use std::path::{Path, PathBuf};
use tracing::Level;
use tracing_subscriber::EnvFilter;
use tracing_subscriber::prelude::*;

thread_local! {
	static WARNED: Cell<bool> = const { Cell::new(false) };
}

struct WarnDetector;

impl<S: tracing::Subscriber> tracing_subscriber::Layer<S> for WarnDetector {
	fn on_event(&self, event: &tracing::Event<'_>, _ctx: tracing_subscriber::layer::Context<'_, S>) {
		if *event.metadata().level() <= Level::WARN {
			WARNED.with(|w| w.set(true));
		}
	}
}

fn with_warnings<T>(f: impl FnOnce() -> rootcause::Result<T>) -> rootcause::Result<(T, bool)> {
	WARNED.with(|w| w.set(false));
	let value = f()?;
	Ok((value, WARNED.with(|w| w.take())))
}

fn main() {
	unsafe { compact_debug::enable(true) };

	tracing_subscriber::registry()
		.with(tracing_subscriber::fmt::layer().with_filter(EnvFilter::from_default_env()))
		.with(WarnDetector)
		.init();

	let mut args = std::env::args().skip(1);
	let dir = PathBuf::from(args.next().expect("Usage: all <dir> [game...]"));
	let filters: Vec<String> = args.collect();

	let cs1 = dir.join("Trails of Cold Steel");
	let cs2 = dir.join("Trails of Cold Steel II");
	let cs3 = dir.join("The Legend of Heroes Trails of Cold Steel III");
	let cs4 = dir.join("The Legend of Heroes Trails of Cold Steel IV");
	let rev = dir.join("The Legend of Heroes Trails into Reverie");
	let tx = dir.join("Tokyo Xanadu eX+");

	if filters.is_empty() {
		game(Game::Cs1, Enc::Sjis, &cs1, "dat");
		game(Game::Cs1, Enc::Utf8, &cs1, "dat_us");
		game(Game::Cs2, Enc::Sjis, &cs2, "dat");
		game(Game::Cs2, Enc::Utf8, &cs2, "dat_us");
		game(Game::Tx, Enc::Utf8, &tx, "dat");
		game(Game::Cs3, Enc::Utf8, &cs3, "dat");
		game(Game::Cs3, Enc::Utf8, &cs3, "dat_en");
		game(Game::Cs3, Enc::Utf8, &cs3, "dat_fr");
		game(Game::Cs4, Enc::Utf8, &cs4, "dat");
		game(Game::Cs4, Enc::Utf8, &cs4, "dat_en");
		game(Game::Reverie, Enc::Utf8, &rev, "dat_en");
	} else {
		for filter in &filters {
			match filter.as_str() {
				"cs1" => game(Game::Cs1, Enc::Utf8, &cs1, "dat_us"),
				"cs2" => game(Game::Cs2, Enc::Utf8, &cs2, "dat_us"),
				"tx" => game(Game::Tx, Enc::Utf8, &tx, "dat"),
				"cs3" => game(Game::Cs3, Enc::Utf8, &cs3, "dat_en"),
				"cs4" => game(Game::Cs4, Enc::Utf8, &cs4, "dat_en"),
				"rev" => game(Game::Reverie, Enc::Utf8, &rev, "dat_en"),
				_ => eprintln!("Unknown game: {filter}"),
			}
		}
	}
}

fn ls(path: impl AsRef<Path>) -> Vec<String> {
	let mut files = match std::fs::read_dir(path.as_ref()) {
		Ok(read_dir) => read_dir
			.filter_map(|entry| entry.ok())
			.filter_map(|entry| entry.file_name().into_string().ok())
			.collect(),
		Err(_) => vec![],
	};
	files.sort();
	files
}

fn game(game: Game, enc: Enc, path: &Path, folder: &str) {
	let path = path.join("data/scripts");
	for dir in ls(&path) {
		for file in ls(path.join(&dir).join(folder)) {
			if game == Game::Tx && file == "magic.dat" {
				// This file is just garbage data
				continue;
			}
			let script = path.join(&dir).join(folder).join(&file);
			let scriptname = format!("{game:?}/{dir}/{folder}/{file}");
			let basename = file.strip_suffix(".dat").unwrap_or(&file);
			let outfile = PathBuf::from("out").join(format!("{game:?}/{folder}/{dir}/{basename}.krz"));
			let _span = tracing::error_span!("script", name = %scriptname).entered();
			match process(game, enc, &script, &outfile) {
				Ok(()) => {}
				Err(e) => {
					println!("Error processing {scriptname}: {e}");
				}
			}
		}
	}
}

fn process(game: Game, enc: Enc, script: &Path, outfile: &Path) -> rootcause::Result<()> {
	let bytes = std::fs::read(script)?;
	let (raw, warned) = with_warnings(|| kreuzen::read(game, enc, &bytes))?;
	let bytes2 = kreuzen::write(&raw)?;
	if bytes != bytes2 {
		let raw2 = kreuzen::read(game, enc, &bytes2)?;

		let s1 = raw.print_to_string();
		let s2 = raw2.print_to_string();
		if s1 != s2 {
			tracing::error!("decoded mismatch after roundtrip");
			print!("{}", pretty_assertions::StrComparison::new(&s1, &s2));
		} else if !warned {
			tracing::error!("bytes differ ({} -> {} bytes)", bytes.len(), bytes2.len());
		}
	} else if warned {
		tracing::warn!("warnings emitted but bytes are identical");
	}

	let (scena, warned) = with_warnings(|| kreuzen::decompile(&raw))?;
	let raw2 = kreuzen::compile(&scena)?;
	if raw2 != raw && !warned {
		let s1 = raw.print_to_string();
		let s2 = raw2.print_to_string();
		tracing::error!("decompile mismatch after roundtrip");
		if s1 == s2 {
			println!("string was equal, so probably NaN issues");
		} else {
			print!("{}", pretty_assertions::StrComparison::new(&s1, &s2));
		}
	}

	let (resugar, warned) = with_warnings(|| kreuzen::sugar::resugar(&scena))?;
	let desugar = kreuzen::sugar::desugar(&resugar)?;
	if desugar != scena && !warned {
		let s1 = scena.print_to_string();
		let s2 = desugar.print_to_string();
		tracing::error!("decompile mismatch after resugar");
		if s1 == s2 {
			println!("string was equal, so probably NaN issues");
		} else {
			print!("{}", pretty_assertions::StrComparison::new(&s1, &s2));
		}
	}
	let desugar2 = kreuzen::sugar::desugar(&desugar)?;
	if desugar2 != desugar {
		let s1 = desugar.print_to_string();
		let s2 = desugar2.print_to_string();
		tracing::error!("desugar is not idempotent");
		if s1 == s2 {
			println!("string was equal, so probably NaN issues");
		} else {
			print!("{}", pretty_assertions::StrComparison::new(&s1, &s2));
		}
	}

	if scena.info.game != game || scena.info.enc != enc {
		return Ok(());
	}

	let s1 = resugar.print_to_string();
	std::fs::create_dir_all(outfile.parent().unwrap())?;
	std::fs::write(outfile, s1)?;

	Ok(())
}

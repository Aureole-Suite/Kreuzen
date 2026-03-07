use eyre_span::Emit;
use kreuzen::{Enc, Game};
use std::path::{Path, PathBuf};

fn main() {
	unsafe { compact_debug::enable(true) };
	eyre_span::install().unwrap();

	tracing_subscriber::fmt::init();

	let dir = PathBuf::from(std::env::args().nth(1).expect("Usage: all <dir>"));
	let cs1 = dir.join("Trails of Cold Steel");
	let cs2 = dir.join("Trails of Cold Steel II");
	let cs3 = dir.join("The Legend of Heroes Trails of Cold Steel III");
	let cs4 = dir.join("The Legend of Heroes Trails of Cold Steel IV");
	let rev = dir.join("The Legend of Heroes Trails into Reverie");

	game(Game::Cs1, Enc::Sjis, &cs1, "dat");
	game(Game::Cs1, Enc::Utf8, &cs1, "dat_us");
	game(Game::Cs2, Enc::Sjis, &cs2, "dat");
	game(Game::Cs2, Enc::Utf8, &cs2, "dat_us");
	game(Game::Cs3, Enc::Utf8, &cs3, "dat");
	game(Game::Cs3, Enc::Utf8, &cs3, "dat_en");
	game(Game::Cs3, Enc::Utf8, &cs3, "dat_fr");
	game(Game::Cs4, Enc::Utf8, &cs4, "dat");
	game(Game::Cs4, Enc::Utf8, &cs4, "dat_en");
	game(Game::Reverie, Enc::Utf8, &rev, "dat_en");
}

fn ls(path: impl AsRef<Path>) -> Vec<String> {
	match std::fs::read_dir(path.as_ref()) {
		Ok(read_dir) => read_dir
			.filter_map(|entry| entry.ok())
			.filter_map(|entry| entry.file_name().into_string().ok())
			.collect(),
		Err(e) => {
			eprintln!("Error reading directory {}: {e}", path.as_ref().display());
			vec![]
		}
	}
}

fn game(game: Game, enc: Enc, path: &Path, folder: &str) {
	let path = path.join("data/scripts");
	for dir in ls(&path) {
		for file in ls(path.join(&dir).join(folder)) {
			let script = path.join(&dir).join(folder).join(&file);
			let scriptname = format!("{game:?}/{dir}/{folder}/{file}");
			let _span = tracing::error_span!("script", name = %scriptname).entered();
			process(game, enc, &script).emit();
		}
	}
}

fn process(game: Game, enc: Enc, script: &Path) -> eyre::Result<()> {
	let src = std::fs::read(script)?;
	tracing::info!("Processing {}", script.display());
	Ok(())
}

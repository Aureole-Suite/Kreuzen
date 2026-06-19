use std::fmt::Write;
use kreuzen::{Enc, Game};
use std::path::{Path, PathBuf};

fn main() {
	unsafe { compact_debug::enable(true) };

	tracing_subscriber::fmt::init();

	let dir = PathBuf::from(std::env::args().nth(1).expect("Usage: all <dir>"));
	let cs1 = dir.join("Trails of Cold Steel");
	let cs2 = dir.join("Trails of Cold Steel II");
	let cs3 = dir.join("The Legend of Heroes Trails of Cold Steel III");
	let cs4 = dir.join("The Legend of Heroes Trails of Cold Steel IV");
	let rev = dir.join("The Legend of Heroes Trails into Reverie");
	let tx = dir.join("Tokyo Xanadu eX+");

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
			let script = path.join(&dir).join(folder).join(&file);
			let scriptname = format!("{game:?}/{dir}/{folder}/{file}");
			let outfile = PathBuf::from("out").join(format!("{game:?}/{folder}/{dir}/{file}"));
			let _span = tracing::error_span!("script", name = %scriptname).entered();
			match process(game, enc, &script) {
				Ok(s) => {
					std::fs::create_dir_all(outfile.parent().unwrap()).unwrap();
					std::fs::write(outfile, s).unwrap();
				}
				Err(e) => {
					println!("Error processing {scriptname}: {e}");
				}
			}
		}
	}
}

fn process(game: Game, enc: Enc, script: &Path) -> rootcause::Result<String> {
	let bytes = std::fs::read(script)?;
	let scena = kreuzen::parse(game, enc, &bytes)?;
	let mut s = format!("scena {} game={:?} enc={:?} oddness={} variant={}\n", scena.name, scena.game, scena.enc, scena.oddness, scena.variant);
	for chunk in &scena.chunks {
		let _span = tracing::error_span!("chunk", name=%chunk.name).entered();
		s.push('\n');
		write!(s, "{} ", chunk.name)?;
		match &chunk.func {
			kreuzen::CodeOrTable::Code(code) => {
				write_dec(&mut s, code)?;
			}
			kreuzen::CodeOrTable::Table(table) => {
				writeln!(s, "{table:#?}")?;
			}
		}
		if !chunk.preload.is_empty() {
			writeln!(s, "_{}={:#?}", chunk.name, chunk.preload)?;
		}
		for (a, shadow) in chunk.shadow.iter().enumerate() {
			write!(s, "_a{a}_{}", chunk.name)?;
			write_dec(&mut s, shadow)?;
		}
	}

	// check_preload(&scena);

	Ok(s)
}

fn check_preload(scena: &kreuzen::Scena) {
	let has_preload = scena.chunks.iter()
		.filter(|c| match &c.func {
			kreuzen::CodeOrTable::Code(code) => !kreuzen::tables::preload::from_code(&code.ops, &c.name, &[]).is_empty(),
			_ => false,
		})
	.map(|x| x.name.as_str())
		.collect::<Vec<_>>();
	for chunk in &scena.chunks {
		let _span = tracing::error_span!("chunk", name=%chunk.name).entered();
		let kreuzen::CodeOrTable::Code(code) = &chunk.func else {
			if !chunk.preload.is_empty() {
				tracing::error!("chunk {} has a preload but is not code", chunk.name);
			}
			continue;
		};
		let preload2 = kreuzen::tables::preload::from_code(&code.ops, &chunk.name, &has_preload);
		if preload2 != chunk.preload {
			let diff = pretty_assertions::Comparison::new(&preload2, &chunk.preload);
			tracing::error!("preload mismatch {:?} {} {}", scena.game, scena.variant, scena.oddness);
			println!("{}", diff);
		}
	}
}

fn write_dec(s: &mut String, code: &kreuzen::Code) -> rootcause::Result<()> {
	match kreuzen::decompile::decompile(&code.ops) {
		Ok(stmts) => writeln!(s, "{:#?}", stmts)?,
		Err(e) => {
			write!(s, "Error decompiling:{e}")?;
			for (i, op) in code.ops.iter().enumerate() {
				writeln!(s, "{i}: {op:?}")?;
			}
			print!("Error decompiling:{e}"); // has a newline on its own
		}
	}
	Ok(())
}

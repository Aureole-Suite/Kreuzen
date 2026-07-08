use kreuzen::code::FlatOp;
use kreuzen::code::preload::Preload;
use kreuzen::{Body, Enc, Game};
use kreuzen_syntax::{Ctx, Print as _};
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
			let outfile = PathBuf::from("out").join(format!("{game:?}/{folder}/{dir}/{file}"));
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
	WARNED.with(|w| w.set(false));
	let scena = kreuzen::read(game, enc, &bytes)?;
	let had_warnings = WARNED.with(|w| w.take());
	if scena.game != game || scena.enc != enc {
		tracing::warn!(
			"resolved as {:?}/{:?}, expected {game:?}/{enc:?} — skipping output",
			scena.game,
			scena.enc
		);
		return Ok(());
	}
	let bytes2 = kreuzen::write(&scena)?;
	let s1 = to_string(&scena);
	if bytes != bytes2 {
		let scena2 = kreuzen::read(game, enc, &bytes2)?;

		let s2 = to_string(&scena2);
		if s1 != s2 {
			tracing::error!("decoded mismatch after roundtrip");
			print!("{}", pretty_assertions::StrComparison::new(&s1, &s2));
		} else if !had_warnings {
			tracing::error!("bytes differ ({} -> {} bytes)", bytes.len(), bytes2.len());
		}
	} else if had_warnings {
		tracing::warn!("warnings emitted but bytes are identical");
	}

	std::fs::create_dir_all(outfile.parent().unwrap())?;
	std::fs::write(outfile, s1)?;

	for c in &scena.chunks {
		if let Body::Code(code) = &c.func {
			check_decompile(&c.name, code);
		}
	}

	// check_preload(&scena);

	Ok(())
}

fn check_decompile(name: &str, code: &kreuzen::code::Code) {
	match kreuzen::decompile::decompile(code) {
		Ok(stmts) => {
			let v = kreuzen::decompile::compile(&stmts).unwrap();
			if v != *code {
				let diff = pretty_assertions::Comparison::new(&v, code);
				tracing::error!("recompile mismatch in {name}");
				println!("{}", diff);
			}
		}
		Err(e) => tracing::error!("Error decompiling {name}:{e}"),
	}
}

fn to_string(scena: &kreuzen::Scena) -> String {
	let mut ctx = Ctx::new();
	let s = format!(
		"scena {} game={:?} enc={:?} oddness={} variant={}",
		scena.name, scena.game, scena.enc, scena.oddness, scena.variant
	);
	ctx.token(s);
	ctx.newline(1);

	for chunk in &scena.chunks {
		let _span = tracing::error_span!("chunk", name=%chunk.name).entered();
		ctx.token(chunk.name.to_owned());
		match &chunk.func {
			Body::Code(code) => {
				match kreuzen::decompile::decompile(code) {
					Ok(stmts) => stmts.print(&mut ctx),
					Err(e) => {
						ctx.block_commented(&format!("Error decompiling:{e}"), &code.ops, FlatOp::print);
						print!("Error decompiling:{e}"); // has a newline on its own
					}
				}
			}
			Body::Table(table) => table.print(&mut ctx),
		}
		if !chunk.preload.is_empty() {
			ctx.word("preload");
			ctx.block(&chunk.preload, Preload::print);
		}
		for (a, shadow) in chunk.shadow.iter().enumerate() {
			ctx.token(format!("_a{a}_{}", chunk.name));
			shadow.print(&mut ctx);
		}
		ctx.newline(1);
	}
	ctx.finish()
}

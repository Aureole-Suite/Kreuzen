use std::path::{Path, PathBuf};
use std::process::ExitCode;

use clap::Parser;
use kreuzen::{Enc, Game};
use kreuzen_syntax::{Print as _, diag};
use rootcause::prelude::ResultExt as _;
use tracing_subscriber::prelude::*;
use walkdir::WalkDir;

#[derive(clap::ValueEnum, Clone, Copy)]
enum GameArg {
	Cs1,
	Cs2,
	Cs3,
	Cs4,
	Rev,
	Tx,
}

impl From<GameArg> for Game {
	fn from(g: GameArg) -> Self {
		match g {
			GameArg::Cs1 => Game::Cs1,
			GameArg::Cs2 => Game::Cs2,
			GameArg::Cs3 => Game::Cs3,
			GameArg::Cs4 => Game::Cs4,
			GameArg::Rev => Game::Reverie,
			GameArg::Tx => Game::Tx,
		}
	}
}

#[derive(clap::ValueEnum, Clone, Copy)]
enum EncArg {
	Utf8,
	Sjis,
}

impl From<EncArg> for Enc {
	fn from(e: EncArg) -> Self {
		match e {
			EncArg::Utf8 => Enc::Utf8,
			EncArg::Sjis => Enc::Sjis,
		}
	}
}

#[derive(clap::ValueEnum, Clone, Copy)]
enum DecompileMode {
	Flat,
	Tree,
	Sugar,
}

#[derive(clap::Parser)]
struct Args {
	files: Vec<PathBuf>,

	#[clap(long, help = "Source game")]
	game: Option<GameArg>,
	#[clap(long, help = "Script text encoding")]
	enc: Option<EncArg>,
	#[clap(long, default_value = "sugar", help = "Decompile depth")]
	mode: DecompileMode,

	#[clap(long, short, help = "Output file")]
	output: Option<PathBuf>,
}

impl Args {
	fn game_for(&self, path: &Path) -> Option<Game> {
		self.game.map(Game::from).or_else(|| detect_game(path))
	}
}

fn main() -> ExitCode {
	tracing_subscriber::registry()
		.with(tracing_subscriber::fmt::layer().with_writer(std::io::stderr))
		.with(
			tracing_subscriber::EnvFilter::builder()
				.with_default_directive(tracing::Level::INFO.into())
				.from_env_lossy(),
		)
		.init();
	let args = Args::parse();

	let mut success = true;

	if args.output.is_some() && args.files.len() > 1 {
		tracing::error!("Cannot specify output file with multiple input files");
		success = false;
	} else {
		for path in &args.files {
			let _span = tracing::debug_span!("process_arg", path = %path.display()).entered();
			if !path.exists() {
				tracing::error!("File does not exist: {}", path.display());
				success = false;
			} else if path.is_dir() {
				success &= handle_dir(&args, path, args.output.as_deref());
			} else {
				success &= handle_file(&args, path, args.output.as_deref());
			}
		}
	}

	if success {
		ExitCode::SUCCESS
	} else {
		windows_wait();
		ExitCode::FAILURE
	}
}

#[cfg(target_os = "windows")]
fn windows_wait() {
	use windows_sys::Win32::System::Console::{GetConsoleProcessList, GetConsoleWindow};
	if unsafe { GetConsoleWindow() }.is_null() {
		return;
	}
	let process_count: u32 = unsafe { GetConsoleProcessList([0].as_mut_ptr(), 1) };
	if process_count == 1 {
		std::process::Command::new("cmd").arg("/c").arg("pause").status().ok();
	}
}

#[cfg(not(target_os = "windows"))]
fn windows_wait() {}

fn handle_dir(args: &Args, path: &Path, out: Option<&Path>) -> bool {
	let mut krz = Vec::new();
	let mut dat = Vec::new();
	for entry in WalkDir::new(path).into_iter().filter_map(|v| v.ok()) {
		if entry.metadata().is_ok_and(|m| m.is_file()) {
			if entry.path().extension().is_some_and(|e| e == "krz") {
				krz.push(entry.path().strip_prefix(path).unwrap().to_owned());
			} else if entry.path().extension().is_some_and(|e| e == "dat") && !skip_dat(args, entry.path()) {
				dat.push(entry.path().strip_prefix(path).unwrap().to_owned());
			}
		}
	}

	if !krz.is_empty() && !dat.is_empty() {
		tracing::error!(
			"Found both krz ({}) and dat ({}) files in the same directory",
			krz[0].display(),
			dat[0].display()
		);
		false
	} else if !krz.is_empty() {
		let outdir = out_dir(path, out, ".krz", ".dat");
		let mut success = true;
		for file in krz {
			let infile = path.join(&file);
			let outfile = out_file(&outdir.join(&file), ".krz", ".dat");
			success &= compile(args, &infile, &outfile);
		}
		success
	} else if !dat.is_empty() {
		let outdir = out_dir(path, out, ".dat", ".krz");
		let mut success = true;
		for file in dat {
			let infile = path.join(&file);
			let outfile = out_file(&outdir.join(&file), ".dat", ".krz");
			success &= decompile(args, &infile, &outfile);
		}
		success
	} else {
		tracing::error!("No krz or dat files found in directory");
		false
	}
}

fn handle_file(args: &Args, path: &Path, out: Option<&Path>) -> bool {
	if path.extension().is_some_and(|e| e == "krz") {
		let infile = path;
		let outfile = out.map_or_else(|| out_file(path, ".krz", ".dat"), |x| x.to_owned());
		compile(args, infile, &outfile)
	} else if path.extension().is_some_and(|e| e == "dat") {
		let infile = path;
		let outfile = out.map_or_else(|| out_file(path, ".dat", ".krz"), |x| x.to_owned());
		decompile(args, infile, &outfile)
	} else {
		tracing::error!("File is not krz or dat");
		false
	}
}

fn out_file(path: &Path, old_suffix: &str, new_suffix: &str) -> PathBuf {
	let name = path.file_name().unwrap().to_str().unwrap();
	let name = name.strip_suffix(old_suffix).expect("suffix is already checked");
	path.with_file_name(format!("{name}{new_suffix}"))
}

fn out_dir(path: &Path, out: Option<&Path>, old_suffix: &str, new_suffix: &str) -> PathBuf {
	if let Some(out) = out {
		return out.to_owned();
	}
	let name = path.file_name().unwrap().to_str().unwrap();
	if let Some(name) = name.strip_suffix(old_suffix) {
		path.with_file_name(name)
	} else {
		path.with_file_name(format!("{name}{new_suffix}"))
	}
}

fn decompile(args: &Args, infile: &Path, outfile: &Path) -> bool {
	let _span = tracing::error_span!("decompile", file = %infile.display()).entered();
	match decompile_inner(args, infile, outfile) {
		Ok(v) => v,
		Err(e) => {
			tracing::error!("{e}");
			tracing::error!("This is probably a bug in Kreuzen, please report it.");
			false
		}
	}
}

fn compile(args: &Args, infile: &Path, outfile: &Path) -> bool {
	let _span = tracing::error_span!("compile", file = %infile.display()).entered();
	match compile_inner(args, infile, outfile) {
		Ok(v) => v,
		Err(e) => {
			tracing::error!("{e}");
			tracing::error!("This is probably a bug in Kreuzen, please report it.");
			false
		}
	}
}

fn decompile_inner(args: &Args, infile: &Path, outfile: &Path) -> rootcause::Result<bool> {
	let Some(game) = args.game_for(infile) else {
		tracing::error!("Could not detect game from exe name or path; specify --game to decompile");
		return Ok(false);
	};
	let enc = args.enc.map(Enc::from).unwrap_or_else(|| detect_enc(game, infile));

	let bytes = std::fs::read(infile).context_with(|| format!("failed to read file: {}", infile.display()))?;
	let scena = kreuzen::read(game, enc, &bytes).context("failed to read scena")?;
	let scena = match args.mode {
		DecompileMode::Flat => scena,
		DecompileMode::Tree => kreuzen::decompile(&scena)?,
		DecompileMode::Sugar => kreuzen::sugar::resugar(&kreuzen::decompile(&scena)?)?,
	};

	let str = scena.print_to_string();
	write_file(outfile, str.as_bytes())?;
	Ok(true)
}

fn compile_inner(_args: &Args, infile: &Path, outfile: &Path) -> rootcause::Result<bool> {
	let source = std::fs::read_to_string(infile).context_with(|| format!("failed to read file: {}", infile.display()))?;

	let mut errors = diag::Errors::new();
	let scena = kreuzen_syntax::parse(&source, |i| kreuzen::spec::for_game(i.game, i.variant), &mut errors);
	if !errors.is_empty() {
		print!("{}", diag::render(&infile.display().to_string(), &source, &errors));
	}
	if errors.max_severity() >= diag::Severity::Error {
		return Ok(false);
	}
	let Some(scena) = scena else {
		return Ok(false);
	};

	let scena = kreuzen::sugar::desugar(&scena)?;
	let scena = kreuzen::compile(&scena)?;
	let data = kreuzen::write(&scena).context("failed to write scena")?;
	write_file(outfile, &data)?;
	Ok(true)
}

fn skip_dat(args: &Args, path: &Path) -> bool {
	match path.file_name().and_then(|n| n.to_str()) {
		Some("utf8sjis.dat" | "sjisutf8.dat") => true,
		Some("magic.dat") => args.game_for(path) == Some(Game::Tx),
		_ => false,
	}
}

fn detect_game(infile: &Path) -> Option<Game> {
	detect_game_from_exe().or_else(|| {
		parents(infile).find_map(|c| match c {
			"Trails of Cold Steel" => Some(Game::Cs1),
			"Trails of Cold Steel II" => Some(Game::Cs2),
			"The Legend of Heroes Trails of Cold Steel III" => Some(Game::Cs3),
			"The Legend of Heroes Trails of Cold Steel IV" => Some(Game::Cs4),
			"The Legend of Heroes Trails into Reverie" => Some(Game::Reverie),
			"Tokyo Xanadu eX+" => Some(Game::Tx),
			_ => None,
		})
	})
}

fn detect_game_from_exe() -> Option<Game> {
	match std::env::current_exe().ok()?.file_stem()?.to_str()? {
		"kreuzen-cs1" => Some(Game::Cs1),
		"kreuzen-cs2" => Some(Game::Cs2),
		"kreuzen-cs3" => Some(Game::Cs3),
		"kreuzen-cs4" => Some(Game::Cs4),
		"kreuzen-rev" => Some(Game::Reverie),
		"kreuzen-tx" => Some(Game::Tx),
		_ => None,
	}
}

/// For Cs1/Cs2, picks sjis or utf8 based on a `dat`/`dat_us` ancestor folder;
/// every other game is always utf8.
fn detect_enc(game: Game, infile: &Path) -> Enc {
	if matches!(game, Game::Cs1 | Game::Cs2) {
		for c in parents(infile) {
			match c {
				"dat" => return Enc::Sjis,
				"dat_us" => return Enc::Utf8,
				_ => {}
			}
		}
	}
	// Probably a sensible default for the cs1/2 scripts as well;
	// most modders are probably interested in the english ones.
	Enc::Utf8
}

fn parents(path: &Path) -> impl Iterator<Item = &str> {
	path.parent()
		.into_iter()
		.flat_map(|p| p.components())
		.filter_map(|c| c.as_os_str().to_str())
}

fn write_file(outfile: &Path, data: &[u8]) -> rootcause::Result<()> {
	if let Some(parent) = outfile.parent()
		&& !parent.exists()
	{
		std::fs::create_dir_all(parent).context_with(|| format!("failed to create directory: {}", parent.display()))?;
	}
	std::fs::write(outfile, data).context_with(|| format!("failed to write file: {}", outfile.display()))?;
	Ok(())
}

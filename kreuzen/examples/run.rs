use std::io::Write;
use std::path::Path;

use snafu::ResultExt;

fn main() {
	unsafe { compact_debug::enable(true) };
	tracing_subscriber::fmt::init();
	let root = Path::new("scripts");
	for arg in walkdir::WalkDir::new("scripts") {
		let arg = arg.unwrap();
		let path = arg.path();
		if path.extension().is_none_or(|ext| ext != "dat") {
			continue;
		}
		if let Err(err) = process_file(path, path.strip_prefix(root).unwrap()) {
			println!("Error processing file '{}'\n{}", path.display(), snafu::Report::from_error(err));
		}
	}
}

#[derive(Debug, snafu::Snafu)]
enum Error {
	#[snafu(display("Failed to read file"))]
	ReadFile { source: std::io::Error },
	#[snafu(display("Failed to parse file"))]
	ParseFile { source: kreuzen::ReadError },
}

fn process_file(path: &Path, relpath: &Path) -> Result<(), Error> {
	tracing::info!("Processing file: {}", relpath.display());
	let data = std::fs::read(path).context(ReadFileSnafu)?;
	let scena = kreuzen::read(&data).context(ParseFileSnafu)?;
	let outpath = Path::new("out").join(relpath);
	std::fs::create_dir_all(outpath.parent().unwrap()).unwrap();

	let f = std::fs::File::create(&outpath).unwrap();
	let mut f = std::io::BufWriter::new(f);
	write_scena(&mut f, &scena).unwrap();
	Ok(())
}

fn write_scena(f: &mut impl Write, scena: &kreuzen::Scena) -> std::io::Result<()> {
	writeln!(f, "scena {:?} {}", scena.name, scena.version)?;
	for (name, entry) in &scena.entries {
		write!(f, "\nentry {name:?}:")?;
		match entry {
			kreuzen::Entry::Func(v) => write!(f, " {v:#?}")?,
			kreuzen::Entry::Preload(v) => write!(f, " {v:#?}")?,
			kreuzen::Entry::Fc(v) => write!(f, " {v:?}")?,
			kreuzen::Entry::BookPage(v) => write!(f, " {v:#?}")?,
			kreuzen::Entry::BookMetadata(v) => write!(f, " {v:#?}")?,
			kreuzen::Entry::Btlset(v) => write!(f, " {v:?}")?,
			kreuzen::Entry::StyleName(v) => write!(f, " {v:?}")?,
			kreuzen::Entry::ActionTable(v) => {
				for a in v {
					write!(f, "\n- {a:?}")?;
				}
			}
			kreuzen::Entry::AddCollision(v) => {
				for a in v {
					write!(f, "\n- {a:?}")?;
				}
			}
			kreuzen::Entry::AlgoTable(v) => {
				for a in v {
					write!(f, "\n- {a:?}")?;
				}
			}
			kreuzen::Entry::AnimeClipTable(v) => {
				for a in v {
					write!(f, "\n- {a:?}")?;
				}
			}
			kreuzen::Entry::BreakTable(v) => {
				for item in v {
					write!(f, "\n- {item:?}")?;
				}
			}
			kreuzen::Entry::FieldFollowData(v) => write!(f, " {v:?}")?,
			kreuzen::Entry::FieldMonsterData(v) => write!(f, " {v:?}")?,
			kreuzen::Entry::PartTable(v) => {
				for item in v {
					write!(f, "\n- {item:?}")?;
				}
			}
			kreuzen::Entry::ReactionTable(v) => {
				for item in v {
					write!(f, "\n- {item:?}")?;
				}
			}
			kreuzen::Entry::SummonTable(v) => {
				for item in v {
					write!(f, "\n- {item:?}")?;
				}
			}
			kreuzen::Entry::WeaponAttTable(v) => write!(f, " {v:?}")?,
			kreuzen::Entry::Empty => {}
		}
		writeln!(f)?;
	}
	Ok(())
}

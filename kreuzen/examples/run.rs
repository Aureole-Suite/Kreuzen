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
	let scena = kreuzen::parse(&data).context(ParseFileSnafu)?;
	let outdir = Path::new("out");
	let outpath = outdir.join(relpath);
	std::fs::create_dir_all(outpath.parent().unwrap()).unwrap();
	std::fs::write(&outpath, format!("{:#?}", scena)).unwrap();
	Ok(())
}

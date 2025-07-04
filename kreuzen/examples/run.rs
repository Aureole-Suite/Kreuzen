use std::path::{Path, PathBuf};

use snafu::ResultExt;

fn main() {
	tracing_subscriber::fmt::init();
	for arg in glob::glob("scripts/**/*.dat").unwrap() {
		let path = arg.as_ref().unwrap();
		if let Err(err) = process_file(path) {
			println!("Error processing file '{}'\n{}", path.display(), snafu::Report::from_error(err));
		}
	}

	println!();
	let mut counts = kreuzen::COUNTS.lock().unwrap().iter().map(|(i, v)| (i.clone(), *v)).collect::<Vec<_>>();
	counts.sort_by_key(|(_, count)| *count);
	for (k, v) in counts {
		println!("{k}: {v}");
	}
}

#[derive(Debug, snafu::Snafu)]
enum Error {
	#[snafu(display("Failed to read file"))]
	ReadFile { source: std::io::Error },
	#[snafu(display("Failed to parse file"))]
	ParseFile { source: kreuzen::ReadError },
}

fn process_file(path: &Path) -> Result<(), Error> {
	println!("Processing file: {}", path.display());
	let data = std::fs::read(path).context(ReadFileSnafu)?;
	let scp = kreuzen::parse(&data).context(ParseFileSnafu)?;
	Ok(())
}

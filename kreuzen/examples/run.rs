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
}

#[derive(Debug, snafu::Snafu)]
enum Error {
	#[snafu(display("Failed to read file"))]
	ReadFile { source: std::io::Error },
	#[snafu(display("Failed to parse file"))]
	ParseFile { source: kreuzen::ReadError },
}

fn process_file(path: &Path) -> Result<(), Error> {
	let data = std::fs::read(path).context(ReadFileSnafu)?;
	let scp = kreuzen::parse(&data).context(ParseFileSnafu)?;
	Ok(())
}

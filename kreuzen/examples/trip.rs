use std::path::Path;

use gospel::read::Reader;

fn main() {
	unsafe { compact_debug::enable(true) };
	tracing_subscriber::fmt::init();
	for arg in walkdir::WalkDir::new("scripts") {
		let arg = arg.unwrap();
		let path = arg.path();
		if path.extension().is_none_or(|ext| ext != "dat") {
			continue;
		}
		run(path)
	}
}

fn run(path: impl AsRef<Path>) {
	let path = path.as_ref();
	let _span = tracing::error_span!("run", path = %path.display()).entered();
	if let Err(e) = run0(path) {
		for e in std::iter::successors(Some(&e as &dyn std::error::Error), |e| e.source()) {
			tracing::error!("{e}");
		}
	};
}

fn run0(path: &Path) -> Result<(), kreuzen::WriteError> {
	let data = std::fs::read(path).unwrap();
	let scena = match kreuzen::read(&data) {
		Ok(scena) => scena,
		Err(kreuzen::ReadError::BadVersion { version }) => {
			tracing::warn!(version, "skipping bad version");
			return Ok(());
		}
		Err(e) => {
			tracing::error!("failed to read!");
			for e in std::iter::successors(Some(&e as &dyn std::error::Error), |e| e.source()) {
				tracing::error!("{e}");
			}
			return Ok(());
		}
	};
	let data2 = kreuzen::write(&scena)?;
	if data != data2 {
		tracing::warn!("Data mismatch:\n{:#X}{:#X}{scena:#?}\n", Reader::new(&data).dump(), Reader::new(&data2).dump());
		let scena2 = kreuzen::read(&data2).unwrap();
	}
	Ok(())
}


use std::path::Path;

use gospel::read::Reader;

fn main() {
	unsafe { compact_debug::enable(true) };
	tracing_subscriber::fmt::init();
	run("scripts/battle/dat_en/btl0000.dat");
	run("scripts/talk/dat_en/tk_bike.dat");
	run("scripts/talk/dat_en/tk_horse.dat");
	run("scripts/talk/dat_en/tk_celine.dat");
	run("scripts/ani/dat_en/dummy.dat");
	run("scripts/talk/dat_en/tk_ferris.dat");
	run("scripts/talk/dat_en/tk_emily.dat");
	run("scripts/ani/dat_en/vehicle.dat");
}

fn run(path: impl AsRef<Path>) {
	let path = path.as_ref();
	let _span = tracing::error_span!("run", path = %path.display()).entered();
	if let Err(e) = run0(path) {
		tracing::error!("{e:#}");
	};
}

fn run0(path: &Path) -> Result<(), kreuzen::WriteError> {
	let data = std::fs::read(path).unwrap();
	let scena = kreuzen::read(&data).unwrap();
	let data2 = kreuzen::write(&scena)?;
	if data != data2 {
		println!("Data mismatch:\n{:#X}{:#X}{scena:#?}\n", Reader::new(&data).dump(), Reader::new(&data2).dump());
	} else {
		tracing::info!("Data matches");
	}
	Ok(())
}


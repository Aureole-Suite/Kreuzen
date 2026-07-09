mod attribution;
use crate::Scena;

pub fn resugar(scena: &Scena) -> rootcause::Result<Scena> {
	let mut scena = scena.clone();
	attribution::resugar(&mut scena)?;
	Ok(scena)
}

pub fn desugar(scena: &Scena) -> rootcause::Result<Scena> {
	let mut scena = scena.clone();
	attribution::desugar(&mut scena)?;
	Ok(scena)
}

mod attribution;
mod shadow_calls;
use crate::Scena;

pub fn resugar(scena: &Scena) -> rootcause::Result<Scena> {
	let mut scena = scena.clone();
	attribution::resugar(&mut scena)?;
	shadow_calls::resugar(&mut scena)?;
	Ok(scena)
}

pub fn desugar(scena: &Scena) -> rootcause::Result<Scena> {
	let mut scena = scena.clone();
	shadow_calls::desugar(&mut scena)?;
	attribution::desugar(&mut scena)?;
	Ok(scena)
}

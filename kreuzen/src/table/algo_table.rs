use super::*;

#[derive(Debug, snafu::Snafu)]
pub enum ReadError {
	#[snafu(display("invalid read (at {location})"), context(false))]
	Read {
		source: gospel::read::Error,
		#[snafu(implicit)]
		location: snafu::Location,
	},
}

#[derive(Debug, Clone, PartialEq)]
pub struct Algo {
	pub id: u16,
	pub chance: u8,
	pub use_limit: u8,
	pub target_priority: u8,
	pub cond: (u8, u32, u32, u32),
	pub no_move: u32,
}

impl Algo {
	pub fn dummy() -> Self {
		Self {
			id: 0xFFFF,
			chance: 0,
			use_limit: 0,
			target_priority: 0,
			cond: (0, 1, 2, 3),
			no_move: 4,
		}
	}
}

pub(crate) fn read(f: &mut VReader) -> Result<Vec<Algo>, ReadError> {
	let mut table = Vec::new();
	while !f.remaining().is_empty() {
		let id = f.u16()?;
		let cond = f.u8()?;
		let chance = f.u8()?;
		let use_limit = f.u8()?;
		let target_priority = f.u8()?;
		f.check_u16(0)?;
		let cond = (cond, f.u32()?, f.u32()?, f.u32()?);
		let no_move = f.u32()?;
		f.check_u32(use_limit as u32)?;
		f.check_u32(0)?;
		table.push(Algo { id, chance, use_limit, target_priority, cond, no_move });
	}
	Ok(table)
}

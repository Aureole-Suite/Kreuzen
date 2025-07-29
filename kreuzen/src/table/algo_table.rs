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

#[derive(Debug, snafu::Snafu)]
pub enum WriteError {
	#[snafu(transparent, context(false))]
	Value { source: crate::ValueError },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Algo {
	pub id: u16,
	pub chance: u8,
	pub use_limit: u8,
	pub target_priority: u8,
	pub cond: (u8, u32, u32, u32, u32),
}

impl Algo {
	pub fn dummy() -> Self {
		Self {
			id: 0xFFFF,
			chance: 0,
			use_limit: 0,
			target_priority: 0,
			cond: (0, 1, 2, 3, 4),
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
		let cond = (cond, f.u32()?, f.u32()?, f.u32()?, f.u32()?);
		f.check_u32(use_limit as u32)?;
		f.check_u32(0)?;
		table.push(Algo { id, chance, use_limit, target_priority, cond });
	}
	Ok(table)
}

pub(crate) fn write(f: &mut VWriter, table: &[Algo]) -> Result<(), WriteError> {
	for algo in table {
		f.u16(algo.id);
		f.u8(algo.cond.0);
		f.u8(algo.chance);
		f.u8(algo.use_limit);
		f.u8(algo.target_priority);
		f.u16(0);
		f.u32(algo.cond.1);
		f.u32(algo.cond.2);
		f.u32(algo.cond.3);
		f.u32(algo.cond.4);
		f.u32(algo.use_limit as u32);
		f.u32(0);
	}
	Ok(())
}

use super::*;

#[derive(Debug, snafu::Snafu)]
pub enum ReadError {
	#[snafu(display("invalid read (at {location})"), context(false))]
	Read {
		source: gospel::read::Error,
		#[snafu(implicit)]
		location: snafu::Location,
	},
	#[snafu(display("bad reaction kind {kind}"))]
	BadKind { kind: u32 },
}

#[derive(Debug, snafu::Snafu)]
pub enum WriteError {
	#[snafu(transparent, context(false))]
	Value { source: crate::ValueError },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Reaction {
	_0 {
		id: u16,
		// This is probably star rating and counter/block/hit/crit chance for each part.
		// Unknown which is which though.
		u2: [(u16, f32, f32, f32, f32); 3],
	},
	_1 {
		id: u16,
		u1: u16,
	}
}

pub(crate) fn read(f: &mut VReader) -> Result<Vec<Reaction>, ReadError> {
	let mut table = Vec::new();
	while !f.remaining().is_empty() {
		let id = f.u16()?;
		if id == 0xFFFF {
			break;
		}
		let u1 = (f.u16()?, f.u16()?, f.u16()?);
		let u3 = (u1.0, f.f32()?, f.f32()?, f.f32()?, f.f32()?);
		let u4 = (u1.1, f.f32()?, f.f32()?, f.f32()?, f.f32()?);
		let u5 = (u1.2, f.f32()?, f.f32()?, f.f32()?, f.f32()?);
		let u6 = f.u32()?;
		let r = match u6 {
			0 => Reaction::_0 {
				id,
				u2: [u3, u4, u5],
			},
			1 => Reaction::_1 {
				id,
				u1: u1.0
			},
			kind => return BadKindSnafu { kind }.fail(),
		};
		table.push(r);
	}
	f.check(&[0; 58])?;
	Ok(table)
}

pub(crate) fn write(f: &mut VWriter, table: &[Reaction]) -> Result<(), WriteError> {
	for r in table {
		match r {
			Reaction::_0 { id, u2 } => {
				f.u16(*id);
				f.u16(u2[0].0);
				f.u16(u2[1].0);
				f.u16(u2[2].0);
				for &(_, f1, f2, f3, f4) in u2 {
					f.f32(f1);
					f.f32(f2);
					f.f32(f3);
					f.f32(f4);
				}
				f.u32(0);
			}
			Reaction::_1 { id, u1 } => {
				f.u16(*id);
				f.u16(*u1);
				f.slice(&[0; 52]);
				f.u32(1);
			}
		}
	}
	f.u16(0xFFFF);
	f.slice(&[0; 58]);
	Ok(())
}

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

// There's always an Action::dummy() in each table, but in one case it's not the last entry.
#[derive(Debug, Clone, PartialEq)]
pub struct Action {
	pub id: u16,
	pub u1: (u8, u8),
	pub target: (u8, u8, u16),
	pub u2: (f32, f32, f32),
	pub time: (u16, u16),
	pub effects: ArrayVec<(u16, u32, u32, u32), 5>,
	pub u3: (u16, u16),
	pub flags: String,
	pub ani: String,
	pub name: String,
}

impl Action {
	pub fn dummy() -> Self {
		Self {
			id: 0xFFFF,
			u1: (0, 0),
			target: (0, 0, 0),
			u2: (0.0, 0.0, 0.0),
			time: (0, 0),
			effects: ArrayVec::from([
				(1, 0, 0, 0),
				(2, 0, 0, 0),
				(3, 0, 0, 0),
				(4, 0, 0, 0),
				(5, 0, 0, 0),
			]),
			u3: (0, 0),
			flags: String::new(),
			ani: String::new(),
			name: String::new(),
		}
	}
}

pub fn read(f: &mut VReader) -> Result<Vec<Action>, ReadError> {
	let mut table = Vec::new();
	while !f.remaining().is_empty() {
		let id = f.u16()?;
		let u1 = (f.u8()?, f.u8()?);
		let target = (f.u8()?, f.u8()?, f.u16()?);
		let u2 = (f.f32()?, f.f32()?, f.f32()?);
		let time = (f.u16()?, f.u16()?);
		let u4 = (f.u16()?, f.u16()?, f.u16()?, f.u16()?, f.u16()?);
		f.check_u16(0)?;
		let mut effects = ArrayVec::<_, 5>::new();
		effects.push((u4.0, f.u32()?, f.u32()?, f.u32()?));
		effects.push((u4.1, f.u32()?, f.u32()?, f.u32()?));
		effects.push((u4.2, f.u32()?, f.u32()?, f.u32()?));
		effects.push((u4.3, f.u32()?, f.u32()?, f.u32()?));
		effects.push((u4.4, f.u32()?, f.u32()?, f.u32()?));
		while effects.last().is_some_and(|v| *v == (0, 0, 0, 0)) {
			effects.pop();
		}
		let u3 = (f.u16()?, f.u16()?);
		let flags = f.sstr(16)?;
		let ani = f.sstr(32)?;
		let name = f.sstr(64)?;
		table.push(Action { id, u1, target, u2, time, effects, u3, flags, ani, name });
	}
	Ok(table)
}

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

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Collision {
	pub a: u32,
	pub b: [f32; 5],
}

pub(crate) fn read(f: &mut VReader) -> Result<Vec<Collision>, ReadError> {
	let mut out = Vec::new();
	for _ in 0..f.u8()? {
		out.push(Collision {
			a: f.u32()?,
			b: [f.f32()?, f.f32()?, f.f32()?, f.f32()?, f.f32()?],
		});
	}
	Ok(out)
}


pub(crate) fn write(f: &mut VWriter, data: &[Collision]) -> Result<(), WriteError> {
	f.u8(data.len() as u8);
	for collision in data {
		f.u32(collision.a);
		for &value in &collision.b {
			f.f32(value);
		}
	}
	Ok(())
}

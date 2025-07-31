use super::*;

#[derive(Debug, snafu::Snafu)]
pub enum ReadError {
	#[snafu(display("invalid read (at {location})"), context(false))]
	Read {
		source: gospel::read::Error,
		#[snafu(implicit)]
		location: snafu::Location,
	},
	#[snafu(display("mismatch between kind and params: {preload:?}"))]
	BadPreload { preload: RawPreload },
}

#[derive(Debug, snafu::Snafu)]
pub enum WriteError {
	#[snafu(transparent, context(false))]
	Value { source: crate::ValueError },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Preload {
	PkgLoad(String),
	EffLoad(String),
	SoundPlay(u32),
	SoundPlayRandom(u32),
	Voiceline(u32), // dialogue Voiceline, not an opcode
	CharAniclipPlay(crate::types::Char, String),
	#[expect(non_camel_case_types)]
	opCE02(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RawPreload {
	pub kind: u16,
	pub charid: u16,
	pub u32: u32,
	pub str: String,
}

pub(crate) fn read(f: &mut VReader) -> Result<Vec<Preload>, ReadError> {
	let mut table = Vec::new();
	while !f.remaining().is_empty() {
		let preload = RawPreload {
			kind: f.u16()?,
			charid: f.u16()?,
			u32: f.u32()?,
			str: f.sstr(32)?,
		};
		table.push(match preload.kind {
			0 => {
				ensure!(preload.charid == 0xFFFF, BadPreloadSnafu { preload });
				ensure!(preload.str.is_empty(), BadPreloadSnafu { preload });
				ensure!(preload.u32 == 0, BadPreloadSnafu { preload });
				break;
			}
			2 => {
				ensure!(preload.charid == 0xFFFF, BadPreloadSnafu { preload });
				ensure!(preload.u32 == 0, BadPreloadSnafu { preload });
				Preload::PkgLoad(preload.str)
			}
			3 => {
				ensure!(preload.charid == 0xFFFF, BadPreloadSnafu { preload });
				ensure!(preload.u32 == 0, BadPreloadSnafu { preload });
				Preload::EffLoad(preload.str)
			}
			4 => {
				ensure!(preload.charid == 0xFFFF, BadPreloadSnafu { preload });
				ensure!(preload.str.is_empty(), BadPreloadSnafu { preload });
				Preload::SoundPlay(preload.u32)
			}
			5 => {
				ensure!(preload.charid == 0xFFFF, BadPreloadSnafu { preload });
				ensure!(preload.str.is_empty(), BadPreloadSnafu { preload });
				Preload::SoundPlayRandom(preload.u32)
			}
			7 => {
				ensure!(preload.charid == 0xFFFF, BadPreloadSnafu { preload });
				ensure!(preload.str.is_empty(), BadPreloadSnafu { preload });
				Preload::Voiceline(preload.u32)
			}
			9 => {
				ensure!(preload.u32 == 0, BadPreloadSnafu { preload });
				Preload::CharAniclipPlay(preload.charid.into(), preload.str)
			}
			10 => {
				ensure!(preload.charid == 0xFFFF, BadPreloadSnafu { preload });
				ensure!(preload.u32 == 0, BadPreloadSnafu { preload });
				Preload::opCE02(preload.str)
			}
			_ => return BadPreloadSnafu { preload }.fail(),
		})
	}
	Ok(table)
}

pub(crate) fn write(f: &mut VWriter, table: &[Preload]) -> Result<(), WriteError> {
	for preload in table {
		let (kind, charid, u32, str) = match preload {
			Preload::PkgLoad(s) => (2, 0xFFFF, 0, s.as_str()),
			Preload::EffLoad(s) => (3, 0xFFFF, 0, s.as_str()),
			Preload::SoundPlay(u) => (4, 0xFFFF, *u, ""),
			Preload::SoundPlayRandom(u) => (5, 0xFFFF, *u, ""),
			Preload::Voiceline(u) => (7, 0xFFFF, *u, ""),
			Preload::CharAniclipPlay(c, s) => (9, c.0, 0, s.as_str()),
			Preload::opCE02(s) => (10, 0xFFFF, 0, s.as_str()),
		};
		f.u16(kind);
		f.u16(charid);
		f.u32(u32);
		f.sstr(32, str)?;
	}
	f.u16(0);
	f.u16(0xFFFF);
	f.u32(0);
	f.sstr(32, "")?;
	Ok(())
}

use super::*;

#[derive(Debug, snafu::Snafu)]
pub enum ReadError {
	#[snafu(display("invalid read (at {location})"), context(false))]
	Read {
		source: gospel::read::Error,
		#[snafu(implicit)]
		location: snafu::Location,
	},
	#[snafu(display("mismatch between kind and params: {effect:?}"))]
	BadEffect { effect: RawEffect },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Effect {
	_02(String), // C_CHRX10
	_03(String), // battle/atk000_2.eff
	_04(u32),
	_05(u32),
	_07(u32),
	_09(u16, String), // charid, BTL_CRAFT00_01
	_0A(String), // BTL_CRAFT01_02_GS
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RawEffect {
	pub kind: u16,
	pub charid: u16,
	pub u32: u32,
	pub str: String,
}

pub fn read(f: &mut VReader) -> Result<Vec<Effect>, ReadError> {
	let mut table = Vec::new();
	while !f.remaining().is_empty() {
		let effect = RawEffect {
			kind: f.u16()?,
			charid: f.u16()?,
			u32: f.u32()?,
			str: f.sstr(32)?,
		};
		table.push(match effect.kind {
			0 => {
				ensure!(effect.charid == 0xFFFF, BadEffectSnafu { effect });
				ensure!(effect.str.is_empty(), BadEffectSnafu { effect });
				ensure!(effect.u32 == 0, BadEffectSnafu { effect });
				break;
			}
			2 => {
				ensure!(effect.charid == 0xFFFF, BadEffectSnafu { effect });
				ensure!(effect.u32 == 0, BadEffectSnafu { effect });
				Effect::_02(effect.str)
			}
			3 => {
				ensure!(effect.charid == 0xFFFF, BadEffectSnafu { effect });
				ensure!(effect.u32 == 0, BadEffectSnafu { effect });
				Effect::_03(effect.str)
			}
			4 => {
				ensure!(effect.charid == 0xFFFF, BadEffectSnafu { effect });
				ensure!(effect.str.is_empty(), BadEffectSnafu { effect });
				Effect::_04(effect.u32)
			}
			5 => {
				ensure!(effect.charid == 0xFFFF, BadEffectSnafu { effect });
				ensure!(effect.str.is_empty(), BadEffectSnafu { effect });
				Effect::_05(effect.u32)
			}
			7 => {
				ensure!(effect.charid == 0xFFFF, BadEffectSnafu { effect });
				ensure!(effect.str.is_empty(), BadEffectSnafu { effect });
				Effect::_07(effect.u32)
			}
			9 => {
				ensure!(effect.u32 == 0, BadEffectSnafu { effect });
				Effect::_09(effect.charid, effect.str)
			}
			10 => {
				ensure!(effect.charid == 0xFFFF, BadEffectSnafu { effect });
				ensure!(effect.u32 == 0, BadEffectSnafu { effect });
				Effect::_0A(effect.str)
			}
			_ => return BadEffectSnafu { effect }.fail(),
		})
	}
	Ok(table)
}

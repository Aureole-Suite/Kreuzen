use std::{fmt::Debug, ops::ControlFlow};
use gospel::read::Le as _;
use snafu::{ensure, ResultExt as _};

use crate::{ReaderaExt as _, VReader};

#[derive(Debug, snafu::Snafu)]
pub enum TableError<E: std::error::Error + 'static> {
	#[snafu(display("invalid read (at {location})"), context(false))]
	Read {
		source: gospel::read::Error,
		#[snafu(implicit)]
		location: snafu::Location,
	},
	#[snafu(display("failed to read entry at {pos:X}"))]
	Entry {
		pos: usize,
		source: E,
	}
}

fn read_table<T: Debug, E: std::error::Error>(
	mut f: VReader,
	end: usize,
	name: &str,
	mut entry: impl FnMut(&mut VReader) -> Result<ControlFlow<(), T>, E>,
) -> Result<Vec<T>, TableError<E>> {
	let start = f.pos();
	let mut table = Vec::new();
	while !at_end(&mut f, end) {
		let pos = f.pos();
		match entry(&mut f).context(EntrySnafu { pos }) {
			Ok(ControlFlow::Continue(val)) => {
				tracing::trace!("{pos:X} {val:?}");
				table.push(val);
			}
			Ok(ControlFlow::Break(())) => {
				break;
			}
			Err(e) => {
				tracing::error!("table {name} ({start:05X})");
				for e in std::iter::successors(Some(&e as &dyn std::error::Error), |e| e.source()) {
					tracing::error!("{e}");
				}
				print!("{:#X}", f.dump().start(start).end(end));
				break;
			}
		}
	}
	Ok(table)
}

fn at_end(f: &mut VReader<'_>, end: usize) -> bool {
	let rest = &f.data()[f.pos()..end];
	rest.len() <= 3 && rest.iter().all(|&b| b == 0)
}

#[derive(Debug, snafu::Snafu)]
pub enum EffectError {
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
	// All tables end with a 00 and a 01 entry, they are not included in the output.
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RawEffect {
	pub kind: u16,
	pub charid: u16,
	pub u32: u32,
	pub str: String,
}

pub fn read_effect(f: VReader, end: usize, name: &str) -> Result<Vec<Effect>, TableError<EffectError>> {
	read_table(f, end, name, |f| {
		let effect = RawEffect {
			kind: f.u16()?,
			charid: f.u16()?,
			u32: f.u32()?,
			str: f.sstr(32)?,
		};
		match effect.kind {
			0 => {
				ensure!(effect.charid == 0xFFFF, BadEffectSnafu { effect });
				ensure!(effect.str.is_empty(), BadEffectSnafu { effect });
				ensure!(effect.u32 == 0, BadEffectSnafu { effect });
				Ok(ControlFlow::Break(()))
			}
			2 => {
				ensure!(effect.charid == 0xFFFF, BadEffectSnafu { effect });
				ensure!(effect.u32 == 0, BadEffectSnafu { effect });
				Ok(ControlFlow::Continue(Effect::_02(effect.str)))
			}
			3 => {
				ensure!(effect.charid == 0xFFFF, BadEffectSnafu { effect });
				ensure!(effect.u32 == 0, BadEffectSnafu { effect });
				Ok(ControlFlow::Continue(Effect::_03(effect.str)))
			}
			4 => {
				ensure!(effect.charid == 0xFFFF, BadEffectSnafu { effect });
				ensure!(effect.str.is_empty(), BadEffectSnafu { effect });
				Ok(ControlFlow::Continue(Effect::_04(effect.u32)))
			}
			5 => {
				ensure!(effect.charid == 0xFFFF, BadEffectSnafu { effect });
				ensure!(effect.str.is_empty(), BadEffectSnafu { effect });
				Ok(ControlFlow::Continue(Effect::_05(effect.u32)))
			}
			7 => {
				ensure!(effect.charid == 0xFFFF, BadEffectSnafu { effect });
				ensure!(effect.str.is_empty(), BadEffectSnafu { effect });
				Ok(ControlFlow::Continue(Effect::_07(effect.u32)))
			}
			9 => {
				ensure!(effect.u32 == 0, BadEffectSnafu { effect });
				Ok(ControlFlow::Continue(Effect::_09(effect.charid, effect.str)))
			}
			10 => {
				ensure!(effect.charid == 0xFFFF, BadEffectSnafu { effect });
				ensure!(effect.u32 == 0, BadEffectSnafu { effect });
				Ok(ControlFlow::Continue(Effect::_0A(effect.str)))
			}
			_ => BadEffectSnafu { effect }.fail(),
		}
	})
}

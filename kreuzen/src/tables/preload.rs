use gospel::read::Le as _;

use crate::{Game, io::VReader, types::Char};

#[expect(non_camel_case_types)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Preload {
	Call(u32, String),
	PkgLoad(String),
	EffLoad(String),
	SoundPlay(u32),
	SoundPlayRandom(u32),
	Voiceline(u32), // dialogue Voiceline, not an opcode
	CharAniclipPlay(Char, String),
	op4F(String), // 4F in cs1/tx, 50 in cs2
	opCE02(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct RawPreload {
	kind: u16,
	charid: Option<Char>,
	u32: Option<u32>,
	str: Option<String>,
}
impl RawPreload {
	fn charid(&mut self) -> Char { self.charid.take().unwrap() }
	fn u32(&mut self) -> u32 { self.u32.take().unwrap() }
	fn str(&mut self) -> String { self.str.take().unwrap() }

	fn finish(self, null: Char) -> eyre::Result<()> {
		let mut errs = Vec::new();
		if let Some(v) = self.charid && v != null {
			errs.push(format!("{v:?}"));
		}
		if let Some(v) = self.u32 && v != 0 {
			errs.push(format!("{v:?}"));
		}
		if let Some(v) = self.str && !v.is_empty() {
			errs.push(format!("{v:?}"));
		}
		if !errs.is_empty() {
			eyre::bail!("unexpected fields in preload {:02X}: {}", self.kind, errs.join(", "));
		}
		Ok(())
	}
}

pub(crate) fn read(f: &mut VReader, _: usize) -> eyre::Result<Vec<Preload>> {
	let mut table = Vec::new();
	loop {
		let mut preload = RawPreload {
			kind: f.u16()?,
			charid: Some(f.u16()?.into()),
			u32: Some(f.u32()?),
			str: Some(f.sstr(32)?),
		};
		let null = match f.game {
			Game::Cs1 | Game::Cs2 => Char(0xFFFD),
			_ => Char(0xFFFF),
		};
		table.push(match preload.kind {
			0 => {
				preload.finish(null)?;
				break;
			}
			1 => Preload::Call(preload.u32(), preload.str()),
			2 => Preload::PkgLoad(preload.str()),
			3 => Preload::EffLoad(preload.str()),
			4 => Preload::SoundPlay(preload.u32()),
			5 => Preload::SoundPlayRandom(preload.u32()),
			7 => Preload::Voiceline(preload.u32()),
			8 => Preload::op4F(preload.str()),
			9 => Preload::CharAniclipPlay(preload.charid(), preload.str()),
			10 => Preload::opCE02(preload.str()),
			_ => eyre::bail!("unknown preload kind {:02X}", preload.kind),
		});
		preload.finish(null)?;
	}
	f.check_u8(1)?;
	Ok(table)
}

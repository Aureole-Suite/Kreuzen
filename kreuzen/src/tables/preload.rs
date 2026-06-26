use gospel::read::Le as _;
use gospel::write::{Le as _, Writer};

use crate::Game;
use crate::code::{Arg, FlatOp};
use crate::io::{OData, CReader, WriterExt as _};
use crate::text::{TextControl, TextPart};
use crate::types::{Char, Sound};

#[expect(non_camel_case_types)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Preload {
	Call(u32, String),
	PkgLoad(String),
	EffLoad(String),
	SoundPlay(Sound),
	SoundPlayVoice(Sound),
	Voice(Sound), // dialogue Voiceline, not an opcode
	CharAniclipPlay(Char, String),
	NameplateShow(String),
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

	fn finish(self, null: Char) -> rootcause::Result<()> {
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
			rootcause::bail!("unexpected fields in preload {:02X}: {}", self.kind, errs.join(", "));
		}
		Ok(())
	}
}

pub(crate) fn read(f: &mut CReader) -> rootcause::Result<Vec<Preload>> {
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
		let sound = |v: u32| -> rootcause::Result<Sound> {
			u16::try_from(v).map(Sound).map_err(|_| rootcause::report!("sound id {v:#X} out of bounds"))
		};
		table.push(match preload.kind {
			0 => {
				preload.finish(null)?;
				break;
			}
			1 => Preload::Call(preload.u32(), preload.str()),
			2 => Preload::PkgLoad(preload.str()),
			3 => Preload::EffLoad(preload.str()),
			4 => Preload::SoundPlay(sound(preload.u32())?),
			5 => Preload::SoundPlayVoice(sound(preload.u32())?),
			7 => Preload::Voice(sound(preload.u32())?),
			8 => Preload::NameplateShow(preload.str()),
			9 => Preload::CharAniclipPlay(preload.charid(), preload.str()),
			10 => Preload::opCE02(preload.str()),
			_ => rootcause::bail!("unknown preload kind {:02X}", preload.kind),
		});
		preload.finish(null)?;
	}
	Ok(table)
}

pub(crate) fn write(d: &OData, preload: &[Preload]) -> rootcause::Result<Writer> {
	let charid = match d.game {
		Game::Cs1 | Game::Cs2 => Char(0xFFFD),
		_ => Char(0xFFFF),
	};

	let mut f = Writer::new();
	let mut write = |kind: u16, charid: Char, u32: u32, str: &str| -> rootcause::Result<()> {
		f.u16(kind);
		f.u16(charid.0);
		f.u32(u32);
		f.sstr(32, d.enc, str)
	};
	for p in preload {
		match p {
			Preload::Call(u32, str) => write(1, charid, *u32, str)?,
			Preload::PkgLoad(str) => write(2, charid, 0, str)?,
			Preload::EffLoad(str) => write(3, charid, 0, str)?,
			Preload::SoundPlay(s) => write(4, charid, s.0 as u32, "")?,
			Preload::SoundPlayVoice(s) => write(5, charid, s.0 as u32, "")?,
			Preload::Voice(s) => write(7, charid, s.0 as u32, "")?,
			Preload::NameplateShow(str) => write(8, charid, 0, str)?,
			Preload::CharAniclipPlay(charid, str) => write(9, *charid, 0, str)?,
			Preload::opCE02(str) => write(10, charid, 0, str)?,
		}
	}
	write(0, charid, 0, "")?;
	Ok(f)
}

const NO_PRELOAD: &[&str] = &["Init", "Init_Replay"];

pub fn from_code(ops: &[FlatOp], name: &str, functions: &[&str]) -> Vec<Preload> {
	if NO_PRELOAD.contains(&name) {
		return Vec::new();
	}
	let mut out = Vec::new();
	for op in ops {
		let FlatOp::Op(op) = op else { continue };
		match (op.name, op.args.as_slice()) {
			("call", [Arg::Int(n), Arg::Str(s)]) if functions.contains(&s.as_str()) => out.push(Preload::Call(*n as u32, s.clone())),
			("PkgLoad", [Arg::Str(s)]) => out.push(Preload::PkgLoad(s.clone())),
			("EffLoad", [_, _, Arg::Str(s)]) => out.push(Preload::EffLoad(s.clone())),
			("SoundPlay", [Arg::Sound(s), ..]) => out.push(Preload::SoundPlay(*s)),
			("SoundPlayVoice", [Arg::Sound(s), ..]) => out.push(Preload::SoundPlayVoice(*s)),
			("SoundPlayRandom", [_, args@..]) => {
				let mut n = 0;
				for (i, v) in args.iter().enumerate() {
					let Arg::Sound(s) = v else { continue };
					if s.0 != 0 {
						n = i + 1;
					}
				}
				for v in &args[..n] {
					if let Arg::Sound(s) = v {
						out.push(Preload::SoundPlayVoice(*s));
					}
				}
			}
			("TextTalk"|"TextShow", [_, Arg::Text(text)]) => {
				for part in &text.0 {
					if let TextPart::Control(TextControl::Voice(id)) = part {
						// Unclear if VoiceSilent should apply this, since that one only exists in CS1 which doesn't have preload
						out.push(Preload::Voice(*id));
					}
				}
			}
			("NameplateShow", [_, _, Arg::Str(s), _, _]) => out.push(Preload::NameplateShow(s.clone())),
			("CharAniclipPlay", [Arg::Char(c), Arg::Str(s), ..]) if s != "_stop_" => out.push(Preload::CharAniclipPlay(*c, s.clone())),
			_ => {},
		}
	}
	out
}

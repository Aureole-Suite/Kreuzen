use gospel::read::Le as _;
use gospel::write::{Le as _, Writer};

use crate::Enc;
use crate::types::Item;

#[derive(Debug, Clone, PartialEq)]
pub struct Text(pub Vec<TextPart>);

#[derive(Clone, PartialEq, derive_more::Debug)]
pub enum TextPart {
	#[debug("{_0:?}")]
	String(String),
	#[debug("{_0:?}")]
	Control(TextControl),
}

#[derive(Debug, Clone, PartialEq)]
pub enum TextControl {
	Line,
	Page,
	_03,
	_06,
	_07,
	_08,
	_09,
	_0B,
	_0C,
	_0F,
	Item(Item),
	Voiceline(u32),
	_12(u32),
	_13,
	_16,
	_17(u16),
	_18,
	_19(u16),
	_1A,
}

impl Text {
	pub(crate) fn read(f: &mut crate::VReader) -> rootcause::Result<Text> {
		let mut out = Vec::new();
		let mut scratch = Vec::new();
		loop {
			let byte = f.u8()?;
			if byte >= 0x20 {
				scratch.push(byte);
			} else {
				if !scratch.is_empty() {
					out.push(TextPart::String(f.decode(&scratch)?));
					scratch.clear();
				}
				let c = match byte {
					0x00 => break,
					0x01 => TextControl::Line,
					0x02 => TextControl::Page,
					0x03 => TextControl::_03,
					0x06 => TextControl::_06,
					0x07 => TextControl::_07,
					0x08 => TextControl::_08,
					0x09 => TextControl::_09,
					0x0B => TextControl::_0B,
					0x0C => TextControl::_0C,
					0x0F => TextControl::_0F,
					0x10 => TextControl::Item(Item(f.u16()?)),
					0x11 => TextControl::Voiceline(f.u32()?),
					0x12 => TextControl::_12(f.u32()?),
					0x13 => TextControl::_13,
					0x16 => TextControl::_16,
					0x17 => TextControl::_17(f.u16()?),
					0x18 => TextControl::_18,
					0x19 => TextControl::_19(f.u16()?),
					0x1A => TextControl::_1A,
					byte => {
						f.rewind();
						rootcause::bail!("Unknown text control byte: {byte:02X}");
					}
				};
				out.push(TextPart::Control(c));
			}
		}
		Ok(Text(out))
	}

	pub(crate) fn write(&self, enc: Enc, f: &mut Writer) -> rootcause::Result<()> {
		for part in &self.0 {
			match part {
				TextPart::String(s) => f.slice(&encode(enc, s)?),
				TextPart::Control(c) => match *c {
					TextControl::Line => f.u8(0x01),
					TextControl::Page => f.u8(0x02),
					TextControl::_03 => f.u8(0x03),
					TextControl::_06 => f.u8(0x06),
					TextControl::_07 => f.u8(0x07),
					TextControl::_08 => f.u8(0x08),
					TextControl::_09 => f.u8(0x09),
					TextControl::_0B => f.u8(0x0B),
					TextControl::_0C => f.u8(0x0C),
					TextControl::_0F => f.u8(0x0F),
					TextControl::Item(Item(v)) => { f.u8(0x10); f.u16(v); }
					TextControl::Voiceline(v) => { f.u8(0x11); f.u32(v); }
					TextControl::_12(v) => { f.u8(0x12); f.u32(v); }
					TextControl::_13 => f.u8(0x13),
					TextControl::_16 => f.u8(0x16),
					TextControl::_17(v) => { f.u8(0x17); f.u16(v); }
					TextControl::_18 => f.u8(0x18),
					TextControl::_19(v) => { f.u8(0x19); f.u16(v); }
					TextControl::_1A => f.u8(0x1A),
				}
			}
		}
		f.u8(0x00);
		Ok(())
	}
}

fn encode(enc: Enc, s: &str) -> rootcause::Result<Vec<u8>> {
	match enc {
		Enc::Utf8 => Ok(s.as_bytes().to_vec()),
		Enc::Sjis => match falcom_sjis::encode(s) {
			Ok(bytes) => Ok(bytes),
			Err(pos) => rootcause::bail!("invalid Shift-JIS in text at byte {pos}: {s:?}"),
		}
	}
}

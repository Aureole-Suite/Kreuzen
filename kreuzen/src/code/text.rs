use gospel::read::Le as _;

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
	pub(crate) fn read(f: &mut crate::VReader) -> eyre::Result<Text> {
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
						eyre::bail!("Unknown text control byte: {byte:02X}");
					}
				};
				out.push(TextPart::Control(c));
			}
		}
		Ok(Text(out))
	}
}

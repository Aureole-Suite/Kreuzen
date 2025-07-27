use gospel::{read::Le as _, write::Le as _};

#[derive(Debug, snafu::Snafu)]
pub enum ReadError {
	#[snafu(display("invalid read (at {location})"), context(false))]
	Read {
		source: gospel::read::Error,
		#[snafu(implicit)]
		location: snafu::Location,
	},
	#[snafu(display("invalid string: {text:?}"))]
	String {
		text: String,
	},
	#[snafu(display("unknown dialogue control byte {byte:02X}"))]
	BadControl {
		byte: u8,
	},
}

#[derive(Debug)]
struct Private;
#[derive(Debug, snafu::Snafu)]
pub enum WriteError {
	#[allow(private_interfaces)] #[doc(hidden)] Never { never: Private },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Dialogue(pub Vec<DialoguePart>);

#[derive(Clone, PartialEq)]
pub enum DialoguePart {
	String(String),
	Control(DialogueControl),
}

#[derive(Debug, Clone, PartialEq)]
pub enum DialogueControl {
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
	_10(u16),
	_11(u32),
	_12(u32),
	_13,
	_16,
	_17(u16),
	_19(u16),
	_1A,
}

impl std::fmt::Debug for DialoguePart {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::String(s) => s.fmt(f),
			Self::Control(c) => c.fmt(f),
		}
	}
}

impl Dialogue {
	pub(crate) fn read(f: &mut crate::VReader) -> Result<Dialogue, ReadError> {
		let mut out = Vec::new();
		let mut scratch = Vec::new();
		loop {
			let byte = f.u8()?;
			if byte >= 0x20 {
				scratch.push(byte);
			} else {
				if !scratch.is_empty() {
					match String::from_utf8(scratch) {
						Ok(text) => out.push(DialoguePart::String(text)),
						Err(e) => {
							let text = String::from_utf8_lossy(&e.into_bytes()).into_owned();
							return StringSnafu { text }.fail();
						}
					}
					scratch = Vec::new();
				}
				let c = match byte {
					0x00 => break,
					0x01 => DialogueControl::Line,
					0x02 => DialogueControl::Page,
					0x03 => DialogueControl::_03,
					0x06 => DialogueControl::_06,
					0x07 => DialogueControl::_07,
					0x08 => DialogueControl::_08,
					0x09 => DialogueControl::_09,
					0x0B => DialogueControl::_0B,
					0x0C => DialogueControl::_0C,
					0x0F => DialogueControl::_0F,
					0x10 => DialogueControl::_10(f.u16()?),
					0x11 => DialogueControl::_11(f.u32()?),
					0x12 => DialogueControl::_12(f.u32()?),
					0x13 => DialogueControl::_13,
					0x16 => DialogueControl::_16,
					0x17 => DialogueControl::_17(f.u16()?),
					0x19 => DialogueControl::_19(f.u16()?),
					0x1A => DialogueControl::_1A,
					byte => return BadControlSnafu { byte }.fail(),
				};
				out.push(DialoguePart::Control(c));
			}
		}
		Ok(Dialogue(out))
	}

	pub(crate) fn write(&self, f: &mut crate::VWriter) -> Result<(), WriteError> {
		for part in &self.0 {
			match part {
				DialoguePart::String(s) => f.slice(s.as_bytes()),
				DialoguePart::Control(c) => match c {
					DialogueControl::Line => f.u8(0x01),
					DialogueControl::Page => f.u8(0x02),
					DialogueControl::_03 => f.u8(0x03),
					DialogueControl::_06 => f.u8(0x06),
					DialogueControl::_07 => f.u8(0x07),
					DialogueControl::_08 => f.u8(0x08),
					DialogueControl::_09 => f.u8(0x09),
					DialogueControl::_0B => f.u8(0x0B),
					DialogueControl::_0C => f.u8(0x0C),
					DialogueControl::_0F => f.u8(0x0F),
					DialogueControl::_10(v) => { f.u8(0x10); f.u16(*v); },
					DialogueControl::_11(v) => { f.u8(0x11); f.u32(*v); },
					DialogueControl::_12(v) => { f.u8(0x12); f.u32(*v); },
					DialogueControl::_13 => f.u8(0x13),
					DialogueControl::_16 => f.u8(0x16),
					DialogueControl::_17(v) => { f.u8(0x17); f.u16(*v); },
					DialogueControl::_19(v) => { f.u8(0x19); f.u16(*v); },
					DialogueControl::_1A => f.u8(0x1A),
				},
			}
		}
		f.u8(0x00);
		Ok(())
	}
}

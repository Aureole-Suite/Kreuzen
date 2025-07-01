use super::*;

pub fn read(f: &mut Reader, op: &mut Op, code: u8) -> Result<(), OpError> {
	match code {
		0x00 => unreachable!(),
		0x01 => unreachable!(),
		0x02 => {
			op.push(f.u8()?);
			op.push(f.str()?);
			let n = f.u8()?;
			for _ in 0..n {
				op.push(call_arg(f)?);
			}
		}
		0x03 => {
			op.push(Arg::Label(f.u32()?));
		}
		0x05 => {
			op.push(expr(f).context(ExprSnafu)?);
			op.push(Arg::Label(f.u32()?));
		}
		0x06 => {
			op.push(expr(f).context(ExprSnafu)?);
			let n = f.u8()?;
			for _ in 0..n {
				op.push(f.i32()?);
				op.push(Arg::Label(f.u32()?));
			}
			op.push(Arg::Label(f.u32()?));
		}
		0x0A => {
			op.push(f.u8()?);
			op.push(expr(f).context(ExprSnafu)?);
		}
		0x0C => {
			op.push(f.u8()?);
			op.push(f.u8()?);
		}
		0x0E => {
			op.push(f.u8()?);
			op.push(f.u8()?);
		}
		0x10 => {
			op.push(f.u16()?);
		}
		0x16 => {
			op.push(f.u16()?);
		}
		0x17 => {
			op.push(f.u16()?);
			op.push(f.u16()?);
		}
		0x18 => {
			op.push(f.u8()?);
			op.push(expr(f).context(ExprSnafu)?);
		}
		0x1D => {
			op.push(f.u16()?);
			op.push(f.str()?);
			op.push(f.str()?);
			op.push(f.str()?);
			op.push(f.u8()?);
			op.push(f.u32()?);
			op.push(f.u32()?);
			op.push(f.f32()?);
			op.push(f.f32()?);
			op.push(f.f32()?);
			op.push(f.f32()?);
			op.push(f.f32()?);
			op.push(f.f32()?);
			op.push(f.f32()?);
			op.push(f.str()?);
			op.push(f.str()?);
			op.push(f.u32()?);
			op.push(f.u8()?);
			op.push(f.f32()?);
			op.push(f.f32()?);
			op.push(f.u16()?);
		}
		0x1E => {
			op.push(f.u16()?);
			op.push(f.u8()?);
			op.push(f.str()?);
			op.push(f.u8()?);
			let n = f.u8()?;
			for _ in 0..n {
				op.push(call_arg(f)?);
			}
		}
		0x22 => {
			op.push(f.u16()?);
			op.push(dialogue(f).context(DialogueSnafu)?);
		}
		0x23 => match op.sub(f.u8()?) {
			0x05 => {
				op.push(f.u16()?);
				op.push(f.u16()?);
				op.push(f.u16()?);
				op.push(f.u16()?);
				op.push(f.u8()?);
			}
			sub => return UnknownSubSnafu { code, sub }.fail(),
		}
		0x24 => {
			op.push(f.u16()?);
			op.push(f.u32()?);
			op.push(dialogue(f).context(DialogueSnafu)?);
		}
		0x25 => {
			op.push(f.u8()?);
		}
		0x26 => {}
		0x2A => match op.sub(f.u8()?) {
			0 => {
				op.push(f.str()?);
				op.push(f.str()?);
				op.push(f.u8()?);
			}
			sub => return UnknownSubSnafu { code, sub }.fail(),
		}
		0x2C => {
			op.push(f.u16()?);
			op.push(f.str()?);
			op.push(f.u8()?);
			op.push(f.u8()?);
			op.push(f.u8()?);
			op.push(f.u8()?);
			op.push(f.u8()?);
			op.push(f.f32()?);
			op.push(f.f32()?);
			op.push(f.f32()?);
			op.push(f.f32()?);
			op.push(f.u8()?);
			op.push(f.u8()?);
		}
		0x2D => {
			op.push(f.u16()?);
			op.push(f.u32()?);
			op.push(f.u8()?);
		}
		0x2F => match op.sub(f.u8()?) {
			6 | 7 => {
				op.push(f.u16()?);
				op.push(f.u32()?);
			}
			8 => {
				op.push(f.u16()?);
				op.push(f.u8()?);
				op.push(f.str()?);
				op.push(f.str()?);
				op.push(f.str()?);
				op.push(f.str()?);
				op.push(f.str()?);
				op.push(f.str()?);
				op.push(f.str()?);
				op.push(f.str()?);
				op.push(f.str()?);
				op.push(f.str()?);
				op.push(f.str()?);
				op.push(f.str()?);
				op.push(f.str()?);
				op.push(f.str()?);
				op.push(f.str()?);
				op.push(f.str()?);
			}
			sub => return UnknownSubSnafu { code, sub }.fail(),
		}
		0x30 => match op.sub(f.u8()?) {
			0 | 1 => {
				op.push(f.u16()?);
				op.push(f.str()?);
				op.push(f.str()?);
				op.push(f.f32()?);
				op.push(f.f32()?);
				op.push(f.f32()?);
				op.push(f.f32()?);
				op.push(f.f32()?);
				op.push(f.f32()?);
				op.push(f.f32()?);
				op.push(f.f32()?);
				op.push(f.f32()?);
			}
			sub => return UnknownSubSnafu { code, sub }.fail(),
		}
		0x31 => match op.sub(f.u8()?) {
			0 | 1 | 2 | 3 => {
				op.push(f.str()?);
			}
			sub => return UnknownSubSnafu { code, sub }.fail(),
		}
		0x32 => match op.sub(f.u8()?) {
			0x0A => {
				op.push(f.u16()?);
				op.push(f.u8()?);
				op.push(f.str()?);
				op.push(f.u32()?);
			}
			0x0B => {
				op.push(f.u16()?);
				op.push(f.u8()?);
			}
			0x0C => {
				op.push(f.u16()?);
				op.push(call_arg(f)?);
				op.push(f.u16()?);
				op.push(f.u32()?);
				op.push(call_arg(f)?);
				op.push(call_arg(f)?);
				op.push(call_arg(f)?);
				op.push(call_arg(f)?);
				op.push(f.f32()?);
				op.push(f.f32()?);
				op.push(f.f32()?);
				op.push(call_arg(f)?);
				op.push(call_arg(f)?);
				op.push(call_arg(f)?);
				op.push(f.u8()?);
			}
			sub => return UnknownSubSnafu { code, sub }.fail(),
		}
		0x33 => match op.sub(f.u8()?) {
			0x00 => {
				op.push(f.u16()?);
				op.push(f.u16()?);
				op.push(f.u8()?);
				op.push(f.u8()?);
			}
			0x01 => {
				op.push(f.u16()?);
				op.push(call_arg2(f)?);
				op.push(call_arg2(f)?);
				op.push(f.u8()?);
			}
			0x02 => {
				op.push(f.u8()?);
				op.push(f.u16()?);
			}
			0x03 => {
				op.push(f.u16()?);
			}
			0x04 => {
				op.push(f.u8()?);
			}
			0x38 => {}
			0x41 => {
				op.push(f.u16()?);
				op.push(f.u16()?);
				op.push(f.f32()?);
				op.push(f.f32()?);
			}
			0x46 => {
				op.push(f.f32()?);
				op.push(f.f32()?);
				op.push(f.f32()?);
			}
			0x47 => {}
			0x48 => {
				op.push(f.u16()?);
				op.push(f.u16()?);
			}
			0x4B => {
				op.push(f.u16()?);
				op.push(f.u8()?);
			}
			sub => return UnknownSubSnafu { code, sub }.fail(),
		}
		0x35 => match op.sub(f.u8()?) {
			0 | 1 => {
				op.push(f.u16()?);
				op.push(f.u32()?);
			}
			sub => return UnknownSubSnafu { code, sub }.fail(),
		}
		0x36 => match op.sub(f.u8()?) {
			0 => {}
			2 => {
				op.push(f.u8()?);
				op.push(f.f32()?);
				op.push(f.f32()?);
				op.push(f.f32()?);
				op.push(f.u16()?);
			}
			4 => {
				op.push(f.u8()?);
				op.push(f.f32()?);
				op.push(f.f32()?);
				op.push(f.f32()?);
				op.push(f.u16()?);
				op.push(f.u8()?);
			}
			5 => {
				op.push(f.u8()?);
				op.push(f.f32()?);
				op.push(f.u16()?);
			}
			0x07 => {
				op.push(f.u16()?);
			}
			0x0B => {
				op.push(f.u8()?);
				op.push(f.f32()?);
				op.push(f.u16()?);
			}
			0x13 => {
				op.push(f.u16()?);
				op.push(f.str()?);
				op.push(f.u8()?);
				op.push(f.f32()?);
				op.push(f.f32()?);
				op.push(f.f32()?);
				op.push(f.u16()?);
				op.push(f.u8()?);
			}
			0x14 => {
				op.push(f.u8()?);
				op.push(f.u16()?);
				op.push(f.str()?);
				op.push(f.f32()?);
				op.push(f.f32()?);
				op.push(f.f32()?);
				op.push(f.u16()?);
			}
			sub => return UnknownSubSnafu { code, sub }.fail(),
		}
		0x37 => {
			op.push(f.u16()?);
			op.push(f.f32()?);
			op.push(f.f32()?);
			op.push(f.f32()?);
			op.push(f.f32()?);
		}
		0x38 => {
			op.push(f.u16()?);
			op.push(f.u8()?);
			op.push(f.u8()?);
			op.push(f.str()?);
		}
		0x39 => {
			op.push(f.u16()?);
			op.push(f.u8()?);
			op.push(f.str()?);
			op.push(f.u32()?);
			op.push(f.f32()?);
			op.push(f.u32()?);
		}
		0x3A => match op.sub(f.u8()?) {
			sub => return UnknownSubSnafu { code, sub }.fail(),
		}
		0x3B => match op.sub(f.u8()?) {
			0 | 0x32 => {
				op.push(call_arg2(f)?);
				op.push(call_arg2(f)?);
				op.push(call_arg2(f)?);
				op.push(f.f32()?);
				op.push(call_arg2(f)?);
				op.push(f.u16()?);
				op.push(f.u16()?);
				op.push(f.f32()?);
				op.push(f.f32()?);
				op.push(f.f32()?);
				op.push(call_arg2(f)?);
				op.push(f.str()?);
				op.push(call_arg2(f)?);
				op.push(call_arg2(f)?);
				op.push(f.u16()?);
				op.push(f.u16()?);
				op.push(f.u16()?);
				op.push(f.u16()?);
				op.push(f.u16()?);
				op.push(f.u16()?);
				op.push(f.f32()?);
			}
			0x3A => {
				op.push(f.u16()?);
				op.push(call_arg2(f)?);
				op.push(call_arg2(f)?);
				op.push(call_arg2(f)?);
				op.push(call_arg2(f)?);
			}
			0x3E | 0x3F => {
				op.push(f.u16()?);
			}
			sub => return UnknownSubSnafu { code, sub }.fail(),
		}
		0x3C => match op.sub(f.u8()?) {
			1 => {
				op.push(f.u16()?);
				op.push(f.str()?);
				op.push(f.str()?);
			}
			3 => {
				op.push(f.u16()?);
				op.push(f.str()?);
				op.push(f.str()?);
				op.push(f.str()?);
				op.push(f.str()?);
				op.push(f.str()?);
			}
			4 => {
				op.push(f.u16()?);
				op.push(f.str()?);
			}
			5 => {
				op.push(f.u16()?);
				op.push(f.str()?);
				op.push(f.str()?);
				op.push(f.str()?);
				op.push(f.str()?);
				op.push(f.str()?);
			}
			sub => return UnknownSubSnafu { code, sub }.fail(),
		}
		0x3D => {
			op.push(f.u16()?);
			op.push(f.f32()?);
			op.push(f.f32()?);
			op.push(f.u8()?);
		}
		0x43 => match op.sub(f.u8()?) {
			0x00 | 0x64 | 0x65 => {
				op.push(f.u16()?);
				op.push(f.f32()?);
				op.push(f.u16()?);
			}
			0xFE => {
				op.push(f.u16()?);
			}
			0xFF => {
				op.push(f.u16()?);
				op.push(f.u16()?);
			}
			sub => return UnknownSubSnafu { code, sub }.fail(),
		}
		0x44 => {
			op.push(f.u16()?);
			op.push(f.u8()?);
			op.push(f.f32()?);
			op.push(f.u16()?);
			op.push(f.f32()?);
		}
		0x45 => {
			op.push(f.u16()?);
			op.push(f.f32()?);
			op.push(f.f32()?);
			op.push(f.f32()?);
			op.push(f.u16()?);
			op.push(f.u16()?);
		}
		0x46 => match op.sub(f.u8()?) {
			0 => {
				op.push(f.u16()?);
				op.push(f.u16()?);
				op.push(f.u16()?);
			}
			2 => {
				op.push(f.u16()?);
				op.push(f.u16()?);
				op.push(f.u16()?);
			}
			sub => return UnknownSubSnafu { code, sub }.fail(),
		}
		0x49 => match op.sub(f.u8()?) {
			0x06 => {
				op.push(f.u16()?);
			}
			sub => return UnknownSubSnafu { code, sub }.fail(),
		}
		0x4B => {
			op.push(f.u16()?);
			op.push(f.f32()?);
			op.push(f.f32()?);
			op.push(f.f32()?);
			op.push(f.f32()?);
			op.push(f.u16()?);
			op.push(f.u8()?);
		}
		0x53 => match op.sub(f.u8()?) {
			sub => return UnknownSubSnafu { code, sub }.fail(),
		}
		0x54 => match op.sub(f.u8()?) {
			0x0B => {
				op.push(f.u16()?);
			}
			sub => return UnknownSubSnafu { code, sub }.fail(),
		}
		0x55 => {
			op.push(f.u8()?);
			op.push(f.u16()?);
			op.push(f.u16()?);
			op.push(f.u16()?);
			op.push(f.u16()?);
			op.push(f.u16()?);
			op.push(f.u16()?);
			op.push(f.u16()?);
			op.push(f.u16()?);
			op.push(f.u16()?);
			op.push(f.u16()?);
			op.push(f.f32()?);
			op.push(f.f32()?);
			op.push(f.f32()?);
			op.push(f.f32()?);
			op.push(f.u8()?);
			op.push(f.u8()?);
			op.push(f.str()?);
			op.push(f.str()?);
		}
		0x68 => match op.sub(f.u8()?) {
			sub => return UnknownSubSnafu { code, sub }.fail(),
		}
		0x6C => {
			op.push(f.u16()?);
			op.push(f.f32()?);
			op.push(f.i32()?);
		}
		0x76 => {
			op.push(f.u16()?);
			op.push(f.str()?);
			op.push(f.str()?);
			op.push(f.u8()?);
			op.push(f.u8()?);
			op.push(f.f32()?);
			op.push(f.f32()?);
			op.push(f.f32()?);
			op.push(f.f32()?);
		}
		0x7A => match op.sub(f.u8()?) {
			0 => {
				op.push(f.str()?);
			}
			1 | 2 => {
				op.push(f.u16()?);
				op.push(f.str()?);
			}
			3 => {
				op.push(f.str()?);
			}
			sub => return UnknownSubSnafu { code, sub }.fail(),
		}
		0x7E => match op.sub(f.u8()?) {
			0 => {}
			2 => {
				op.push(f.u16()?);
			}
			5 => {
				op.push(f.f32()?);
			}
			6 => {
				op.push(f.u16()?);
			}
			sub => return UnknownSubSnafu { code, sub }.fail(),
		}
		0x80 => {
			op.push(f.f32()?);
		}
		0x8A => match op.sub(f.u8()?) {
			sub => return UnknownSubSnafu { code, sub }.fail(),
		}
		0x91 => match op.sub(f.u8()?) {
			0 => {
				op.push(f.u16()?);
				op.push(f.u32()?);
			}
			sub => return UnknownSubSnafu { code, sub }.fail(),
		}
		0xAC => match op.sub(f.u8()?) {
			0x00 => {
				op.push(f.u8()?);
			}
			0x07 => {
				op.push(f.u16()?);
			}
			0x0D => {
				op.push(call_arg2(f)?);
			}
			sub => return UnknownSubSnafu { code, sub }.fail(),
		}
		0xAE => {
			op.push(f.str()?);
			op.push(f.u16()?);
		}
		0xB5 => {
			op.push(f.u16()?);
			op.push(f.u8()?);
			op.push(f.f32()?);
			op.push(f.f32()?);
			op.push(f.f32()?);
			op.push(f.u16()?);
			op.push(f.u16()?);
		}
		0xBB => {
			op.push(f.u16()?);
		}
		0xC8 => match op.sub(f.u8()?) {
			1 => {
				op.push(f.u16()?);
				op.push(f.u32()?);
			}
			2 => {
				op.push(f.u16()?);
				op.push(f.u32()?);
				op.push(f.u16()?);
			}
			sub => return UnknownSubSnafu { code, sub }.fail(),
		}
		0xC9 => match op.sub(f.u8()?) {
			0x01 => {
				op.push(f.u16()?);
			}
			sub => return UnknownSubSnafu { code, sub }.fail(),
		}
		0xFF => match op.sub(f.u8()?) {
			sub => return UnknownSubSnafu { code, sub }.fail(),
		}
		code => return UnknownOpSnafu { code }.fail(),
	}
	Ok(())
}

use kreuzen::code::{Arg, Op, OpMeta};
use kreuzen::spec::Part;
use kreuzen::types::*;

use super::alt::Alt;
use super::parser::{Error, Expect, Parser, Result};
use super::types::Parse;
use super::{PCtx, expr, types};

/// Optional `<line>@` and `<width>~`/`~` markers. Never fails; consumes nothing if absent.
impl Parse for OpMeta {
	fn parse(p: &mut Parser) -> Result<Self> {
		let mut meta = OpMeta::default();
		if let Ok(line) = p.test(Expect::Nt("line"), |p| {
			let span = p.cursor.next_span();
			let line = p.cursor.int()?;
			p.cursor.glued_punct('@')?;
			u16::try_from(line).map_err(|_| {
				p.errors.error("line number out of range", span);
				Error
			})
		}) {
			meta.line = line;
		}

		if let Ok(width) = p.test(Expect::Nt("width"), |p| {
			let span = p.cursor.next_span();
			let width = p.cursor.int()?;
			p.cursor.glued_punct('~')?;
			u8::try_from(width).map_err(|_| {
				p.errors.error("width out of range", span);
				Error
			})
		}) {
			meta.width = width;
		} else if p.punct('~').is_ok() {
			meta.width = 1;
		}
		Ok(meta)
	}
}

/// Like OpMeta's `Parse` impl, but fails (without consuming) if no marker is present.
pub fn parse_meta_present(p: &mut Parser) -> Result<OpMeta> {
	let meta: OpMeta = p.parse()?;
	if meta == OpMeta::default() { Err(Error) } else { Ok(meta) }
}

// Pseudo-ops created by kreuzen::sugar, which don't exist in the spec.
const SUGAR_OPS: &[(&str, &[Part])] = &[("CallShadow", &[Part::U16])];

/// An op whose name has not been consumed yet, with the given (already parsed) meta.
pub fn parse_op_named(p: &mut Parser, ctx: &PCtx, meta: OpMeta) -> Result<Op> {
	let span = p.next_span();
	let name = p.ident()?;

	if let Some((name, parts)) = SUGAR_OPS.iter().find(|(n, _)| *n == name) {
		let mut op = Op { name, meta, args: Vec::new() };
		parse_parts(p, ctx, parts, &mut op)?;
		return Ok(op);
	}

	let Some(opcode) = ctx.spec.by_name.get(name) else {
		p.errors.error(format!("unknown op '{name}'"), span);
		return Err(Error);
	};

	let mut op_spec = ctx.spec.ops[opcode[0] as usize].as_ref().ok_or(Error)?;
	let mut op = Op {
		name: op_spec.name.as_str(),
		meta,
		args: Vec::new(),
	};
	parse_parts(p, ctx, &op_spec.parts, &mut op)?;
	for &byte in &opcode[1..] {
		op_spec = op_spec.child(byte).ok_or(Error)?;
		op.name = op_spec.name.as_str();
		parse_parts(p, ctx, &op_spec.parts, &mut op)?;
	}
	Ok(op)
}

// The text analogue of read_parts in kreuzen::code.
fn parse_parts(p: &mut Parser, ctx: &PCtx, parts: &[Part], op: &mut Op) -> Result<()> {
	use Part as P;
	for part in parts {
		match part {
			P::U8 => op.args.push(Arg::Int(p.parse::<u8>()? as i64)),
			P::U16 => op.args.push(Arg::Int(p.parse::<u16>()? as i64)),
			P::U32 => op.args.push(Arg::Int(p.parse::<u32>()? as i64)),
			P::I8 => op.args.push(Arg::Int(p.parse::<i8>()? as i64)),
			P::I16 => op.args.push(Arg::Int(p.parse::<i16>()? as i64)),
			P::I32 => op.args.push(Arg::Int(p.parse::<i32>()? as i64)),
			P::F32 => op.args.push(parse_f32_arg(p)?),
			P::Pos => parse_parts(p, ctx, &[P::F32, P::F32, P::F32], op)?,
			P::Str => op.args.push(Arg::Str(p.parse()?)),

			P::Char => op.args.push(Arg::Char(p.parse()?)),
			P::Item => op.args.push(Arg::Item(p.parse()?)),
			P::Battle => op.args.push(types::battle_arg(p)?),
			P::Magic => op.args.push(Arg::Magic(p.parse()?)),
			P::Sound => op.args.push(Arg::Sound(p.parse()?)),
			P::Music => op.args.push(Arg::Music(p.parse()?)),
			P::Flag => op.args.push(Arg::Flag(p.parse()?)),
			P::Global => op.args.push(Arg::Global(p.parse()?)),
			P::Var => op.args.push(Arg::Var(p.parse()?)),
			P::FuncArg => op.args.push(Arg::FuncArg(p.parse()?)),
			P::NumReg => op.args.push(Arg::NumReg(p.parse()?)),
			P::StrReg => op.args.push(Arg::StrReg(p.parse()?)),
			P::Attr => op.args.push(Arg::Attr(p.parse()?)),
			P::CharAttr => op.args.push(Arg::CharAttr(p.parse()?)),

			P::Flags8 => op.args.push(Arg::Flags8(p.parse()?)),
			P::Flags16 => op.args.push(Arg::Flags16(p.parse()?)),
			P::Flags32 => op.args.push(Arg::Flags32(p.parse()?)),
			P::SystemFlags => op.args.push(Arg::SystemFlags(p.parse()?)),

			P::Expr => op.args.push(Arg::Expr(expr::parse_expr(p, ctx)?)),
			P::Text => op.args.push(Arg::Text(p.parse()?)),

			P::Dyn => op.args.push(parse_dyn(p)?),
			P::Ndyn => {
				while let Ok(arg) = parse_dyn(p) {
					op.args.push(arg);
				}
			}
			P::Dyn_Char => op.args.push(match p.parse::<Char>() {
				Ok(c) => Arg::Char(c),
				Err(_) => parse_dyn(p)?,
			}),
			P::Dyn_Sound => op.args.push(match p.parse::<Sound>() {
				Ok(s) => Arg::Sound(s),
				Err(_) => parse_dyn(p)?,
			}),

			P::Cs1_36 | P::Cs1_3C | P::Cs2_37 | P::Tx_3C | P::Cs3_98 | P::Cs3_c0 | P::Cs4_40 | P::Rev_79 | P::Rev_D2 | P::Rev_E002 => {
				let extra = kreuzen::code::extra_parts(part, &op.args, ctx.game).map_err(|e| {
					p.errors.error(format!("{e}"), p.next_span());
					Error
				})?;
				parse_parts(p, ctx, extra, op)?;
			}

			P::Tx_isforceload => {
				if let Ok(s) = p.parse() {
					op.args.push(Arg::Str(s));
				} else {
					parse_parts(p, ctx, &[P::U8], op)?;
				}
			}
			P::Tx_2F => {
				if let Ok(s) = p.parse() {
					op.args.push(Arg::Str(s));
					op.args.push(Arg::Int(p.parse()?));
					parse_parts(p, ctx, &[P::U8, P::F32, P::F32, P::F32], op)?;
					break;
				}
			}
			P::Cs4_wtf_are_you_doing => {
				let zero = p.test(Expect::Nt("0"), |p| if p.cursor.int()? == 0 { Ok(()) } else { Err(Error) });
				if zero.is_ok() {
					op.args.push(Arg::Int(0));
				}
			}
			P::Rev_3E => match op.args.get(1) {
				Some(Arg::Char(Char(0xFE12))) => parse_parts(p, ctx, &[P::U8], op)?,
				Some(Arg::Char(Char(0xFE13))) => parse_parts(p, ctx, &[P::F32], op)?,
				Some(Arg::Char(Char(0xFFFF))) => {
					// btlcom omits these, so presence decides
					if p.cursor.clone().int().is_ok() {
						parse_parts(p, ctx, &[P::U8, P::U8, P::U8], op)?;
					}
				}
				_ => {}
			},

			P::Print => {}
			P::Fail => {
				p.errors.error(format!("op '{}' cannot be encoded", op.name), p.prev_span());
				return Err(Error);
			}
		}
	}
	Ok(())
}

/// A float-typed arg: either a float literal, or a munged int (`123'`).
fn parse_f32_arg(p: &mut Parser) -> Result<Arg> {
	Alt::new(p)
		.test(|p| p.parse().map(Arg::F32))
		.test(|p| {
			let v = p.parse()?;
			p.glued_punct('\'')?;
			Ok(Arg::F32Munged(v))
		})
		.finish()
}

// The text analogue of read_dyn in kreuzen::code.
fn parse_dyn(p: &mut Parser) -> Result<Arg> {
	Alt::new(p)
		.test(|p| p.parse().map(Arg::Var))
		.test(|p| p.parse().map(Arg::NumReg))
		.test(|p| p.parse().map(Arg::StrReg))
		.test(|p| p.parse().map(Arg::Global))
		.test(|p| p.parse().map(Arg::Str))
		// I32Munged prints as a float with a trailing '
		.test(|p| {
			let v = p.parse()?;
			if p.glued_punct('\'').is_ok() {
				Ok(Arg::I32Munged(v))
			} else {
				Ok(Arg::F32(v))
			}
		})
		.test(|p| Ok(Arg::Int(p.parse::<i32>()? as i64)))
		.finish()
}

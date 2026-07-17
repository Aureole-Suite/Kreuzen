use kreuzen::code::Arg;

use super::parser::{Parser, Result};

/// `name[...]` where the contents are parsed by `f`.
pub fn bracket<T>(p: &mut Parser, name: &'static str, f: impl FnOnce(&mut Parser) -> Result<T>) -> Result<T> {
	p.cursor.keyword(name)?;
	p.delim('[', f)
}

/// `btlset[N]:battle[M]`, the syntax for `Arg::Battle`.
pub fn battle_arg(p: &mut Parser) -> Result<Arg> {
	let a = bracket(p, "btlset", |p| p.parse())?;
	p.glued_punct(':')?;
	let b = p.parse()?;
	Ok(Arg::Battle(a, b))
}

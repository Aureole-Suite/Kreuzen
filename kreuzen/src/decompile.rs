use std::collections::BTreeMap;

use rootcause::option_ext::OptionExt as _;
use rootcause::prelude::ResultExt as _;

use crate::types::Char;
use crate::code::{Expr, FlatOp, Label, Op, OpMeta};

#[derive(Clone, PartialEq)]
pub enum Stmt {
	Op(Op),
	If(OpMeta, Expr, Vec<Stmt>, Option<(OpMeta, Vec<Stmt>)>),
	While(OpMeta, Expr, Vec<Stmt>, OpMeta),
	Break(OpMeta),
	Continue(OpMeta),
	Switch(OpMeta, Expr, Vec<(Case, Vec<Stmt>)>),
	ForkLambda(OpMeta, Char, u8, Vec<Stmt>),
}

impl std::fmt::Debug for Stmt {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Op(o) => o.fmt(f),
			Self::If(m, e, then, els) => {
				m.fmt(f)?;
				let mut tup = f.debug_tuple("If");
				tup.field(e);
				tup.field(then);
				tup.finish()?;
				if let Some((m2, els)) = els {
					f.write_str(" ")?;
					m2.fmt(f)?;
					f.write_str("else ")?;
					if let [stmt@Stmt::If(..)] = els.as_slice() {
						stmt.fmt(f)?;
					} else {
						els.fmt(f)?;
					}
				}
				Ok(())
			},
			Self::While(m, a, b, c) => m.fmt(f)?.debug_tuple("While").field(a).field(b).field(c).finish(),
			Self::Break(m) => m.fmt(f)?.debug_tuple("Break").finish(),
			Self::Continue(m) => m.fmt(f)?.debug_tuple("Continue").finish(),
			Self::Switch(m, a, b) => m.fmt(f)?.debug_tuple("Switch").field(a).field(b).finish(),
			Self::ForkLambda(m, a, b, c) => m.fmt(f)?.debug_tuple("ForkLambda").field(a).field(b).field(c).finish(),
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Case {
	Default,
	Case(i32),
	// There's a freaky switch in c0400:ronald_setting that has a switch with bodies but no cases.
	None,
}

pub fn decompile(stmts: &[FlatOp]) -> rootcause::Result<Vec<Stmt>> {
	let mut labels = BTreeMap::new();
	for (i, stmt) in stmts.iter().enumerate() {
		if let FlatOp::Label(l) = stmt {
			labels.insert(*l, i);
		}
	}

	let (body, _) = Ctx::new(&Gctx { stmts, labels }).block("body", GotoAllowed::No)?;
	Ok(body)
}

struct Gctx<'a> {
	stmts: &'a [FlatOp],
	labels: BTreeMap<Label, usize>,
}

impl Gctx<'_> {
	#[track_caller]
	fn lookup(&self, label: Label) -> rootcause::Result<usize> {
		Ok(self.labels.get(&label).copied().context_with(|| format!("undefined label: {label}"))?)
	}
}

struct Ctx<'a> {
	gctx: &'a Gctx<'a>,
	pos: usize,
	end: usize,
	brk: Option<Label>,
	cont: Option<Label>,
}

impl<'a> Ctx<'a> {
	fn new(gctx: &'a Gctx) -> Self {
		Self {
			gctx,
			pos: 0,
			end: gctx.stmts.len(),
			brk: None,
			cont: None,
		}
	}

	fn next(&mut self) -> Option<&'a FlatOp> {
		if self.pos == self.end {
			None
		} else {
			let stmt = &self.gctx.stmts[self.pos];
			self.pos += 1;
			Some(stmt)
		}
	}

	#[track_caller]
	fn lookup(&self, label: Label) -> rootcause::Result<usize> {
		let pos = self.gctx.lookup(label)?;
		if !(self.pos..=self.end).contains(&pos) {
			rootcause::bail!("label {label} at position {pos} is out of bounds");
		};
		Ok(pos)
	}

	#[track_caller]
	fn sub(&mut self, label: Label) -> rootcause::Result<Ctx<'a>> {
		let pos = self.lookup(label)?;
		let sub = Self {
			end: pos,
			..*self
		};
		self.pos = pos;
		Ok(sub)
	}

	#[track_caller]
	fn goto_before(&self, label: Label) -> rootcause::Result<Option<(OpMeta, Label)>> {
		let pos = self.lookup(label)?;
		if pos > 0 && let FlatOp::Goto(meta, cont) = self.gctx.stmts[pos - 1] {
			Ok(Some((meta, cont)))
		} else {
			Ok(None)
		}
	}

	fn block(&mut self, what: &'static str, goto_allowed: GotoAllowed) -> rootcause::Result<BlockValue> {
		let start = self.pos;
		let end = self.end;
		Ok(block(self, goto_allowed)
			.context_with(|| format!("while parsing {what} block at {start}..{end}"))?)
	}
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum GotoAllowed {
	Anywhere,
	Yes,
	No,
}

type BlockValue = (Vec<Stmt>, Option<(OpMeta, Label)>);

fn block(ctx: &mut Ctx, goto_allowed: GotoAllowed) -> rootcause::Result<BlockValue> {
	let mut stmts = Vec::new();
	while let Some(stmt) = ctx.next() {
		match stmt {
			FlatOp::Op(o) => stmts.push(Stmt::Op(o.clone())),
			FlatOp::Label(_) => {}
			FlatOp::If(m, e, label) => {
				let start = ctx.pos - 1;
				let end = ctx.end;
				parse_if(&mut stmts, ctx, *m, e.clone(), *label)
					.context_with(|| format!("while parsing if statement at {start}..{end}"))?;
			},
			FlatOp::Switch(m, e, cases, default) => {
				let start = ctx.pos - 1;
				let end = ctx.end;
				parse_switch(&mut stmts, ctx, *m, e.clone(), cases, *default)
					.context_with(|| format!("while parsing switch statement at {start}..{end}"))?;
			},

			FlatOp::Goto(m, l) => {
				let ok = match goto_allowed {
					GotoAllowed::Anywhere => true,
					GotoAllowed::Yes => ctx.pos == ctx.end,
					GotoAllowed::No => false,
				};
				if Some(*l) == ctx.brk {
					stmts.push(Stmt::Break(*m))
				} else if Some(*l) == ctx.cont {
					stmts.push(Stmt::Continue(*m))
				} else if ok {
					return Ok((stmts, Some((*m, *l))))
				} else {
					rootcause::bail!("unexpected goto to {l} at position {}", ctx.pos - 1);
				}
			}
		}
	}
	Ok((stmts, None))
}

fn parse_if(
	stmts: &mut Vec<Stmt>,
	ctx: &mut Ctx,
	l: OpMeta,
	e: Expr,
	label: Label,
) -> rootcause::Result<()> {
	if let Some((m, cont)) = ctx.goto_before(label)?
			&& ctx.pos >= 2
			&& ctx.gctx.lookup(cont)? == ctx.pos - 2 {
		let mut sub = ctx.sub(label)?;
		sub.brk = Some(label);
		sub.cont = Some(cont);
		let (mut body, _) = sub.block("while body", GotoAllowed::No)?;
		assert_eq!(body.pop(), Some(Stmt::Continue(m)));
		stmts.push(Stmt::While(l, e, body, m));
		return Ok(())
	}

	let (body, goto) = ctx.sub(label)?.block("if body", GotoAllowed::Yes)?;

	if let Some((m, goto)) = goto {
		let (no, _) = ctx.sub(goto)?.block("else body", GotoAllowed::No)?;
		stmts.push(Stmt::If(l, e, body, Some((m, no))));
	} else {
		stmts.push(Stmt::If(l, e, body, None));
	};
	Ok(())
}

fn parse_switch(
	stmts: &mut Vec<Stmt>,
	ctx: &mut Ctx,
	l: OpMeta,
	e: Expr,
	cases: &[(i32, Label)],
	default: Label,
) -> rootcause::Result<()> {
	let pos = cases.iter().map(|(_, l)| ctx.lookup(*l)).collect::<rootcause::Result<Vec<_>, _>>()?;
	if !pos.is_sorted() {
		rootcause::bail!("switch cases are not in order: {:?}", pos);
	}
	let default_pos = ctx.lookup(default)?;
	let default_index = pos.partition_point(|&p| p < default_pos);
	let mut cases = cases.iter().map(|(k, l)| (Case::Case(*k), *l)).collect::<Vec<_>>();
	cases.insert(default_index, (Case::Default, default));

	let last_pos = pos.last().copied().max(Some(default_pos)).unwrap();
	let mut brk = None;
	for stmt in &ctx.gctx.stmts[ctx.pos..last_pos] {
		if let FlatOp::Goto(_, goto) = stmt
		&& ctx.lookup(*goto)? >= last_pos
		{
			brk = brk.max(Some(*goto));
		}
	}

	let mut cases2 = Vec::with_capacity(cases.len());

	let mut pre = ctx.sub(cases.first().unwrap().1)?;
	pre.brk = brk;
	if pre.pos != pre.end {
		let (body, _) = pre.block("switch pre-body", GotoAllowed::No)?;
		cases2.push((Case::None, body));
	}

	let ends = cases.iter().skip(1).map(|i| i.1).chain(brk);
	let mut has_brk = false;
	for (&(key, target), end) in std::iter::zip(&cases, ends) {
		let target = ctx.gctx.lookup(target)?;
		assert_eq!(ctx.pos, target);
		let mut sub = ctx.sub(end)?;
		sub.brk = brk;
		let (body, _) = sub.block("switch body", GotoAllowed::No)?;
		if body.last().is_some_and(|s| matches!(s, Stmt::Break(_))) {
			has_brk = true;
		}
		cases2.push((key, body));
	}

	if has_brk {
		// we know where the break is, so no need for that bullshit
		if cases2.last().is_some_and(|(k, v)| *k == Case::Default && v.is_empty()) {
			cases2.pop();
		}
		stmts.push(Stmt::Switch(l, e, cases2));
		return Ok(());
	}

	let last = cases.last().unwrap();
	assert_eq!(ctx.gctx.lookup(last.1)?, ctx.pos);
	let prev_brk = ctx.brk.take();
	let (mut body, goto) = ctx.block("switch last body", GotoAllowed::Anywhere)?;
	ctx.brk = prev_brk;
	if let Some((m, goto)) = goto {
		if ctx.gctx.lookup(goto)? == ctx.pos {
			// finally found a break, but it's useless
			body.push(Stmt::Break(m));
			cases2.push((last.0, body));
			stmts.push(Stmt::Switch(l, e, cases2));
			return Ok(());
		} else {
			// this goto probably belongs to a parent scope. Rewind and let the parent handle it.
			ctx.pos -= 1;
		}
	}
	// got to end of block without a break, so assume the last case is empty
	if last.0 != Case::Default {
		cases2.push((last.0, Vec::new()));
	}
	stmts.push(Stmt::Switch(l, e, cases2));
	stmts.extend(body);

	Ok(())
}

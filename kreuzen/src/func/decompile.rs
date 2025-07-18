use std::collections::BTreeMap;

use snafu::{OptionExt as _, ResultExt as _};

use super::{Op, OpMeta, Expr, FlatOp, Label};

#[derive(Debug, snafu::Snafu)]
#[snafu(module(decompile), context(suffix(false)))]
pub enum DecompileError {
	#[snafu(display("unknown label: {label:?} ({location})"))]
	MissingLabel {
		label: Label,
		#[snafu(implicit)]
		location: snafu::Location,
	},
	#[snafu(display("label out of bounds: {label:?} at {pos} should be in {start}..={end} ({location})"))]
	WrongLabel {
		label: Label,
		pos: usize,
		start: usize,
		end: usize,
		#[snafu(implicit)]
		location: snafu::Location,
	},
	#[snafu(display("unsorted switch"))]
	UnsortedSwitch,
	#[snafu(display("unexpected jump to {label:?}"))]
	UnexpectedJump { label: Label },

	#[snafu(display("while parsing {what} at {start}..{end}"))]
	Block {
		what: &'static str,
		start: usize,
		end: usize,
		#[snafu(source(from(DecompileError, Box::new)))]
		source: Box<DecompileError>,
	},
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
	Op(Op),
	If(OpMeta, Expr, Vec<Stmt>, Option<Vec<Stmt>>),
	While(OpMeta, Expr, Vec<Stmt>),
	Break,
	Continue,
	Switch(OpMeta, Expr, Vec<(Case, Vec<Stmt>)>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Case {
	Default,
	Case(i32),
	// There's a freaky switch in c0400:ronald_setting that has a switch with bodies but no cases.
	None,
}

pub fn decompile(stmts: &[FlatOp]) -> Result<Vec<Stmt>, DecompileError> {
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
	fn lookup(&self, label: Label) -> Result<usize, DecompileError> {
		self.labels.get(&label).copied().context(decompile::MissingLabel { label })
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
	fn lookup(&self, label: Label) -> Result<usize, DecompileError> {
		let pos = self.gctx.lookup(label)?;
		snafu::ensure!((self.pos..=self.end).contains(&pos), decompile::WrongLabel {
			label,
			pos,
			start: self.pos,
			end: self.end,
		});
		Ok(pos)
	}

	#[track_caller]
	fn sub(&mut self, label: Label) -> Result<Ctx<'a>, DecompileError> {
		let pos = self.lookup(label)?;
		let sub = Self {
			end: pos,
			..*self
		};
		self.pos = pos;
		Ok(sub)
	}

	#[track_caller]
	fn goto_before(&self, label: Label) -> Result<Option<Label>, DecompileError> {
		let pos = self.lookup(label)?;
		if pos > 0 && let FlatOp::Goto(meta, cont) = self.gctx.stmts[pos - 1] {
			Ok(Some(cont))
		} else {
			Ok(None)
		}
	}

	fn block(&mut self, what: &'static str, goto_allowed: GotoAllowed) -> Result<(Vec<Stmt>, Option<Label>), DecompileError> {
		let start = self.pos;
		let end = self.end;
		block(self, goto_allowed).context(decompile::Block { what, start, end })
	}
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum GotoAllowed {
	Anywhere,
	Yes,
	No,
}

fn block(ctx: &mut Ctx, goto_allowed: GotoAllowed) -> Result<(Vec<Stmt>, Option<Label>), DecompileError> {
	let mut stmts = Vec::new();
	while let Some(stmt) = ctx.next() {
		match stmt {
			FlatOp::Op(o) => stmts.push(Stmt::Op(o.clone())),
			FlatOp::Label(_) => {}
			FlatOp::If(m, e, label) => {
				let start = ctx.pos - 1;
				let end = ctx.end;
				parse_if(&mut stmts, ctx, *m, e.clone(), *label)
					.context(decompile::Block { what: "if", start, end })?
			},
			FlatOp::Switch(m, e, cases, default) => {
				let start = ctx.pos - 1;
				let end = ctx.end;
				parse_switch(&mut stmts, ctx, *m, e.clone(), cases, *default)
					.context(decompile::Block { what: "switch", start, end })?
			},

			FlatOp::Goto(m, l) => {
				let ok = match goto_allowed {
					GotoAllowed::Anywhere => true,
					GotoAllowed::Yes => ctx.pos == ctx.end,
					GotoAllowed::No => false,
				};
				if Some(*l) == ctx.brk {
					stmts.push(Stmt::Break)
				} else if Some(*l) == ctx.cont {
					stmts.push(Stmt::Continue)
				} else if ok {
					return Ok((stmts, Some(*l)))
				} else {
					return decompile::UnexpectedJump { label: *l }.fail()
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
) -> Result<(), DecompileError> {
	if let Some(cont) = ctx.goto_before(label)?
			&& ctx.pos >= 2
			&& ctx.gctx.lookup(cont)? == ctx.pos - 2 {
		let mut sub = ctx.sub(label)?;
		sub.brk = Some(label);
		sub.cont = Some(cont);
		let (mut body, _) = sub.block("while body", GotoAllowed::No)?;
		assert_eq!(body.pop(), Some(Stmt::Continue));
		stmts.push(Stmt::While(l, e, body));
		return Ok(())
	}

	let (body, goto) = ctx.sub(label)?.block("if body", GotoAllowed::Yes)?;

	if let Some(goto) = goto {
		let (no, _) = ctx.sub(goto)?.block("else body", GotoAllowed::No)?;
		stmts.push(Stmt::If(l, e, body, Some(no)));
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
) -> Result<(), DecompileError> {
	let pos = cases.iter().map(|(_, l)| ctx.lookup(*l)).collect::<Result<Vec<_>, _>>()?;
	snafu::ensure!(pos.is_sorted(), decompile::UnsortedSwitch);
	let default_pos = ctx.lookup(default)?;
	let default_index = pos.partition_point(|&p| p < default_pos);
	let mut cases = cases.iter().map(|(k, l)| (Case::Case(*k), *l)).collect::<Vec<_>>();
	cases.insert(default_index, (Case::Default, default));

	let mut brk = None;
	let mut brk_pos = 0;
	for (_, l) in &cases {
		if let Some(goto) = ctx.goto_before(*l)?
			&& let goto_pos = ctx.lookup(goto)?
			&& goto_pos >= ctx.lookup(*l)?
			&& goto_pos > brk_pos
		{
			brk = Some(goto);
			brk_pos = goto_pos;
		}
	}

	let mut cases2 = Vec::with_capacity(cases.len());

	let mut pre = ctx.sub(cases.first().unwrap().1)?;
	while pre.pos != pre.end {
		let (mut body, jump) = pre.block("switch pre-body", GotoAllowed::Anywhere)?;
		snafu::ensure!(jump == Some(default), decompile::UnsortedSwitch);
		body.push(Stmt::Break);
		cases2.push((Case::None, body));
	}

	let ends = cases.iter().skip(1).map(|i| i.1).chain(brk);
	for (&(key, target), end) in std::iter::zip(&cases, ends) {
		let target = ctx.gctx.lookup(target)?;
		assert_eq!(ctx.pos, target);
		let mut sub = ctx.sub(end)?;
		sub.brk = brk;
		let (body, _) = sub.block("switch body", GotoAllowed::No)?;
		cases2.push((key, body));
	}

	if brk.is_some() {
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
	if let Some(goto) = goto {
		if ctx.gctx.lookup(goto)? == ctx.pos {
			// finally found a break, but it's useless
			body.push(Stmt::Break);
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

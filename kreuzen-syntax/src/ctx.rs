use std::borrow::Cow;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default)]
enum Space {
	#[default]
	None,
	Inline,
	Block(usize),
}

pub struct Ctx {
	out: String,
	space: Space,
	pub indent: usize,
}

impl Ctx {
	#[expect(clippy::new_without_default)]
	pub fn new() -> Self {
		Self { out: String::new(), space: Space::None, indent: 0 }
	}

	pub fn token(&mut self, word: impl Into<Cow<'static, str>>) {
		self.do_space(true);
		self.out.push_str(&word.into());
		self.set_space(Space::Inline);
	}

	pub fn word(&mut self, word: &'static str) {
		self.token(word)
	}

	pub fn sym(&mut self, sym: &'static str) {
		self.do_space(false);
		self.out.push_str(sym);
		self.set_space(Space::None);
	}

	pub fn _sym(&mut self, sym: &'static str) {
		self.do_space(true);
		self.out.push_str(sym);
		self.set_space(Space::None);
	}

	pub fn sym_(&mut self, sym: &'static str) {
		self.do_space(false);
		self.out.push_str(sym);
		self.set_space(Space::Inline);
	}

	pub fn _sym_(&mut self, sym: &'static str) {
		self.do_space(true);
		self.out.push_str(sym);
		self.set_space(Space::Inline);
	}

	fn do_space(&mut self, inline: bool) {
		if self.out.is_empty() {
			self.space = Space::None;
			return;
		}
		match self.space {
			Space::None => {}
			Space::Inline => {
				if inline {
					self.out.push(' ');
				}
			}
			Space::Block(n) => {
				for _ in 0..=n {
					self.out.push('\n');
				}
				for _ in 0..self.indent {
					self.out.push('\t');
				}
			}
		}
		self.space = Space::None;
	}

	pub fn newline(&mut self, lines: usize) {
		self.set_space(Space::Block(lines));
	}

	fn set_space(&mut self, space: Space) {
		self.space = self.space.max(space);
	}

	pub fn comment(&mut self, text: &str) {
		self.do_space(true);
		self.out.push_str("# ");
		self.out.push_str(text);
		self.newline(0);
	}

	pub fn block<I: IntoIterator>(&mut self, block: I, f: impl FnMut(I::Item, &mut Self)) {
		self.block_commented("", block, f);
	}

	pub fn block_commented<I: IntoIterator>(&mut self, comment: &str, block: I, mut f: impl FnMut(I::Item, &mut Self)) {
		self._sym_("{");
		self.indent += 1;
		let mut n = false;
		if !comment.is_empty() {
			self.newline(0);
			self.comment(comment);
			n = true;
		}
		for stmt in block {
			self.newline(0);
			f(stmt, self);
			n = true;
		}
		if n {
			self.newline(0);
		}
		self.indent -= 1;
		self._sym_("}");
	}

	pub fn finish(mut self) -> String {
		if matches!(self.space, Space::Block(_)) {
			self.out.push('\n');
		}
		self.out
	}
}

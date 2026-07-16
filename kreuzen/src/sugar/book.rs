//! Combine groups of `BookData<XX>_*` chunks into a single `Book` chunk.
use crate::tables::Table;
use crate::tables::book::{Book, BookData};
use crate::text::{Text, TextControl, TextPart};
use crate::{Chunk, Scena};

pub fn resugar(scena: &mut Scena) -> rootcause::Result<()> {
	let mut chunks = Vec::with_capacity(scena.chunks.len());
	let mut iter = std::mem::take(&mut scena.chunks).into_iter().peekable();
	while let Some(chunk) = iter.next() {
		let Chunk::Table(Table::BookData { name, book }) = chunk else {
			chunks.push(chunk);
			continue;
		};
		let _span = tracing::error_span!("book", name = %name).entered();
		crate::ensure!(
			let Some(base) = name.strip_suffix("_99"),
			"expected book header, got {name}"
		);
		crate::ensure!(let BookData::Header(n) = book, "book header {name} has non-header contents");

		let mut pages = Vec::with_capacity(n as usize);
		while let Some(Chunk::Table(Table::BookData { name, .. })) = iter.peek()
			&& *name == format!("{base}_{:02}", pages.len() + 1)
		{
			let Some(Chunk::Table(Table::BookData { name, book })) = iter.next() else {
				unreachable!()
			};
			pages.push(match book {
				BookData::TitlePage(title, text) => (Some(title), parse_text(&text)?),
				BookData::Page(text) => (None, parse_text(&text)?),
				BookData::Empty => (None, Text(Vec::new())),
				BookData::Header(_) => rootcause::bail!("page {name} is a header"),
			});
		}
		if pages.len() != n as usize {
			tracing::warn!("book {base} header says {n} pages, found {}", pages.len());
		}
		chunks.push(Chunk::Table(Table::Book { name: base.to_owned(), book: Book { pages } }));
	}
	scena.chunks = chunks;
	Ok(())
}

pub fn desugar(scena: &mut Scena) -> rootcause::Result<()> {
	let mut chunks = Vec::with_capacity(scena.chunks.len());
	for chunk in std::mem::take(&mut scena.chunks) {
		let Chunk::Table(Table::Book { name: base, book }) = chunk else {
			chunks.push(chunk);
			continue;
		};
		chunks.push(Chunk::Table(Table::BookData {
			name: format!("{base}_99"),
			book: BookData::Header(book.pages.len() as u16),
		}));
		for (m, (title, text)) in book.pages.into_iter().enumerate() {
			let text = unparse_text(&text)?;
			let book = match title {
				Some(title) => BookData::TitlePage(title, text),
				None if text.is_empty() => BookData::Empty,
				None => BookData::Page(text),
			};
			chunks.push(Chunk::Table(Table::BookData { name: format!("{base}_{:02}", m + 1), book }));
		}
	}
	scena.chunks = chunks;
	Ok(())
}

fn parse_text(s: &str) -> rootcause::Result<Text> {
	let mut parts = Vec::new();
	let mut scratch = String::new();
	let mut chars = s.chars();
	while let Some(c) = chars.next() {
		if c == '\\' {
			crate::ensure!(let Some('n') = chars.next(), "unknown escape in book text {s:?}");
			if !scratch.is_empty() {
				parts.push(TextPart::String(std::mem::take(&mut scratch)));
			}
			parts.push(TextPart::Control(TextControl::Line));
		} else {
			scratch.push(c);
		}
	}
	if !scratch.is_empty() {
		parts.push(TextPart::String(scratch));
	}
	Ok(Text(parts))
}

fn unparse_text(text: &Text) -> rootcause::Result<String> {
	let mut out = String::new();
	for part in &text.0 {
		match part {
			TextPart::String(s) => {
				crate::ensure!(!s.contains('\\'), "backslash in book text {s:?}");
				out.push_str(s);
			}
			TextPart::Control(TextControl::Line) => out.push_str("\\n"),
			TextPart::Control(c) => rootcause::bail!("{c:?} is not allowed in book text"),
		}
	}
	Ok(out)
}

use std::fmt::Debug;
use std::ops::Range;

pub struct Errors {
	pub errors: Vec<Diagnostic>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Severity {
	Info,
	Warning,
	Error,
	Fatal,
}

pub struct Diagnostic {
	pub severity: Severity,
	pub main: Note,
	pub notes: Vec<Note>,
}

impl Diagnostic {
	pub fn sort_key(&self) -> impl Ord {
		(self.main.span.start, self.severity)
	}

	pub fn note(&mut self, desc: impl Into<String>, span: Range<usize>) -> &mut Self {
		self.notes.push(Note { desc: desc.into(), span });
		self
	}
}

pub struct Note {
	pub desc: String,
	pub span: Range<usize>,
}

impl Errors {
	pub fn new() -> Self {
		Self { errors: Vec::new() }
	}

	pub fn info(&mut self, desc: impl Into<String>, span: Range<usize>) -> &mut Diagnostic {
		self.push(Severity::Info, desc, span)
	}

	pub fn warning(&mut self, desc: impl Into<String>, span: Range<usize>) -> &mut Diagnostic {
		self.push(Severity::Warning, desc, span)
	}

	pub fn error(&mut self, desc: impl Into<String>, span: Range<usize>) -> &mut Diagnostic {
		self.push(Severity::Error, desc, span)
	}

	pub fn fatal(&mut self, desc: impl Into<String>, span: Range<usize>) -> &mut Diagnostic {
		self.push(Severity::Fatal, desc, span)
	}

	fn push(&mut self, severity: Severity, desc: impl Into<String>, span: Range<usize>) -> &mut Diagnostic {
		self.errors.push(Diagnostic {
			severity,
			main: Note { desc: desc.into(), span },
			notes: Vec::new(),
		});
		self.errors.last_mut().unwrap()
	}

	pub fn max_severity(&self) -> Severity {
		self.errors.iter().map(|e| e.severity).max().unwrap_or(Severity::Info)
	}

	pub fn is_empty(&self) -> bool {
		self.errors.is_empty()
	}
}

impl Default for Errors {
	fn default() -> Self {
		Self::new()
	}
}

impl Debug for Errors {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.debug_struct("Errors").field("errors", &self.errors).finish_non_exhaustive()
	}
}

impl Debug for Diagnostic {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		let mut tup = f.debug_tuple(&format!("{:?}", self.severity));
		tup.field(&self.main);
		for note in &self.notes {
			tup.field(note);
		}
		tup.finish()
	}
}

impl Debug for Note {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.write_fmt(format_args!("{:?}:{}", self.span, self.desc))
	}
}

/// Renders the diagnostics as human-readable text with source snippets, with ANSI colors.
pub fn render(filename: &str, src: &str, errors: &Errors) -> String {
	use codespan_reporting::diagnostic::{Diagnostic as CsDiagnostic, Label, Severity as CsSeverity};
	use codespan_reporting::files::SimpleFile;
	use codespan_reporting::term;

	let file = SimpleFile::new(filename, src);
	let config = term::Config::default();
	let mut writer = term::termcolor::Buffer::ansi();

	let mut sorted = errors.errors.iter().collect::<Vec<_>>();
	sorted.sort_by(|a, b| a.sort_key().cmp(&b.sort_key()));

	for error in sorted {
		let severity = match error.severity {
			Severity::Fatal | Severity::Error => CsSeverity::Error,
			Severity::Warning => CsSeverity::Warning,
			Severity::Info => CsSeverity::Note,
		};
		let diag = CsDiagnostic::new(severity)
			.with_message(&error.main.desc)
			.with_label(Label::primary((), error.main.span.clone()).with_message(&error.main.desc))
			.with_labels_iter(
				error
					.notes
					.iter()
					.map(|note| Label::secondary((), note.span.clone()).with_message(&note.desc)),
			);
		term::emit(&mut writer, &config, &file, &diag).unwrap();
	}
	String::from_utf8(writer.into_inner()).unwrap()
}

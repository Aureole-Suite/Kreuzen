use std::ffi::{CStr, CString};

use gospel::read::{Reader, Le as _};
use snafu::ResultExt as _;

#[extend::ext]
impl<'a> Reader<'a> {
	#[track_caller]
	fn str(&mut self) -> Result<&'a str, ReadError> {
		let cstr = self.cstr()?;
		cstr.to_str().context(StringSnafu { cstr })
	}
}

#[derive(Debug, snafu::Snafu)]
pub enum ReadError {
	#[snafu(display("invalid read (at {location})"), context(false))]
	Read {
		source: gospel::read::Error,
		#[snafu(implicit)]
		location: snafu::Location,
	},
	#[snafu(display("invalid string: {cstr:?}"))]
	String {
		cstr: CString,
		#[snafu(implicit)]
		location: snafu::Location,
		source: std::str::Utf8Error,
	},
	#[snafu(display("could not read function '{name}'"))]
	Function {
		name: String,
		source: FunctionError,
	},
}

#[derive(Clone)]
struct Entry {
	name: String,
	start: usize,
}

impl std::fmt::Debug for Entry {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}@{}", self.name, self.start)
	}
}

pub fn parse(data: &[u8]) -> Result<(), ReadError> {
	let mut f = Reader::new(data);
	f.check_u32(0x20)?;
	f.check_u32(0x20)?;
	let table_top = f.u32()? as usize;
	let table_size = f.u32()? as usize;
	let function_name_table_top = f.u32()? as usize;
	let nfunc = f.u32()? as usize;
	let asm_end = f.u32()? as usize;
	assert_eq!(table_top + table_size, function_name_table_top);
	assert_eq!(table_size, nfunc * 4);

	f.check_u32(0xABCDEF00)?;
	let name = f.str()?;

	// In most cases, there's an align 4 followed by `00 00 00 FF`. But not always.
	let mut aligned = 0;
	while f.pos() < table_top {
		f.align_zeroed(4)?;
		f.check_u32(0xFF000000)?;
		aligned += 1;
	};

	assert_eq!(f.pos(), table_top);
	let table = read_table(&mut f, nfunc)?;
	assert_eq!(f.pos(), asm_end);
	f.align_zeroed(4)?;

	let ends = table.iter().map(|e| e.start).skip(1).chain([f.len()]);
	for (entry, end) in table.iter().zip(ends) {
		let _span = tracing::trace_span!("function", name = entry.name.as_str(), start = entry.start, end).entered();
		read_func(f.at(entry.start)?, end).context(FunctionSnafu { name: &entry.name })?;
	}

	println!("{name:?} {:?}", table);

	Ok(())
}

#[derive(Debug, snafu::Snafu)]
pub enum FunctionError {
	#[snafu(display("invalid read (at {location})"), context(false))]
	Read {
		source: gospel::read::Error,
		#[snafu(implicit)]
		location: snafu::Location,
	},
}

// This function corresponds to the /asm/ files. Cursed.
fn read_table<'a>(f: &mut Reader<'a>, n: usize) -> Result<Vec<Entry>, ReadError> {
	let mut starts = Vec::with_capacity(n);
	for _ in 0..n {
		starts.push(f.u32()? as usize);
	}
	let mut lengths = Vec::with_capacity(n);
	for _ in 0..n {
		lengths.push(f.u16()? as usize);
	}
	let mut names = Vec::with_capacity(n);
	#[expect(clippy::needless_range_loop)]
	for i in 0..n {
		assert_eq!(f.pos(), lengths[i]);
		names.push(f.str()?);
	}
	let mut entries = Vec::with_capacity(n);
	for i in 0..n {
		entries.push(Entry {
			name: names[i].to_owned(),
			start: starts[i],
		});
	}
	Ok(entries)
}

fn read_func(mut f: Reader<'_>, end: usize) -> Result<(), FunctionError> {
	print!("{:#1.32X}", f.dump().num_width_as(0xFFFF).end(end));
	Ok(())
}

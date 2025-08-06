#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Opcode([u8; 4], u8);

impl std::fmt::Display for Opcode {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		std::fmt::Debug::fmt(self, f)
	}
}

impl std::fmt::Debug for Opcode {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		for byte in *self {
			write!(f, "{byte:02X}")?;
		}
		Ok(())
	}
}

impl std::str::FromStr for Opcode {
	type Err = hex::FromHexError;

	fn from_str(s: &str) -> Result<Self, Self::Err> {
		use hex::FromHex;
		Ok(match s.len() {
			0 => Opcode::new(&[]),
			2 => Opcode::new(&<[u8; 1]>::from_hex(s)?),
			4 => Opcode::new(&<[u8; 2]>::from_hex(s)?),
			6 => Opcode::new(&<[u8; 3]>::from_hex(s)?),
			8 => Opcode::new(&<[u8; 4]>::from_hex(s)?),
			_ => return Err(hex::FromHexError::InvalidStringLength),
		})
	}
}

impl Opcode {
	pub fn new(bytes: &[u8]) -> Self {
		assert!(bytes.len() <= 4, "Opcode length must be between 0 and 4");
		let mut val = [0; 4];
		val[..bytes.len()].copy_from_slice(bytes);
		Opcode(val, bytes.len() as u8)
	}

	pub fn as_slice(&self) -> &[u8] {
		&self.0[..self.1 as usize]
	}

	pub fn prefixes(self) -> impl Iterator<Item = Opcode> {
		(0..self.len()).map(move |i| Opcode::new(&self[..i]))
	}

	pub fn push(&mut self, byte: u8) {
		assert!(self.1 < 4, "opcode length cannot exceed 4 bytes");
		self.0[self.1 as usize] = byte;
		self.1 += 1;
	}
}

impl std::borrow::Borrow<[u8]> for Opcode {
	fn borrow(&self) -> &[u8] {
		&self.0
	}
}

impl<T: std::slice::SliceIndex<[u8]>> std::ops::Index<T> for Opcode {
	type Output = T::Output;

	fn index(&self, index: T) -> &Self::Output {
		&self.as_slice()[index]
	}
}

impl IntoIterator for Opcode {
	type Item = u8;
	type IntoIter = std::array::IntoIter<u8, 4>;

	fn into_iter(self) -> Self::IntoIter {
		let mut iter = self.0.into_iter();
		for _ in self.1..4 {
			iter.next_back();
		}
		iter
	}
}

impl std::ops::Deref for Opcode {
	type Target = [u8];

	fn deref(&self) -> &Self::Target {
		&self.0[..self.1 as usize]
	}
}

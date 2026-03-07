#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Game {
	Cs1,
	Cs2,
	Cs3,
	Cs4,
	Reverie,
	// I believe Tx should be supported too but I don't own it
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Enc {
	Sjis,
	Utf8,
}

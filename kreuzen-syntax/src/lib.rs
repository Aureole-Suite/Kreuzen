mod code;
mod ctx;
mod tables;
mod types;

pub use ctx::Ctx;

pub trait Print {
	fn print(&self, ctx: &mut Ctx);
}

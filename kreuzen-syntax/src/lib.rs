mod code;
mod ctx;
mod tables;
mod types;

pub use ctx::Ctx;

pub use code::{print_flat, print_function, print_preload, print_shadow};
pub use tables::print_table;

pub trait Print {
	fn print(&self, ctx: &mut Ctx);
}

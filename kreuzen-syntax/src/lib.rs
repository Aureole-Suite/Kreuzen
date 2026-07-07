mod ctx;
mod print;
mod tables;

pub use ctx::Ctx;

pub use print::{print_flat, print_function, print_preload, print_shadow};
pub use tables::print_table;

pub trait Print {
	fn print(&self, ctx: &mut Ctx);
}

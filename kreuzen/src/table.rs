use arrayvec::ArrayVec;
use gospel::read::Le as _;
use snafu::ensure;

use crate::{ReaderaExt as _, VReader};

pub mod effect;
pub mod fc_auto;
pub mod book;
pub mod book99;
pub mod btlset;
pub mod style_name;

pub mod action_table;
pub mod add_collision;
pub mod algo_table;
pub mod anime_clip_table;
pub mod break_table;

pub use effect::Effect;
pub use book::Book;
pub use btlset::{Btlset, BtlVariant};
pub use action_table::Action;
pub use add_collision::Collision;
pub use algo_table::Algo;
pub use anime_clip_table::AnimeClip;

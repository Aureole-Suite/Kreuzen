use gospel::read::Le as _;
use gospel::write::Le as _;
use snafu::ensure;

use crate::{ReaderaExt as _, VReader};
use crate::{WriterExt as _, VWriter};

pub mod preload;
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
pub mod field_follow_data;
pub mod field_monster_data;
pub mod part_table;
pub mod reaction_table;
pub mod summon_table;
pub mod weapon_att_table;

pub use preload::Preload;
pub use book::Book;
pub use btlset::{Btlset, BtlVariant};
pub use action_table::Action;
pub use add_collision::Collision;
pub use algo_table::Algo;
pub use anime_clip_table::AnimeClip;
pub use field_follow_data::FieldFollowData;
pub use field_monster_data::FieldMonsterData;
pub use part_table::Part;
pub use reaction_table::Reaction;
pub use summon_table::Summon;
pub use weapon_att_table::WeaponAtt;

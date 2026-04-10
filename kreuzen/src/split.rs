#[derive(Debug, Clone, Default)]
pub struct Split {
	pub entries: Vec<Entry>,
	pub charater_section: Option<usize>,
}

#[derive(Debug, Clone)]
pub struct Entry {
	pub name: String,
	pub main: usize,
	pub preload: Option<usize>,
	pub shadow: Vec<usize>,
}

fn strip_shadow_prefix(s: &str) -> Option<(u32, &str)> {
	let s = s.strip_prefix("_a")?;
	let (n, s) = s.split_once('_')?;
	Some((n.parse().ok()?, s))
}

fn is_valid_preload(main_names: &[String], preload: &[String]) -> bool {
	let mut main_names = main_names.iter();
	for s in preload {
		let Some(base) = s.strip_prefix('_') else {
			return false;
		};
		if main_names.find(|&m| m == base).is_none() {
			return false;
		}
	}
	true
}

/// Finds the index at which the shadow section begins.
///
/// Assumes that no main or preload element starts with `_aN_`. Hopefully this holds.
fn find_shadow_start(list: &[String]) -> usize {
	list.iter()
		.position(|s| strip_shadow_prefix(s).is_some())
		.unwrap_or(list.len())
}

/// Finds the index at which the preload section begins.
fn find_preload_start(list: &[String]) -> usize {
	let min_start = list
		.iter()
		.rposition(|s| !s.starts_with('_'))
		.map_or(0, |i| i + 1);

	(min_start..=list.len())
		.find(|&start| is_valid_preload(&list[..start], &list[start..]))
		.expect("empty preload is always valid")
}

pub fn parse(list: &[String]) -> Split {
	let mut shadow_start = find_shadow_start(list);
	let preload_start = find_preload_start(&list[..shadow_start]);
	let main = &list[..preload_start];
	let preload = &list[preload_start..shadow_start];
	let mut charater_section = None;
	if list.get(shadow_start).map(|s| s.as_str()) == Some("_a0_CharaterSection") {
		charater_section = Some(shadow_start);
		shadow_start += 1;
	}
	let shadow = &list[shadow_start..];

	let mut entries: Vec<Entry> = main.iter().enumerate()
		.map(|(i, n)| Entry {
			name: n.clone(),
			main: i,
			preload: None,
			shadow: Vec::new(),
		})
		.collect();

	{
		let mut main_iter = entries.iter_mut();
		for (s, offset) in preload.iter().zip(preload_start..) {
			let base = &s[1..];
			match main_iter.find(|a| a.name == base) {
				Some(e) => e.preload = Some(offset),
				None => panic!("preload {s:?} has no matching main entry"),
			}
		}
	}

	{
		let mut main_iter = entries.iter_mut();
		let mut cur = None;
		for (s, offset) in shadow.iter().zip(shadow_start..) {
			let (n, base) = strip_shadow_prefix(s).expect("shadow element lacks _aN_ prefix");
			if n == 0 {
				match main_iter.find(|a| a.name == base) {
					Some(e) => cur = Some(e),
					None => panic!("shadow {s:?} has no matching main entry"),
				}
			}
			let e = cur.as_mut().expect("shadow levels must be filled in ascending N order");
			assert_eq!(e.shadow.len(), n as usize, "shadow levels must be filled in ascending N order");
			e.shadow.push(offset);
		}
	}

	Split { entries, charater_section }
}

#[cfg(test)]
mod tests;

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

pub fn parse(list: &[String]) -> Vec<Entry> {
	let shadow_start = find_shadow_start(list);
	let preload_start = find_preload_start(&list[..shadow_start]);

	let mut entries: Vec<Entry> = list[..preload_start].iter().enumerate()
		.map(|(i, n)| Entry {
			name: n.clone(),
			main: i,
			preload: None,
			shadow: Vec::new(),
		})
		.collect();

	{
		let mut main_iter = entries.iter_mut();
		for (offset, s) in list[preload_start..shadow_start].iter().enumerate() {
			let base = &s[1..];
			let e = main_iter
				.find(|a| a.name == base)
				.expect("preload element has no matching main entry");
			e.preload = Some(preload_start + offset);
		}
	}

	{
		let mut main_iter = entries.iter_mut();
		let mut cur = None;
		for (offset, s) in list[shadow_start..].iter().enumerate() {
			let (n, base) = strip_shadow_prefix(s).expect("shadow element lacks _aN_ prefix");
			let abs_idx = shadow_start + offset;
			if n == 0 {
				let e = main_iter
					.find(|a| a.name == base)
					.expect("shadow _a0_ element has no matching main entry");
				cur = Some(e);
			}
			let e = cur.as_mut().expect("_aN_ (N>0) shadow element without a preceding _a0_");
			assert_eq!(e.shadow.len(), n as usize, "shadow levels must be filled in ascending N order");
			e.shadow.push(abs_idx);
		}
	}

	entries
}

#[cfg(test)]
mod tests;

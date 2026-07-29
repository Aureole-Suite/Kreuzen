use std::collections::HashMap;

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
/// Only used to delimit the region in which preloads may occur; elements after this point are
/// classified by name, so a main element may well appear there.
///
/// Assumes that no main or preload element starts with `_aN_`. Hopefully this holds.
fn find_shadow_start(list: &[String]) -> usize {
	list.iter().position(|s| strip_shadow_prefix(s).is_some()).unwrap_or(list.len())
}

/// Finds the index at which the preload section begins.
fn find_preload_start(list: &[String]) -> usize {
	let min_start = list.iter().rposition(|s| !s.starts_with('_')).map_or(0, |i| i + 1);

	(min_start..=list.len())
		.find(|&start| is_valid_preload(&list[..start], &list[start..]))
		.expect("empty preload is always valid")
}

pub fn parse(list: &[String]) -> Split {
	// Initial version had a stricter dependency on chunk order, but some scripts modded with other tools do not follow the usual patterns.
	// So we instead use a fairly ugly check that mixes name and positionality.
	// We can't go fully name based, since there's some functions in cs4 btlwin that genuinely start with _.
	let shadow_start = find_shadow_start(list);
	let preload_start = find_preload_start(&list[..shadow_start]);

	let mut entries: Vec<Entry> = Vec::new();
	let mut by_name = HashMap::new();
	let mut preloads = Vec::new();
	let mut shadows = Vec::new();
	let mut charater_section = None;

	for (offset, s) in list.iter().enumerate() {
		if s == "_a0_CharaterSection" {
			assert!(charater_section.is_none(), "duplicate charater section");
			charater_section = Some(offset);
		} else if let Some((n, base)) = strip_shadow_prefix(s) {
			shadows.push((n, base, offset));
		} else if (preload_start..shadow_start).contains(&offset) {
			preloads.push((&s[1..], offset));
		} else {
			by_name.entry(s.as_str()).or_insert(entries.len());
			entries.push(Entry {
				name: s.clone(),
				main: offset,
				preload: None,
				shadow: Vec::new(),
			});
		}
	}

	for (base, offset) in preloads {
		let Some(&i) = by_name.get(base) else {
			panic!("preload {:?} has no matching main entry", list[offset])
		};
		let e = &mut entries[i];
		assert!(e.preload.is_none(), "duplicate preload for {:?}", e.name);
		e.preload = Some(offset);
	}

	// Sorting by level lets the assert below check that no level is missing.
	shadows.sort_by_key(|&(n, ..)| n);
	for (n, base, offset) in shadows {
		let Some(&i) = by_name.get(base) else {
			panic!("shadow {:?} has no matching main entry", list[offset])
		};
		let e = &mut entries[i];
		assert_eq!(e.shadow.len(), n as usize, "shadow {:?} is missing lower levels", list[offset]);
		e.shadow.push(offset);
	}

	// Writing always emits mains, then preloads, then the charater section, then shadows, so
	// anything else will come back out in a different order than it went in.
	let emitted = entries
		.iter()
		.map(|e| e.main)
		.chain(entries.iter().filter_map(|e| e.preload))
		.chain(charater_section)
		.chain(entries.iter().flat_map(|e| e.shadow.iter().copied()));
	if let Some((_, i)) = emitted.enumerate().find(|&(pos, i)| pos != i) {
		tracing::warn!("chunk {:?} is out of order", list[i]);
	}

	Split { entries, charater_section }
}

#[cfg(test)]
mod tests;

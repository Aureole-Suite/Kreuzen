use super::{Entry, Split, parse};

fn s<'a>(names: impl IntoIterator<Item = &'a str>) -> Vec<Entry> {
	let (names, split) = parse_and_validate(names);
	// Nothing has moved, so the sections are still in the order they will be written back in.
	let parts = parts(&split);
	assert!((0..names.len()).eq(parts.iter().copied()), "bad indices: {parts:?}");
	split.entries
}

/// Like [`s`], but for scripts with functions appended after the preloads and shadows. Those are
/// written back before them, so the order does not survive.
fn s_appended<'a>(names: impl IntoIterator<Item = &'a str>) -> Vec<Entry> {
	parse_and_validate(names).1.entries
}

fn parse_and_validate<'a>(names: impl IntoIterator<Item = &'a str>) -> (Vec<String>, Split) {
	let names = names.into_iter().map(|n| n.to_owned()).collect::<Vec<_>>();
	let split = parse(&names);
	validate(&names, &split);
	(names, split)
}

fn parts(split: &Split) -> Vec<usize> {
	split
		.entries
		.iter()
		.map(|e| e.main)
		.chain(split.entries.iter().filter_map(|e| e.preload))
		.chain(split.charater_section)
		.chain(split.entries.iter().flat_map(|e| e.shadow.iter().copied()))
		.collect()
}

fn validate(names: &[String], split: &Split) {
	for (i, e) in split.entries.iter().enumerate() {
		assert_eq!(names[e.main], e.name, "main name mismatch at {i}");
		if let Some(p) = e.preload {
			assert_eq!(names[p], format!("_{}", e.name), "preload name mismatch for {:?}", e.name);
		}
		for (j, &s) in e.shadow.iter().enumerate() {
			assert_eq!(names[s], format!("_a{j}_{}", e.name), "shadow[{j}] name mismatch for {:?}", e.name);
		}
	}
	if let Some(i) = split.charater_section {
		assert_eq!(names[i], "_a0_CharaterSection", "charater section name mismatch");
	}

	let mut parts = parts(split);
	parts.sort();
	assert!((0..names.len()).eq(parts.iter().copied()), "elements used more than once or not at all: {parts:?}");
}

#[test]
fn w1410() {
	let entries = s(include_str!("w1410.txt").lines());

	assert_eq!(entries.len(), 18);
	for (i, e) in entries.iter().enumerate() {
		assert_eq!(e.main, i);
	}
	for e in &entries[..6] {
		assert!(e.preload.is_none());
		assert!(e.shadow.is_empty());
	}
	assert_eq!(entries[6].preload, Some(18));
	assert_eq!(entries[6].shadow, vec![22]);
	assert_eq!(entries[10].preload, None);
	assert_eq!(entries[10].shadow, vec![26]);
	assert_eq!(entries[17].preload, None);
	assert_eq!(entries[17].shadow, vec![33]);
}

#[test]
fn w1210() {
	s(include_str!("w1210.txt").lines());
}

#[test]
fn btlwin() {
	s(include_str!("btlwin.txt").lines());
}

#[test]
fn a3210() {
	s([
		"",
		"PreInit",
		"Init",
		"Init_Replay",
		"Reinit",
		"EV_OneShotTest",
		"TK_Test",
		"_a0_CharaterSection",
		"_a0_Init_Replay",
		"_a0_EV_OneShotTest",
	]);
}

#[test]
fn w1210_a1_shadow() {
	let entries = s([
		"EV_A3_20_17",
		"EV_A3_20_17_END",
		"_a0_EV_A3_20_17",
		"_a1_EV_A3_20_17",
		"_a0_EV_A3_20_17_END",
	]);

	assert_eq!(entries.len(), 2);
	assert_eq!(entries[0].shadow, vec![2, 3]);
	assert_eq!(entries[1].shadow, vec![4]);
	assert!(entries[0].preload.is_none());
	assert!(entries[1].preload.is_none());
}

#[test]
fn no_preload_no_shadow() {
	let entries = s(["foo", "bar", "baz"]);
	assert_eq!(entries.len(), 3);
	for (i, e) in entries.iter().enumerate() {
		assert_eq!(e.main, i);
		assert!(e.preload.is_none());
		assert!(e.shadow.is_empty());
	}
}

#[test]
fn main_appended_after_shadows() {
	let entries = s_appended([
		"",
		"PreInit",
		"Init",
		"EV_OneShotTest",
		"_Init",
		"_a0_CharaterSection",
		"_a0_Init",
		"_a0_EV_OneShotTest",
		"ModdedFunc",
		"ModdedFunc2",
	]);

	assert_eq!(entries.len(), 6);
	assert_eq!(entries[2].name, "Init");
	assert_eq!(entries[2].preload, Some(4));
	assert_eq!(entries[2].shadow, vec![6]);
	assert_eq!(entries[3].shadow, vec![7]);
	assert_eq!(entries[4].name, "ModdedFunc");
	assert_eq!(entries[4].main, 8);
	assert_eq!(entries[5].name, "ModdedFunc2");
	assert_eq!(entries[5].main, 9);
}

#[test]
fn main_appended_after_preloads() {
	let entries = s_appended(["Init", "Func1", "_Init", "_Func1", "ModdedFunc"]);

	assert_eq!(entries.len(), 3);
	assert_eq!(entries[0].preload, Some(2));
	assert_eq!(entries[1].preload, Some(3));
	assert_eq!(entries[2].name, "ModdedFunc");
	assert_eq!(entries[2].main, 4);
	assert!(entries[2].preload.is_none());
}

#[test]
fn underscore_main_not_mistaken_for_preload() {
	let entries = s([
		"BtlWinkeaInternal",
		"BtlWinKisin",
		"BtlWinLink_CELINE_TALK_D",
		"_BtlWinLink_CELINE_TALK_D",
		"_BtlWinkeaInternal",
		"_BtlWinKisin",
	]);

	assert_eq!(entries.len(), 4);
	assert_eq!(entries[3].name, "_BtlWinLink_CELINE_TALK_D");
	assert_eq!(entries[3].main, 3);
	assert_eq!(entries[0].preload, Some(4));
	assert_eq!(entries[1].preload, Some(5));
	assert!(entries[2].preload.is_none());
	assert!(entries[3].preload.is_none());
}

use super::{Entry, parse};

fn s<'a>(names: impl IntoIterator<Item=&'a str>) -> Vec<Entry> {
	let names = names.into_iter().map(|n| n.to_owned()).collect::<Vec<_>>();
	let entries = parse(&names);
	validate(&names, &entries);
	entries
}

fn validate(names: &[String], entries: &[Entry]) {
	for (i, e) in entries.iter().enumerate() {
		assert_eq!(names[e.main], e.name, "main name mismatch at {i}");
		if let Some(p) = e.preload {
			assert_eq!(names[p], format!("_{}", e.name), "preload name mismatch for {:?}", e.name);
		}
		for (j, &s) in e.shadow.iter().enumerate() {
			assert_eq!(names[s], format!("_a{j}_{}", e.name), "shadow[{j}] name mismatch for {:?}", e.name);
		}
	}

	let mut parts = entries
		.iter()
		.map(|e| e.main)
		.chain(entries.iter().filter_map(|e| e.preload))
		.chain(entries.iter().flat_map(|e| e.shadow.iter().copied()))
		.collect::<Vec<_>>();
	assert!((0..names.len()).eq(parts.iter().copied()), "bad indices: {parts:?}");
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

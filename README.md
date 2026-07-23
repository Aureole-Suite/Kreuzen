# Kreuzen

A decompiler for *Trails of Cold Steel I-IV*, *Trails into Reverie*, and *Tokyo Xanadu eX+*.
It can roundtrip most scripts bytewise, with the remainder being things that are either inconsequential or broken in the original scripts.

<details><summary>Non-roundtripping scripts</summary>

(Counts are not counting language duplicates)

- 12 scripts where AlgoTable is malformed
- 3 scripts where ActionTable is malformed
- 2 scripts have malformed btlsets
- 1 script in Reverie has an unconventional preload table
- 1 script in CS3 has a broken jump label that would probably crash the game
- One book in English TX, and 14 in Japanese CS3/CS4, have extra pages that are erroneously cut out and are restored

</details>

## Usage

For basic usage, drag either a .dat or .krz file, or a folder containing such, onto the executable. Outputs will be placed next to the input. For commandline usage, read `--help`.
Kruzen will attempt to guess the game based on the containing folder name, but you can override this detection with either `--game cs1` or by renaming the executable itself to `kreuzen-cs1.exe`.

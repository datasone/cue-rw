# cue-rw

[Documentation](https://docs.rs/cue-rw)

A library for reading and writing cue files, this library tries to be complicant with [Hydrogenaudio wiki](https://wiki.hydrogenaud.io/index.php?title=Cue_sheet).

## Usage
Parse a cue file with `TryFrom<&str>` trait on `CUEFile`, and use `Display` trait to serialize it back to `String`.

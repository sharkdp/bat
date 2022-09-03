#![cfg(feature = "paging")]

use std::ffi::OsStr;
use std::process::Command;

#[derive(Debug, PartialEq, Eq)]
pub enum LessVersion {
    Less(usize),
    BusyBox,
}

pub fn retrieve_less_version(less_path: &dyn AsRef<OsStr>) -> Option<LessVersion> {
    let resolved_path = grep_cli::resolve_binary(less_path.as_ref()).ok()?;

    let cmd = Command::new(resolved_path).arg("--version").output().ok()?;
    if cmd.status.success() {
        parse_less_version(&cmd.stdout)
    } else {
        parse_less_version_busybox(&cmd.stderr)
    }
}

fn parse_less_version(output: &[u8]) -> Option<LessVersion> {
    if !output.starts_with(b"less ") {
        return None;
    }

    let version = std::str::from_utf8(&output[5..]).ok()?;
    let end = version.find(|c: char| !c.is_ascii_digit())?;
    Some(LessVersion::Less(version[..end].parse::<usize>().ok()?))
}

fn parse_less_version_busybox(output: &[u8]) -> Option<LessVersion> {
    match std::str::from_utf8(output) {
        Ok(version) if version.contains("BusyBox ") => Some(LessVersion::BusyBox),
        _ => None,
    }
}

#[test]
fn test_parse_less_version_487() {
    let output = b"less 487 (GNU regular expressions)
Copyright (C) 1984-2016  Mark Nudelman

less comes with NO WARRANTY, to the extent permitted by law.
For information about the terms of redistribution,
see the file named README in the less distribution.
Homepage: http://www.greenwoodsoftware.com/less";

    assert_eq!(Some(LessVersion::Less(487)), parse_less_version(output));
}

#[test]
fn test_parse_less_version_529() {
    let output = b"less 529 (Spencer V8 regular expressions)
Copyright (C) 1984-2017  Mark Nudelman

less comes with NO WARRANTY, to the extent permitted by law.
For information about the terms of redistribution,
see the file named README in the less distribution.
Homepage: http://www.greenwoodsoftware.com/less";

    assert_eq!(Some(LessVersion::Less(529)), parse_less_version(output));
}

#[test]
fn test_parse_less_version_551() {
    let output = b"less 551 (PCRE regular expressions)
Copyright (C) 1984-2019  Mark Nudelman

less comes with NO WARRANTY, to the extent permitted by law.
For information about the terms of redistribution,
see the file named README in the less distribution.
Home page: http://www.greenwoodsoftware.com/less";

    assert_eq!(Some(LessVersion::Less(551)), parse_less_version(output));
}

#[test]
fn test_parse_less_version_581_2() {
    let output = b"less 581.2 (PCRE2 regular expressions)
Copyright (C) 1984-2021  Mark Nudelman

less comes with NO WARRANTY, to the extent permitted by law.
For information about the terms of redistribution,
see the file named README in the less distribution.
Home page: https://greenwoodsoftware.com/less";

    assert_eq!(Some(LessVersion::Less(581)), parse_less_version(output));
}

#[test]
fn test_parse_less_version_wrong_program() {
    let output = b"more from util-linux 2.34";

    assert_eq!(None, parse_less_version(output));
    assert_eq!(None, parse_less_version_busybox(output));
}

#[test]
fn test_parse_less_version_busybox() {
    let output = b"pkg/less: unrecognized option '--version'
BusyBox v1.35.0 (2022-04-21 10:38:11 EDT) multi-call binary.

Usage: less [-EFIMmNSRh~] [FILE]...

View FILE (or stdin) one screenful at a time

        -E      Quit once the end of a file is reached
        -F      Quit if entire file fits on first screen
        -I      Ignore case in all searches
        -M,-m   Display status line with line numbers
                and percentage through the file
        -N      Prefix line number to each line
        -S      Truncate long lines
        -R      Remove color escape codes in input
        -~      Suppress ~s displayed past EOF";

    assert_eq!(
        Some(LessVersion::BusyBox),
        parse_less_version_busybox(output)
    );
}

#[test]
fn test_parse_less_version_invalid_utf_8() {
    let output = b"\xff";

    assert_eq!(None, parse_less_version(output));
    assert_eq!(None, parse_less_version_busybox(output));
}

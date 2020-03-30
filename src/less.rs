#![cfg(feature = "paging")]

use std::process::Command;

pub fn retrieve_less_version() -> Option<usize> {
    let cmd = Command::new("less").arg("--version").output().ok()?;
    parse_less_version(&cmd.stdout)
}

fn parse_less_version(output: &[u8]) -> Option<usize> {
    if output.starts_with(b"less ") {
        let version = std::str::from_utf8(&output[5..]).ok()?;
        let end = version.find(' ')?;
        version[..end].parse::<usize>().ok()
    } else {
        None
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

    assert_eq!(Some(487), parse_less_version(output));
}

#[test]
fn test_parse_less_version_529() {
    let output = b"less 529 (Spencer V8 regular expressions)
Copyright (C) 1984-2017  Mark Nudelman

less comes with NO WARRANTY, to the extent permitted by law.
For information about the terms of redistribution,
see the file named README in the less distribution.
Homepage: http://www.greenwoodsoftware.com/less";

    assert_eq!(Some(529), parse_less_version(output));
}

#[test]
fn test_parse_less_version_551() {
    let output = b"less 551 (PCRE regular expressions)
Copyright (C) 1984-2019  Mark Nudelman

less comes with NO WARRANTY, to the extent permitted by law.
For information about the terms of redistribution,
see the file named README in the less distribution.
Home page: http://www.greenwoodsoftware.com/less";

    assert_eq!(Some(551), parse_less_version(output));
}

#[test]
fn test_parse_less_version_wrong_program() {
    let output = b"more from util-linux 2.34";

    assert_eq!(None, parse_less_version(output));
}

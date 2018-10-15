extern crate assert_cmd;

use assert_cmd::prelude::*;
use std::process::Command;

fn bat() -> Command {
    let mut cmd = Command::main_binary().unwrap();
    cmd.current_dir("tests/examples");
    cmd.arg("--no-config");
    cmd.env_remove("PAGER");
    cmd.env_remove("BAT_PAGER");
    cmd
}

#[test]
fn basic() {
    bat()
        .arg("test.txt")
        .assert()
        .success()
        .stdout("hello world\n")
        .stderr("");
}

#[test]
fn stdin() {
    bat()
        .with_stdin()
        .buffer("foo\nbar\n")
        .assert()
        .success()
        .stdout("foo\nbar\n");
}

#[test]
fn concatenate() {
    bat()
        .arg("test.txt")
        .arg("test.txt")
        .assert()
        .success()
        .stdout("hello world\nhello world\n");
}

#[test]
fn concatenate_stdin() {
    bat()
        .arg("test.txt")
        .arg("-")
        .arg("test.txt")
        .with_stdin()
        .buffer("stdin\n")
        .assert()
        .success()
        .stdout("hello world\nstdin\nhello world\n");
}

#[test]
fn line_numbers() {
    bat()
        .arg("multiline.txt")
        .arg("--style=numbers")
        .arg("--decorations=always")
        .assert()
        .success()
        .stdout("   1 line 1\n   2 line 2\n   3 line 3\n   4 line 4\n");
}

#[test]
fn line_range_2_3() {
    bat()
        .arg("multiline.txt")
        .arg("--line-range=2:3")
        .assert()
        .success()
        .stdout("line 2\nline 3\n");
}

#[test]
fn line_range_first_two() {
    bat()
        .arg("multiline.txt")
        .arg("--line-range=:2")
        .assert()
        .success()
        .stdout("line 1\nline 2\n");
}

#[test]
fn line_range_last_3() {
    bat()
        .arg("multiline.txt")
        .arg("--line-range=2:")
        .assert()
        .success()
        .stdout("line 2\nline 3\nline 4\n");
}

#[test]
fn tabs_passthrough_wrapped() {
    bat()
        .arg("tabs.txt")
        .arg("--tabs=0")
        .arg("--wrap=character")
        .arg("--style=plain")
        .arg("--decorations=always")
        .assert()
        .success()
        .stdout("
	1	2	3	4
1	?
22	?
333	?
4444	?
55555	?
666666	?
7777777	?
88888888	?
");
}

#[test]
fn tabs_4_wrapped() {
    bat()
        .arg("tabs.txt")
        .arg("--tabs=4")
        .arg("--wrap=character")
        .arg("--style=plain")
        .arg("--decorations=always")
        .assert()
        .success()
        .stdout("
    1   2   3   4
1   ?
22  ?
333 ?
4444    ?
55555   ?
666666  ?
7777777 ?
88888888    ?
");
}

#[test]
fn tabs_8_wrapped() {
    bat()
        .arg("tabs.txt")
        .arg("--tabs=8")
        .arg("--wrap=character")
        .arg("--style=plain")
        .arg("--decorations=always")
        .assert()
        .success()
        .stdout("
        1       2       3       4
1       ?
22      ?
333     ?
4444    ?
55555   ?
666666  ?
7777777 ?
88888888        ?
");
}

#[test]
fn tabs_passthrough() {
    bat()
        .arg("tabs.txt")
        .arg("--tabs=0")
        .arg("--wrap=never")
        .arg("--style=plain")
        .arg("--decorations=always")
        .assert()
        .success()
        .stdout("
	1	2	3	4
1	?
22	?
333	?
4444	?
55555	?
666666	?
7777777	?
88888888	?
");
}

#[test]
fn tabs_4() {
    bat()
        .arg("tabs.txt")
        .arg("--tabs=4")
        .arg("--wrap=never")
        .arg("--style=plain")
        .arg("--decorations=always")
        .assert()
        .success()
        .stdout("
    1   2   3   4
1   ?
22  ?
333 ?
4444    ?
55555   ?
666666  ?
7777777 ?
88888888    ?
");
}

#[test]
fn tabs_8() {
    bat()
        .arg("tabs.txt")
        .arg("--tabs=8")
        .arg("--wrap=never")
        .arg("--style=plain")
        .arg("--decorations=always")
        .assert()
        .success()
        .stdout("
        1       2       3       4
1       ?
22      ?
333     ?
4444    ?
55555   ?
666666  ?
7777777 ?
88888888        ?
");
}

#[test]
fn fail_non_existing() {
    bat().arg("non-existing-file").assert().failure();
}

#[test]
fn fail_directory() {
    bat().arg("sub_directory").assert().failure();
}

#[test]
fn do_not_exit_directory() {
    bat()
        .arg("sub_directory")
        .arg("test.txt")
        .assert()
        .stdout("hello world\n")
        .failure();
}

#[test]
fn pager_basic() {
    bat()
        .env("PAGER", "echo pager-output")
        .arg("--paging=always")
        .arg("test.txt")
        .assert()
        .success()
        .stdout("pager-output\n");
}

#[test]
fn pager_overwrite() {
    bat()
        .env("PAGER", "echo other-pager")
        .env("BAT_PAGER", "echo pager-output")
        .arg("--paging=always")
        .arg("test.txt")
        .assert()
        .success()
        .stdout("pager-output\n");
}

#[test]
fn pager_disable() {
    bat()
        .env("PAGER", "echo other-pager")
        .env("BAT_PAGER", "")
        .arg("--paging=always")
        .arg("test.txt")
        .assert()
        .success()
        .stdout("hello world\n");
}

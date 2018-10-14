extern crate assert_cmd;

mod tester;

use assert_cmd::prelude::*;
use std::process::Command;
use tester::BatTester;

fn bat() -> Command {
    let mut cmd = Command::main_binary().unwrap();
    cmd.current_dir("tests/examples");
    cmd
}

fn tester() -> BatTester {
    BatTester::new()
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
    tester().test_snapshot("tabs_passthrough_wrapped", "full", 0, true);
}

#[test]
fn tabs_4_wrapped() {
    tester().test_snapshot("tabs_4_wrapped", "full", 4, true);
}

#[test]
fn tabs_8_wrapped() {
    tester().test_snapshot("tabs_8_wrapped", "full", 8, true);
}

#[test]
fn tabs_passthrough() {
    tester().test_snapshot("tabs_passthrough", "full", 0, false);
}

#[test]
fn tabs_4() {
    tester().test_snapshot("tabs_4", "full", 4, false);
}

#[test]
fn tabs_8() {
    tester().test_snapshot("tabs_8", "full", 8, false);
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

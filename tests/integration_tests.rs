extern crate assert_cmd;
extern crate escargot;
#[macro_use]
extern crate lazy_static;

use assert_cmd::prelude::*;
use escargot::CargoRun;
use std::process::Command;

lazy_static! {
    static ref CARGO_RUN: CargoRun = escargot::CargoBuild::new()
        .bin("bat")
        .current_release()
        .run()
        .unwrap();
}

fn bat_with_config() -> Command {
    let mut cmd = CARGO_RUN.command();
    cmd.current_dir("tests/examples");
    cmd.env_remove("PAGER");
    cmd.env_remove("BAT_PAGER");
    cmd.env_remove("BAT_CONFIG_PATH");
    cmd.env_remove("BAT_STYLE");
    cmd.env_remove("BAT_THEME");
    cmd.env_remove("BAT_TABS");
    cmd
}

fn bat() -> Command {
    let mut cmd = bat_with_config();
    cmd.arg("--no-config");
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
fn concatenate_empty_first() {
    bat()
        .arg("empty.txt")
        .arg("test.txt")
        .assert()
        .success()
        .stdout("hello world\n");
}

#[test]
fn concatenate_empty_last() {
    bat()
        .arg("test.txt")
        .arg("empty.txt")
        .assert()
        .success()
        .stdout("hello world\n");
}

#[test]
fn concatenate_empty_both() {
    bat()
        .arg("empty.txt")
        .arg("empty.txt")
        .assert()
        .success()
        .stdout("");
}

#[test]
fn concatenate_empty_between() {
    bat()
        .arg("test.txt")
        .arg("empty.txt")
        .arg("test.txt")
        .assert()
        .success()
        .stdout("hello world\nhello world\n");
}

#[test]
fn concatenate_empty_first_and_last() {
    bat()
        .arg("empty.txt")
        .arg("test.txt")
        .arg("empty.txt")
        .assert()
        .success()
        .stdout("hello world\n");
}

#[test]
fn concatenate_single_line() {
    bat()
        .arg("single-line.txt")
        .arg("single-line.txt")
        .assert()
        .success()
        .stdout("Single LineSingle Line");
}

#[test]
fn concatenate_single_line_empty() {
    bat()
        .arg("single-line.txt")
        .arg("empty.txt")
        .arg("single-line.txt")
        .assert()
        .success()
        .stdout("Single LineSingle Line");
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
fn line_range_multiple() {
    bat()
        .arg("multiline.txt")
        .arg("--line-range=1:2")
        .arg("--line-range=4:4")
        .assert()
        .success()
        .stdout("line 1\nline 2\nline 4\n");
}

#[test]
fn tabs_numbers() {
    bat()
        .arg("tabs.txt")
        .arg("--tabs=4")
        .arg("--style=numbers")
        .arg("--decorations=always")
        .assert()
        .success()
        .stdout(
            "   1     1   2   3   4
   2 1   ?
   3 22  ?
   4 333 ?
   5 4444    ?
   6 55555   ?
   7 666666  ?
   8 7777777 ?
   9 88888888    ?
",
        );
}

#[test]
fn tabs_passthrough_wrapped() {
    bat()
        .arg("tabs.txt")
        .arg("--tabs=0")
        .arg("--style=plain")
        .arg("--decorations=always")
        .assert()
        .success()
        .stdout(
            "	1	2	3	4
1	?
22	?
333	?
4444	?
55555	?
666666	?
7777777	?
88888888	?
",
        );
}

#[test]
fn tabs_4_wrapped() {
    bat()
        .arg("tabs.txt")
        .arg("--tabs=4")
        .arg("--style=plain")
        .arg("--decorations=always")
        .assert()
        .success()
        .stdout(
            "    1   2   3   4
1   ?
22  ?
333 ?
4444    ?
55555   ?
666666  ?
7777777 ?
88888888    ?
",
        );
}

#[test]
fn tabs_8_wrapped() {
    bat()
        .arg("tabs.txt")
        .arg("--tabs=8")
        .arg("--style=plain")
        .arg("--decorations=always")
        .assert()
        .success()
        .stdout(
            "        1       2       3       4
1       ?
22      ?
333     ?
4444    ?
55555   ?
666666  ?
7777777 ?
88888888        ?
",
        );
}

#[test]
fn tabs_passthrough() {
    bat()
        .arg("tabs.txt")
        .arg("--tabs=0")
        .arg("--style=plain")
        .arg("--decorations=always")
        .assert()
        .success()
        .stdout(
            "	1	2	3	4
1	?
22	?
333	?
4444	?
55555	?
666666	?
7777777	?
88888888	?
",
        );
}

#[test]
fn tabs_4() {
    bat()
        .arg("tabs.txt")
        .arg("--tabs=4")
        .arg("--style=plain")
        .arg("--decorations=always")
        .assert()
        .success()
        .stdout(
            "    1   2   3   4
1   ?
22  ?
333 ?
4444    ?
55555   ?
666666  ?
7777777 ?
88888888    ?
",
        );
}

#[test]
fn tabs_8() {
    bat()
        .arg("tabs.txt")
        .arg("--tabs=8")
        .arg("--style=plain")
        .arg("--decorations=always")
        .assert()
        .success()
        .stdout(
            "        1       2       3       4
1       ?
22      ?
333     ?
4444    ?
55555   ?
666666  ?
7777777 ?
88888888        ?
",
        );
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

#[test]
fn config_location_test() {
    bat_with_config()
        .env("BAT_CONFIG_PATH", "bat.conf")
        .arg("--config-file")
        .assert()
        .success()
        .stdout("bat.conf\n");
}

#[test]
fn config_read_arguments_from_file() {
    bat_with_config()
        .env("BAT_CONFIG_PATH", "bat.conf")
        .arg("test.txt")
        .assert()
        .success()
        .stdout("dummy-pager-from-config\n");
}

#[test]
fn utf16() {
    // The output will be converted to UTF-8 with a leading UTF-8 BOM
    bat()
        .arg("--plain")
        .arg("--decorations=always")
        .arg("test_UTF-16LE.txt")
        .assert()
        .success()
        .stdout(std::str::from_utf8(b"\xEF\xBB\xBFhello world\n").unwrap());
}

#[test]
fn can_print_file_named_cache() {
    bat_with_config()
        .arg("cache")
        .assert()
        .success()
        .stdout("test\n")
        .stderr("");
}

#[test]
fn can_print_file_named_cache_with_additional_argument() {
    bat_with_config()
        .arg("cache")
        .arg("test.txt")
        .assert()
        .success()
        .stdout("test\nhello world\n")
        .stderr("");
}

#[test]
fn can_print_file_starting_with_cache() {
    bat_with_config()
        .arg("cache.c")
        .assert()
        .success()
        .stdout("test\n")
        .stderr("");
}

#[test]
fn does_not_print_unwanted_file_named_cache() {
    bat_with_config().arg("cach").assert().failure();
}

#[test]
fn snip() {
    bat()
        .arg("multiline.txt")
        .arg("--style=numbers,snip")
        .arg("--decorations=always")
        .arg("--line-range=1:2")
        .arg("--line-range=4:")
        .arg("--terminal-width=80")
        .assert()
        .success()
        .stdout(
            "   1 line 1
   2 line 2
 ...─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ 8< ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─
   4 line 4
",
        );
}

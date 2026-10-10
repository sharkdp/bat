mod utils;

use predicates::prelude::*;
use utils::command::bat;

const WARNING: &str = "--warning=missing-trailing-newline";

#[test]
fn warning_does_not_change_redirected_bytes() {
    bat()
        .arg(WARNING)
        .write_stdin("text")
        .assert()
        .success()
        .stdout("text")
        .stderr(predicate::str::contains("No newline at end of file"));
}

#[test]
fn formatted_warning_appears_before_bottom_border() {
    bat().args([WARNING, "--decorations=always", "--style=numbers,grid", "--color=never", "--terminal-width=40"])
        .write_stdin("text").assert().success()
        .stdout("─────┬──────────────────────────────────\n   1 │ text\n     │ [No newline at end of file]\n─────┴──────────────────────────────────\n")
        .stderr("");
}

#[test]
fn complete_empty_and_excluded_final_lines_do_not_warn() {
    for input in ["", "text\n", "text\r\n"] {
        bat()
            .arg(WARNING)
            .write_stdin(input)
            .assert()
            .success()
            .stdout(input)
            .stderr("");
    }
    bat()
        .args([WARNING, "--line-range=1:1"])
        .write_stdin("first\nlast")
        .assert()
        .success()
        .stdout("first\n")
        .stderr("");
}

#[test]
fn utf16_endings_are_checked_as_code_units() {
    for (bom, little) in [([0xff, 0xfe], true), ([0xfe, 0xff], false)] {
        for (text, should_warn) in [("hello\n", false), ("hello", true)] {
            let mut input = bom.to_vec();
            for unit in text.encode_utf16() {
                input.extend(if little {
                    unit.to_le_bytes()
                } else {
                    unit.to_be_bytes()
                });
            }
            let result = bat()
                .arg(WARNING)
                .write_stdin(input.clone())
                .assert()
                .success()
                .stdout(input);
            if should_warn {
                result.stderr(predicate::str::contains("No newline at end of file"));
            } else {
                result.stderr("");
            }
        }
    }
}

#[test]
fn warnings_can_be_disabled_and_binary_inputs_do_not_warn() {
    bat()
        .args([WARNING, "--warning=none"])
        .write_stdin("text")
        .assert()
        .success()
        .stdout("text")
        .stderr("");
    bat()
        .args([WARNING, "test.binary"])
        .assert()
        .success()
        .stderr("");
}

#[test]
fn unbuffered_complete_input_does_not_report_partial_chunks() {
    bat()
        .args([WARNING, "--unbuffered"])
        .write_stdin("first\nsecond\n")
        .assert()
        .success()
        .stdout("first\nsecond\n")
        .stderr("");
}

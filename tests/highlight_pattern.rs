mod utils;

use utils::command::bat;

fn output(args: &[&str], input: &[u8]) -> Vec<u8> {
    bat()
        .args([
            "--color=always",
            "--theme=TwoDark",
            "--style=plain",
            "--paging=never",
        ])
        .args(args)
        .write_stdin(input)
        .assert()
        .success()
        .get_output()
        .stdout
        .clone()
}

#[test]
fn patterns_match_whole_lines_and_union_with_ranges() {
    let input = b"ok\nERROR: first\nwarn: second\ntail\n";
    assert_eq!(
        output(
            &[
                "--highlight-pattern",
                "^ERROR:",
                "--highlight-pattern",
                "(?i)^WARN:",
                "-H",
                "4"
            ],
            input
        ),
        output(&["-H", "2:4"], input),
    );
}

#[test]
fn pattern_anchors_ignore_crlf_and_work_without_final_newline() {
    let input = "ok\r\n警告\r\n警告".as_bytes();
    assert_eq!(
        output(&["--highlight-pattern", "^警告$"], input),
        output(&["-H", "2:3"], input)
    );
}

#[test]
fn pattern_highlights_all_wrapped_fragments() {
    let input = b"long enough for several lines\n";
    assert_eq!(
        output(
            &[
                "--highlight-pattern",
                "several lines$",
                "--wrap=character",
                "--terminal-width=10"
            ],
            input
        ),
        output(
            &["-H", "1", "--wrap=character", "--terminal-width=10"],
            input
        ),
    );
}

#[test]
fn patterns_work_with_ansi_theme_and_visible_ranges() {
    let input = b"first\nmatch\nlast\n";
    assert_eq!(
        output(
            &[
                "--theme=ansi",
                "--highlight-pattern",
                "match",
                "--line-range=2:3"
            ],
            input
        ),
        output(&["--theme=ansi", "-H", "2", "--line-range=2:3"], input),
    );
}

#[test]
fn invalid_pattern_fails_before_printing() {
    bat()
        .args(["--highlight-pattern", "[", "test.txt"])
        .assert()
        .failure()
        .stdout("");
}

#[test]
fn pattern_does_not_force_colors_when_redirected() {
    bat()
        .args(["--highlight-pattern", ".", "test.txt"])
        .assert()
        .success()
        .stdout("hello world\n");
}

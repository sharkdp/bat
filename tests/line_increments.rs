mod utils;

use utils::command::bat;

#[test]
fn display_periodic_lines_with_original_numbers() {
    bat()
        .args([
            "--line-range=2:8~3",
            "--style=numbers",
            "--decorations=always",
            "--color=never",
        ])
        .write_stdin("one\ntwo\nthree\nfour\nfive\nsix\nseven\neight\nnine\n")
        .assert()
        .success()
        .stdout("   2 two\n   5 five\n   8 eight\n");
}

#[test]
fn open_and_tail_relative_increments_work() {
    for (range, expected) in [("2~2", "two\nfour\n"), ("-3:~2", "three\nfive\n")] {
        bat()
            .arg(format!("--line-range={range}"))
            .write_stdin("one\ntwo\nthree\nfour\nfive\n")
            .assert()
            .success()
            .stdout(expected);
    }
}

#[test]
fn periodic_highlighting_matches_explicit_lines() {
    let input = "one\ntwo\nthree\nfour\nfive\n";
    let output = |highlights: &[&str]| {
        bat()
            .args([
                "--color=always",
                "--theme=ansi",
                "--style=plain",
                "--paging=never",
            ])
            .args(highlights)
            .write_stdin(input)
            .assert()
            .success()
            .get_output()
            .stdout
            .clone()
    };
    assert_eq!(
        output(&["--highlight-line=2~2"]),
        output(&["-H", "2", "-H", "4"])
    );
}

#[test]
fn ranges_union_without_duplicate_lines() {
    bat()
        .args(["--line-range=1~2", "--line-range=3:4"])
        .write_stdin("one\ntwo\nthree\nfour\nfive\n")
        .assert()
        .success()
        .stdout("one\nthree\nfour\nfive\n");
}

#[test]
fn zero_stride_fails_without_output() {
    for flag in ["--line-range", "--highlight-line"] {
        bat()
            .args([flag, "1~0", "test.txt"])
            .assert()
            .failure()
            .stdout("");
    }
}

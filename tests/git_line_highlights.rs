#![cfg(feature = "git")]
mod utils;
use std::path::Path;
use utils::command::bat;

fn repository() -> tempfile::TempDir {
    let dir = tempfile::tempdir().unwrap();
    std::fs::write(dir.path().join("example.txt"), "alpha\nbeta\ngamma\n").unwrap();
    for args in [vec!["init"], vec!["add", "example.txt"]] {
        assert!(std::process::Command::new("git")
            .args(args)
            .current_dir(dir.path())
            .status()
            .unwrap()
            .success());
    }
    dir
}

fn render(dir: &Path, args: &[&str]) -> Vec<u8> {
    bat()
        .current_dir(dir)
        .args([
            "--paging=never",
            "--decorations=always",
            "--color=always",
            "--theme=ansi",
            "--terminal-width=30",
        ])
        .args(args)
        .arg("example.txt")
        .assert()
        .success()
        .get_output()
        .stdout
        .clone()
}

#[test]
fn modified_and_added_lines_match_explicit_highlights() {
    let dir = repository();
    std::fs::write(
        dir.path().join("example.txt"),
        "alpha\nBETA\ngamma\ndelta\n",
    )
    .unwrap();
    assert_eq!(
        render(dir.path(), &["--style=changes-highlight"]),
        render(dir.path(), &["--style=plain", "-H2", "-H4"])
    );
    assert_eq!(
        render(dir.path(), &["--style=changes-highlight", "-H1"]),
        render(dir.path(), &["--style=plain", "-H1:2", "-H4"])
    );
}

#[test]
fn deletion_marks_adjacent_surviving_line() {
    let dir = repository();
    std::fs::write(dir.path().join("example.txt"), "beta\ngamma\n").unwrap();
    assert_eq!(
        render(dir.path(), &["--style=changes-highlight"]),
        render(dir.path(), &["--style=plain", "-H1"])
    );
}

#[test]
fn unchanged_files_and_colorless_output_remain_plain() {
    let dir = repository();
    assert_eq!(
        render(dir.path(), &["--style=changes-highlight"]),
        render(dir.path(), &["--style=plain"])
    );
    std::fs::write(dir.path().join("example.txt"), "changed\n").unwrap();
    assert_eq!(
        render(dir.path(), &["--style=changes-highlight", "--color=never"]),
        b"changed\n"
    );
    assert_eq!(
        render(
            dir.path(),
            &["--style=changes-highlight,-changes-highlight"]
        ),
        render(dir.path(), &["--style=plain"])
    );
}

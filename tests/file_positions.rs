use std::fs;
use tempfile::tempdir;

mod utils;
use utils::command::bat;

#[test]
fn file_position_keeps_all_output_when_paging_is_disabled() {
    let directory = tempdir().unwrap();
    let file = directory.path().join("a source file.rs");
    fs::write(&file, "first\nsecond\nthird\n").unwrap();
    let positioned = format!("{}:2", file.display());
    bat()
        .args(["--paging=never", "--color=never"])
        .arg(&positioned)
        .assert()
        .success()
        .stdout("first\nsecond\nthird\n");
    bat()
        .args(["--paging=never", "--literal-file-names"])
        .arg(positioned)
        .assert()
        .failure();
}

#[test]
fn file_position_preserves_language_detection_and_the_original_header_name() {
    let directory = tempdir().unwrap();
    let file = directory.path().join("source.rs");
    fs::write(&file, "fn main() {\n    let answer = 42;\n}\n").unwrap();
    let options = [
        "--paging=never",
        "--color=always",
        "--decorations=always",
        "--style=header,numbers",
        "--theme=ansi",
    ];
    let original = bat()
        .args(options)
        .arg(&file)
        .assert()
        .success()
        .get_output()
        .stdout
        .clone();
    bat()
        .args(options)
        .arg(format!("{}:2", file.display()))
        .assert()
        .success()
        .stdout(original);
}

#[cfg(windows)]
#[test]
fn existing_numeric_ntfs_streams_take_precedence_over_line_positions() {
    let directory = tempdir().unwrap();
    let file = directory.path().join("source.txt");
    fs::write(&file, "main stream\n").unwrap();
    let stream = format!("{}:2", file.display());
    fs::write(&stream, "numeric alternate stream\n").unwrap();
    bat()
        .args(["--paging=never", "--color=never"])
        .arg(stream)
        .assert()
        .success()
        .stdout("numeric alternate stream\n");
}

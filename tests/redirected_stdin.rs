#![cfg(any(target_os = "linux", target_os = "android"))]

mod utils;

use std::fs::File;
use std::io::{Seek, SeekFrom};
use std::process::Stdio;
use utils::command::{bat, bat_raw_command};

const OPTIONS: &[&str] = &[
    "--style=plain",
    "--color=always",
    "--theme=Monokai Extended",
    "--paging=never",
];
const JSON: &str = "{\"message\": \"hello\", \"count\": 42}\n";

fn redirected(file: File, args: &[&str]) -> std::process::Output {
    bat_raw_command()
        .args(OPTIONS)
        .args(args)
        .stdin(Stdio::from(file))
        .output()
        .unwrap()
}

#[test]
fn redirected_file_is_detected_without_consuming_or_reopening_input() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("input.json");
    std::fs::write(&path, format!("skip!\n{JSON}")).unwrap();
    let mut file = File::open(&path).unwrap();
    file.seek(SeekFrom::Start(6)).unwrap();
    let actual = redirected(file, &[]);
    let expected = bat()
        .args(OPTIONS)
        .args(["--language=json"])
        .write_stdin(JSON)
        .assert()
        .success()
        .get_output()
        .stdout
        .clone();
    assert!(actual.status.success());
    assert!(actual.stderr.is_empty());
    assert_eq!(actual.stdout, expected);
}

#[test]
fn explicit_filename_and_language_take_precedence() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("input.json");
    std::fs::write(&path, JSON).unwrap();
    for args in [["--file-name=input.txt"], ["--language=txt"]] {
        let actual = redirected(File::open(&path).unwrap(), &args);
        let expected = bat()
            .args(OPTIONS)
            .args(["--language=txt"])
            .write_stdin(JSON)
            .assert()
            .success()
            .get_output()
            .stdout
            .clone();
        assert!(actual.status.success());
        assert_eq!(actual.stdout, expected);
    }
}

#[test]
fn redirected_stdin_still_obeys_mappings_and_explicit_dash_inputs() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("input.custom");
    std::fs::write(&path, JSON).unwrap();
    let actual = redirected(
        File::open(&path).unwrap(),
        &["--map-syntax=*.custom:JSON", "-", "-"],
    );
    let expected = bat()
        .args(OPTIONS)
        .args(["--language=json"])
        .write_stdin(JSON)
        .assert()
        .success()
        .get_output()
        .stdout
        .clone();
    assert!(actual.status.success());
    assert_eq!(actual.stdout, expected);
}

#[test]
fn pipes_and_deleted_files_keep_the_stdin_fallback() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("input.json");
    std::fs::write(&path, JSON).unwrap();
    let file = File::open(&path).unwrap();
    std::fs::remove_file(&path).unwrap();
    let actual = redirected(file, &[]);
    let expected = bat()
        .args(OPTIONS)
        .write_stdin(JSON)
        .assert()
        .success()
        .get_output()
        .stdout
        .clone();
    assert!(actual.status.success());
    assert_eq!(actual.stdout, expected);
}

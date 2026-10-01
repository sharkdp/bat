use std::fs;
use std::path::Path;
use tempfile::tempdir;

mod utils;
use utils::command::bat;

fn output(file: &Path, extra: &[&str]) -> String {
    let bytes = bat()
        .args([
            "--paging=never",
            "--color=always",
            "--theme=ansi",
            "--style=plain",
            "--wrap=never",
            "--show-paths",
            "-l",
            "txt",
        ])
        .args(extra)
        .arg(file)
        .assert()
        .success()
        .get_output()
        .stdout
        .clone();
    String::from_utf8(bytes).unwrap()
}

#[test]
fn paths_are_relative_to_each_real_input_directory() {
    let dir = tempdir().unwrap();
    fs::create_dir(dir.path().join("nested")).unwrap();
    fs::write(dir.path().join("present.txt"), "exists").unwrap();
    let first = dir.path().join("first.txt");
    let second = dir.path().join("nested/second.txt");
    let input = "\"present.txt\" ./missing.txt\n";
    fs::write(&first, input).unwrap();
    fs::write(&second, input).unwrap();
    assert!(output(&first, &[]).contains("\x1b[4mpresent.txt\x1b[0m"));
    assert_eq!(output(&second, &[]), input);
    assert!(output(&first, &["--file-name=elsewhere/input.rs"]).contains("\x1b[4mpresent.txt"));
}

#[test]
fn quoted_unicode_paths_with_spaces_and_directories_are_recognized() {
    let dir = tempdir().unwrap();
    fs::create_dir(dir.path().join("some folder 界")).unwrap();
    fs::write(dir.path().join("some folder 界/name.txt"), "exists").unwrap();
    let file = dir.path().join("source.txt");
    fs::write(&file, "\"./some folder 界/name.txt\" './some folder 界'\n").unwrap();
    let out = output(&file, &[]);
    assert!(
        out.contains("\x1b[4m./some folder 界/name.txt\x1b[0m"),
        "{out:?}"
    );
    assert!(out.contains("\x1b[4m./some folder 界\x1b[0m"), "{out:?}");
}

#[test]
fn urls_expansions_missing_paths_and_unclosed_quotes_remain_unmarked() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("exists.txt"), "exists").unwrap();
    let file = dir.path().join("source.txt");
    let input = "https://example.test/exists.txt \"file:///exists.txt\" $HOME/exists.txt ./missing.txt\n\"./exists.txt\n";
    fs::write(&file, input).unwrap();
    assert_eq!(output(&file, &[]), input);
}

#[test]
fn stdin_uses_working_directory_even_with_a_display_filename() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("exists.txt"), "exists").unwrap();
    let out = bat()
        .current_dir(dir.path())
        .args([
            "--paging=never",
            "--color=always",
            "--theme=ansi",
            "--style=plain",
            "--show-paths",
            "--wrap=never",
            "-l",
            "txt",
            "--file-name=elsewhere/source.txt",
        ])
        .write_stdin("./exists.txt\n")
        .assert()
        .success()
        .get_output()
        .stdout
        .clone();
    assert_eq!(
        String::from_utf8(out).unwrap(),
        "\x1b[4m./exists.txt\x1b[0m\n"
    );
}

#[test]
fn color_never_and_disabled_option_preserve_literal_output() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("exists.txt"), "exists").unwrap();
    let file = dir.path().join("source.txt");
    fs::write(&file, "./exists.txt\n").unwrap();
    assert_eq!(
        output(&file, &["--color=never", "--show-paths"]),
        "./exists.txt\n"
    );
    bat()
        .args([
            "--paging=never",
            "--color=always",
            "--theme=ansi",
            "--style=plain",
            "-l",
            "txt",
        ])
        .arg(&file)
        .assert()
        .success()
        .stdout("./exists.txt\n");
}

#[test]
fn library_option_marks_paths_in_file_inputs() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("exists.txt"), "exists").unwrap();
    let file = dir.path().join("source.txt");
    fs::write(&file, "./exists.txt\n").unwrap();
    let mut out = String::new();
    bat::PrettyPrinter::new()
        .input_file(file)
        .language("txt")
        .theme("ansi")
        .colored_output(true)
        .show_paths(true)
        .print_with_writer(Some(&mut out))
        .unwrap();
    assert!(out.contains("\x1b[4m./exists.txt\x1b[0m"), "{out:?}");
}

#[cfg(windows)]
#[test]
fn native_windows_drive_and_relative_paths_are_supported() {
    let dir = tempdir().unwrap();
    let existing = dir.path().join("exists.txt");
    fs::write(&existing, "exists").unwrap();
    let file = dir.path().join("source.txt");
    let absolute = existing.to_str().unwrap();
    fs::write(&file, format!("\"{absolute}\" .\\exists.txt\n")).unwrap();
    let out = output(&file, &[]);
    assert!(
        out.contains(&format!("\x1b[4m{absolute}\x1b[0m")),
        "{out:?}"
    );
    assert!(out.contains("\x1b[4m.\\exists.txt\x1b[0m"), "{out:?}");
}

#[cfg(unix)]
#[test]
fn broken_symlinks_are_not_present_paths() {
    let dir = tempdir().unwrap();
    std::os::unix::fs::symlink("missing", dir.path().join("broken")).unwrap();
    let file = dir.path().join("source.txt");
    fs::write(&file, "./broken\n").unwrap();
    assert_eq!(output(&file, &[]), "./broken\n");
}

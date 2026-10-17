#![cfg(all(unix, feature = "paging"))]

use std::fs;
use std::io::Write;
use std::os::unix::fs::PermissionsExt;
use std::path::PathBuf;
use std::process::Stdio;
use std::time::{Duration, Instant};
use tempfile::{tempdir, TempDir};
use wait_timeout::ChildExt;

mod utils;
use utils::command::{bat, bat_raw_command};

struct Pager {
    directory: TempDir,
    arguments: PathBuf,
    position: PathBuf,
    command: String,
}

impl Pager {
    fn new(name: &str, version: &str, body: &str) -> Self {
        let directory = tempdir().unwrap();
        let binary = directory.path().join(name);
        fs::write(&binary, format!(
            "#!/bin/sh\nif [ \"$1\" = --version ]; then printf '%s\\n' '{version}'; exit 0; fi\nprintf '%s\\n' \"$@\" > \"$BAT_TEST_ARGS\"\nprintf '%s\\n%s\\n' \"${{BAT_SCROLL_LINE-unset}}\" \"${{BAT_SCROLL_POSITION-unset}}\" > \"$BAT_TEST_POSITION\"\n{body}\n"
        )).unwrap();
        fs::set_permissions(&binary, fs::Permissions::from_mode(0o700)).unwrap();
        let arguments = directory.path().join("arguments");
        let position = directory.path().join("position");
        Self {
            command: shell_words::quote(binary.to_str().unwrap()).into_owned(),
            directory,
            arguments,
            position,
        }
    }

    fn less() -> Self {
        Self::new("less", "less 590", "cat")
    }

    fn raw(&self) -> std::process::Command {
        let mut command = bat_raw_command();
        command
            .args([
                "--paging=always",
                "--style=plain",
                "--color=never",
                "--pager",
                &self.command,
            ])
            .env_remove("LESS")
            .env("BAT_TEST_ARGS", &self.arguments)
            .env("BAT_TEST_POSITION", &self.position);
        command
    }

    fn command(&self) -> assert_cmd::Command {
        assert_cmd::Command::from_std(self.raw())
    }
    fn args(&self) -> Vec<String> {
        fs::read_to_string(&self.arguments)
            .unwrap()
            .lines()
            .map(str::to_owned)
            .collect()
    }
    fn position(&self) -> String {
        fs::read_to_string(&self.position).unwrap()
    }
}

#[test]
fn scroll_mapping_includes_headers_wrapping_unicode_and_range_snips() {
    let pager = Pager::less();
    let input = "first line\nthis is a deliberately long wrapping line with 界界 and a tab\there\nthird\nfourth\nTARGET fifth\nsixth\n";
    for options in [
        vec!["--style=full"],
        vec!["--style=numbers,grid", "--wrap=character"],
        vec![
            "--style=numbers,snip",
            "--line-range=1:2",
            "--line-range=5:",
        ],
        vec!["--style=plain", "--wrap=never"],
    ] {
        let mut command = pager.command();
        command
            .args([
                "--decorations=always",
                "--terminal-width=32",
                "--tabs=4",
                "--file-name=example.txt",
            ])
            .args(&options)
            .args(["--scroll-to=5"])
            .write_stdin(input);
        let actual = command.assert().success().get_output().stdout.clone();
        let mut original = bat();
        original
            .args([
                "--paging=never",
                "--color=never",
                "--decorations=always",
                "--terminal-width=32",
                "--tabs=4",
                "--file-name=example.txt",
            ])
            .args(&options)
            .write_stdin(input);
        assert_eq!(actual, original.assert().success().get_output().stdout);
        let target = String::from_utf8(actual)
            .unwrap()
            .lines()
            .position(|line| line.contains("TARGET fifth"))
            .unwrap()
            + 1;
        assert!(
            pager.args().contains(&format!("+{target}g")),
            "{:?}",
            pager.args()
        );
        assert_eq!(pager.position(), format!("{target}\ntop\n"));
    }
}

#[test]
fn excluded_or_squeezed_target_uses_the_next_printed_line() {
    let pager = Pager::less();
    pager
        .command()
        .args(["--scroll-to=2", "--line-range=1:1", "--line-range=4:"])
        .write_stdin("one\ntwo\nthree\nfour\nfive\n")
        .assert()
        .success()
        .stdout("one\nfour\nfive\n");
    assert_eq!(pager.position(), "2\ntop\n");
    pager
        .command()
        .args(["--scroll-to=3", "--squeeze-blank"])
        .write_stdin("one\n\n\n\nfive\n")
        .assert()
        .success()
        .stdout("one\n\nfive\n");
    assert_eq!(pager.position(), "3\ntop\n");
}

#[test]
fn center_highlight_finds_the_first_visible_highlight() {
    let pager = Pager::less();
    pager
        .command()
        .args([
            "--center-highlight",
            "--highlight-line=2",
            "--highlight-line=5:6",
            "--line-range=4:",
        ])
        .write_stdin("one\ntwo\nthree\nfour\nfive\nsix\n")
        .assert()
        .success()
        .stdout("four\nfive\nsix\n");
    assert_eq!(pager.position(), "2\ncenter\n");
    assert!(pager.args().contains(&"-j.5".into()));
    assert!(pager.args().contains(&"+2g".into()));
}

#[test]
fn a_squeezed_highlight_does_not_scroll_to_an_unhighlighted_line() {
    let pager = Pager::less();
    pager
        .command()
        .args([
            "--center-highlight",
            "--highlight-line=3",
            "--squeeze-blank",
        ])
        .write_stdin("one\n\n\nfour\n")
        .assert()
        .success();
    assert_eq!(pager.position(), "1\ncenter\n");
}

#[test]
fn empty_and_beyond_eof_targets_do_not_request_a_nonexistent_row() {
    let pager = Pager::less();
    for input in ["", "one\ntwo\n", "last without newline"] {
        pager
            .command()
            .arg("--scroll-to=999999")
            .write_stdin(input)
            .assert()
            .success()
            .stdout(input);
        assert_eq!(pager.position(), "unset\nend\n");
        assert!(pager.args().contains(&"+G".into()));
    }
    pager
        .command()
        .args(["--center-highlight", "--highlight-line=999999"])
        .write_stdin("one\ntwo\n")
        .assert()
        .success();
    assert_eq!(pager.position(), "1\ncenter\n");
}

#[test]
fn custom_wrappers_receive_position_without_less_options() {
    let pager = Pager::new("wrapper", "wrapper 1", "cat");
    pager
        .command()
        .args(["--scroll-to=2"])
        .write_stdin("one\ntwo\n")
        .assert()
        .success()
        .stdout("one\ntwo\n");
    assert_eq!(pager.position(), "2\ntop\n");
    assert!(pager.args().iter().all(|arg| arg.is_empty()));
}

#[test]
fn no_paging_preserves_raw_output_and_does_not_run_a_pager() {
    let pager = Pager::less();
    pager
        .command()
        .args(["--paging=never", "--scroll-to=2"])
        .write_stdin(b"raw\xFF\none\n".as_slice())
        .assert()
        .success()
        .stdout(b"raw\xFF\none\n".as_slice());
    assert!(!pager.arguments.exists());
    pager
        .command()
        .args(["--pager=", "--scroll-to=2"])
        .write_stdin("one\ntwo\n")
        .assert()
        .success()
        .stdout("one\ntwo\n");
    assert!(!pager.arguments.exists());
}

#[test]
fn invalid_targets_and_unsupported_pagers_are_rejected_before_startup() {
    let pager = Pager::less();
    for options in [
        vec!["--scroll-to=0", "test.txt"],
        vec!["--scroll-to=2", "test.txt", "test.txt"],
        vec![
            "--scroll-to=2",
            "--center-highlight",
            "--highlight-line=2",
            "test.txt",
        ],
        vec!["--center-highlight", "test.txt"],
        vec!["--scroll-to=2", "--pager=builtin", "test.txt"],
    ] {
        pager.command().args(options).assert().failure();
        assert!(!pager.arguments.exists());
    }
    let busybox = Pager::new("less", "BusyBox v1.35.0", "cat");
    let script = busybox.directory.path().join("less");
    fs::write(
        &script,
        fs::read_to_string(&script)
            .unwrap()
            .replace("exit 0; fi", "exit 1; fi")
            .replace("'BusyBox v1.35.0';", "'BusyBox v1.35.0' >&2;"),
    )
    .unwrap();
    busybox
        .command()
        .args(["--scroll-to=2", "test.txt"])
        .assert()
        .failure();
    assert!(!busybox.arguments.exists());
}

#[test]
fn large_prefix_spills_and_is_preserved_byte_for_byte() {
    let pager = Pager::less();
    let input = (0..3000)
        .map(|n| format!("line {n:04}: a longer prefix with unicode 界 and tabs\tend\n"))
        .collect::<String>();
    assert!(input.len() > 64 * 1024);
    pager
        .command()
        .arg("--scroll-to=3000")
        .write_stdin(input.clone())
        .assert()
        .success()
        .stdout(input);
    assert_eq!(pager.position(), "3000\ntop\n");
}

#[test]
fn pager_starts_at_target_before_stdin_reaches_eof() {
    let pager = Pager::less();
    let mut child = pager
        .raw()
        .arg("--scroll-to=2")
        .stdin(Stdio::piped())
        .stdout(Stdio::null())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();
    let mut input = child.stdin.take().unwrap();
    input.write_all(b"one\ntwo\nthree\n").unwrap();
    input.flush().unwrap();
    let deadline = Instant::now() + Duration::from_secs(5);
    while !pager.position.exists() && Instant::now() < deadline {
        std::thread::sleep(Duration::from_millis(20));
    }
    if !pager.position.exists() {
        child.kill().unwrap();
        panic!("pager did not start while stdin remained open");
    }
    assert!(child.try_wait().unwrap().is_none());
    assert_eq!(pager.position(), "2\ntop\n");
    input.write_all(b"four\nfive\n").unwrap();
    drop(input);
    assert!(child
        .wait_timeout(Duration::from_secs(5))
        .unwrap()
        .unwrap()
        .success());
}

#[test]
fn an_early_pager_exit_preserves_broken_pipe_handling() {
    let pager = Pager::new("less", "less 590", "exit 0");
    let input = pager.directory.path().join("large.txt");
    fs::write(
        &input,
        "a long enough line to exceed the pipe buffer\n".repeat(100000),
    )
    .unwrap();
    pager
        .command()
        .arg("--scroll-to=2")
        .arg(input)
        .assert()
        .success()
        .stderr("");
}

#[test]
fn file_line_syntax_opens_the_real_file_and_centers_the_requested_line() {
    let pager = Pager::less();
    let file = pager.directory.path().join("a file.rs");
    fs::write(&file, "fn main() {\n    // TARGET\n}\n").unwrap();
    let position = format!("{}:2", file.display());
    pager
        .command()
        .arg(&position)
        .assert()
        .success()
        .stdout("fn main() {\n    // TARGET\n}\n");
    assert_eq!(pager.position(), "2\ncenter\n");
    pager
        .command()
        .arg(&position)
        .arg("--scroll-to=1")
        .assert()
        .success();
    assert_eq!(pager.position(), "1\ntop\n");
    pager
        .command()
        .arg(&position)
        .args(["--center-highlight", "--highlight-line=3"])
        .assert()
        .success();
    assert_eq!(pager.position(), "3\ncenter\n");
    pager
        .command()
        .arg(&position)
        .arg("--literal-file-names")
        .assert()
        .failure();
}

#[test]
fn literal_colon_names_and_broken_symlinks_are_not_reinterpreted() {
    let pager = Pager::less();
    let file = pager.directory.path().join("source");
    fs::write(&file, "original\n").unwrap();
    let literal = pager.directory.path().join("source:2");
    fs::write(&literal, "literal\n").unwrap();
    pager
        .command()
        .arg(&literal)
        .assert()
        .success()
        .stdout("literal\n");
    assert_eq!(pager.position(), "unset\nunset\n");
    fs::remove_file(&literal).unwrap();
    std::os::unix::fs::symlink(pager.directory.path().join("missing"), &literal).unwrap();
    pager.command().arg(&literal).assert().failure();
}

#[test]
fn malformed_and_ambiguous_file_positions_remain_errors() {
    let pager = Pager::less();
    let file = pager.directory.path().join("source");
    fs::write(&file, "one\ntwo\n").unwrap();
    for suffix in [
        ":0",
        ":-1",
        ":+2",
        ":two",
        ":",
        ":999999999999999999999999999999999",
    ] {
        pager
            .command()
            .arg(format!("{}{suffix}", file.display()))
            .assert()
            .failure();
    }
    pager
        .command()
        .arg(format!("{}:2", file.display()))
        .arg(&file)
        .assert()
        .failure();
    assert!(!pager.arguments.exists());
}

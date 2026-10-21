mod utils;

#[cfg(unix)]
use std::time::Duration;
use utils::command::bat;

fn child_bat(args: &str) -> String {
    format!(
        "{} --no-config --paging=never {args}",
        shell_words::quote(env!("CARGO_BIN_EXE_bat"))
    )
}

#[cfg(unix)]
fn notebook_filter() -> String {
    format!(
        "python3 {}",
        shell_words::quote(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/examples/notebook-preview.py"
        ))
    )
}

#[test]
#[cfg(unix)]
#[ignore = "requires Python 3; the host CI job runs notebook previews explicitly"]
fn notebook_preview_preserves_cells_outputs_and_embedded_fences() {
    let expected = "## Cell 1\n\n# Notebook café\nSaved analysis\n\n## Cell 2\n\n````python\nprint('hello')\n# embedded ``` fence\n````\n\n```text\nhello world\n```\n\n```text\n42\n```\n\n```text\n[Output: image/png]\n```\n\n```text\nTraceback line 1\nValueError: bad\n```\n\n## Cell 3\n\n````text\nraw ``` fence\n````\n";
    bat()
        .args([
            "--process",
            &notebook_filter(),
            "--language=markdown",
            "--style=plain",
            "--color=never",
            "notebook-preview.ipynb",
        ])
        .assert()
        .success()
        .stdout(expected)
        .stderr("");
}

#[test]
#[cfg(unix)]
#[ignore = "requires Python 3; the host CI job runs notebook previews explicitly"]
fn notebook_preview_handles_missing_metadata_and_invalid_inputs() {
    use predicates::prelude::*;

    bat()
        .args([
            "--process",
            &notebook_filter(),
            "--language=markdown",
            "--style=plain",
            "--color=never",
        ])
        .write_stdin(r#"{"nbformat":4,"cells":[{"cell_type":"code","source":"x"}]}"#)
        .assert()
        .success()
        .stdout("## Cell 1\n\n```text\nx\n```\n")
        .stderr("");
    for invalid in [
        "{",
        r#"{"nbformat":3,"cells":[]}"#,
        r#"{"nbformat":4,"cells":{}}"#,
        r#"{"nbformat":4,"cells":[{"cell_type":"markdown","source":"valid first cell"},{"cell_type":"code","source":[7]}]}"#,
    ] {
        bat()
            .args([
                "--process",
                &notebook_filter(),
                "--style=plain",
                "--color=never",
            ])
            .write_stdin(invalid)
            .assert()
            .failure()
            .stdout("")
            .stderr(predicate::str::contains("notebook-preview:"));
    }
}

#[test]
#[cfg(unix)]
fn binary_strings_filter_reads_bytes_without_executing_the_input() {
    use predicates::prelude::*;

    let dir = tempfile::tempdir().unwrap();
    let file = dir.path().join("binary input");
    std::fs::write(
        &file,
        b"\x7fELF\0\x01\x02printable payload\0\xff\0second marker\0",
    )
    .unwrap();
    bat()
        .args([
            "--process",
            "strings -a",
            "--language=txt",
            "--decorations=always",
            "--style=header",
            "--terminal-width=200",
            "--color=never",
        ])
        .arg(&file)
        .assert()
        .success()
        .stdout(
            predicate::str::contains("binary input")
                .and(predicate::str::contains("printable payload\n"))
                .and(predicate::str::contains("second marker\n")),
        )
        .stderr("");
    bat()
        .args([
            "--process",
            "strings -a",
            "--language=txt",
            "--style=plain",
            "--color=never",
        ])
        .write_stdin(b"\0stdin marker\0" as &[u8])
        .assert()
        .success()
        .stdout("stdin marker\n");
}

#[test]
fn filters_run_per_file_and_retain_names_and_syntax() {
    let dir = tempfile::tempdir().unwrap();
    let first = dir.path().join("first file.json");
    let second = dir.path().join("second.json");
    std::fs::write(&first, "{}\n").unwrap();
    std::fs::write(&second, "[]\n").unwrap();
    let output = bat()
        .args([
            "--decorations=always",
            "--style=header",
            "--terminal-width=200",
            "--color=always",
            "--process",
        ])
        .arg(child_bat("--color=never --style=plain"))
        .args([&first, &second])
        .assert()
        .success()
        .get_output()
        .stdout
        .clone();
    let text = String::from_utf8(output).unwrap();
    assert!(text.contains("first file.json"), "{text:?}");
    assert!(text.contains("second.json"), "{text:?}");
    assert!(text.contains("\x1b["), "{text:?}");
    assert_eq!(std::fs::read_to_string(first).unwrap(), "{}\n");
    let numbered = bat()
        .args(["--color=never", "--style=plain", "--process"])
        .arg(child_bat(
            "--color=never --decorations=always --style=numbers",
        ))
        .args([&second, &second])
        .assert()
        .success()
        .get_output()
        .stdout
        .clone();
    assert_eq!(numbered, b"   1 []\n   1 []\n");
}

#[test]
fn stdin_and_explicit_names_are_filtered() {
    bat()
        .args(["--color=never", "--style=plain", "--process"])
        .arg(child_bat(
            "--color=never --decorations=always --style=numbers",
        ))
        .args(["-", "-"])
        .write_stdin("one\ntwo\n")
        .assert()
        .success()
        .stdout("   1 one\n   2 two\n");
    let output = bat()
        .args([
            "--color=never",
            "--style=header",
            "--terminal-width=200",
            "--decorations=always",
            "--file-name=logical.json",
            "--process",
        ])
        .arg(child_bat("--color=never --style=plain"))
        .write_stdin("{}\n")
        .assert()
        .success()
        .get_output()
        .stdout
        .clone();
    assert!(String::from_utf8(output).unwrap().contains("logical.json"));
}

#[test]
fn invalid_commands_missing_files_and_directories_fail_without_content() {
    for command in [
        "",
        "''",
        "'unterminated",
        "bat-no-such-process-command-2289",
    ] {
        bat()
            .args(["--process", command])
            .write_stdin("source\n")
            .assert()
            .failure()
            .stdout("");
    }
    let dir = tempfile::tempdir().unwrap();
    for input in [dir.path().to_path_buf(), dir.path().join("missing-file")] {
        bat()
            .arg("--process")
            .arg(child_bat("--style=plain"))
            .arg(input)
            .assert()
            .failure()
            .stdout("");
    }
}

#[test]
fn replacing_contents_preserves_library_input_identity() {
    let input = bat::input::Input::stdin().with_reader(Box::new(&b"replacement\n"[..]));
    assert!(input.is_stdin());
    assert_eq!(input.description().title(), "STDIN");
    let input =
        bat::input::Input::ordinary_file("nonexistent.json").with_reader(Box::new(&b"{}\n"[..]));
    assert!(!input.is_stdin());
    let mut output = String::new();
    let config = bat::config::Config {
        loop_through: true,
        ..Default::default()
    };
    let assets = bat::assets::HighlightingAssets::from_binary();
    assert!(bat::controller::Controller::new(&config, &assets)
        .run(
            vec![input],
            Some(&mut bat::output::OutputHandle::FmtWrite(&mut output))
        )
        .unwrap());
    assert_eq!(output, "{}\n");
}

#[cfg(unix)]
#[test]
fn process_failures_and_stderr_are_reported() {
    bat()
        .args(["--process", "sh -c 'printf warning >&2; exit 7'"])
        .write_stdin("source\n")
        .assert()
        .failure()
        .stdout("")
        .stderr(predicates::str::contains("warning"));
    bat()
        .args(["--process", "sh -c 'exit 0'"])
        .write_stdin("source\n")
        .assert()
        .success()
        .stdout("");
    // The command is split into arguments, not evaluated by a shell.
    bat()
        .args([
            "--style=plain",
            "--color=never",
            "--process",
            "printf '%s' '$HOME; | literal'",
        ])
        .write_stdin("")
        .assert()
        .success()
        .stdout("$HOME; | literal");
}

#[cfg(unix)]
#[test]
fn stopping_at_a_line_range_terminates_and_reaps_the_filter() {
    let dir = tempfile::tempdir().unwrap();
    let pid_path = dir.path().join("pid");
    let script = format!(
        "printf '%s' \"$$\" > {}; while :; do printf 'line\\n'; done",
        shell_words::quote(pid_path.to_str().unwrap())
    );
    let command = format!("sh -c {}", shell_words::quote(&script));
    bat()
        .timeout(Duration::from_secs(10))
        .args([
            "--process",
            &command,
            "--line-range=1:1",
            "--style=plain",
            "--color=never",
        ])
        .write_stdin("")
        .assert()
        .success()
        .stdout("line\n");
    let pid = std::fs::read_to_string(pid_path).unwrap();
    let result = std::process::Command::new("kill")
        .args(["-0", pid.trim()])
        .stderr(std::process::Stdio::null())
        .status()
        .unwrap();
    assert!(!result.success(), "filter process {pid} was left running");
}

#[cfg(unix)]
#[test]
fn filters_detect_input_output_cycles() {
    use std::fs::OpenOptions;
    let dir = tempfile::tempdir().unwrap();
    let file = dir.path().join("source");
    std::fs::write(&file, "source\n").unwrap();
    let output = utils::command::bat_raw_command()
        .args(["--process", "cat"])
        .arg(&file)
        .stdout(OpenOptions::new().append(true).open(&file).unwrap())
        .output()
        .unwrap();
    assert!(!output.status.success());
    assert!(String::from_utf8(output.stderr)
        .unwrap()
        .contains("IO circle detected"));
    assert_eq!(std::fs::read_to_string(file).unwrap(), "source\n");
}

#[cfg(feature = "git")]
#[test]
fn process_and_diff_cannot_be_combined() {
    bat()
        .args(["--process", "cat", "--diff"])
        .write_stdin("source\n")
        .assert()
        .failure()
        .stdout("");
}

#[cfg(feature = "lessopen")]
#[test]
fn process_and_lessopen_cannot_be_combined() {
    bat()
        .args(["--process", "cat", "--lessopen"])
        .write_stdin("source\n")
        .assert()
        .failure()
        .stdout("");
}

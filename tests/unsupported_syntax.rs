mod utils;
use utils::command::bat;

#[test]
fn unsupported_text_and_binary_are_silent_even_with_headers() {
    for input in [b"unrecognized input\n".as_slice(), b"PK\x03\x04binary\n"] {
        bat()
            .args([
                "--fail-if-syntax-unsupported",
                "--decorations=always",
                "--style=full",
                "--color=always",
            ])
            .write_stdin(input)
            .assert()
            .failure()
            .stdout("")
            .stderr("");
    }
    bat()
        .args(["--fail-if-syntax-unsupported", "--file-name=plain.txt"])
        .write_stdin("plain\n")
        .assert()
        .failure()
        .stdout("")
        .stderr("");
}

#[test]
fn explicit_detected_and_fallback_syntaxes_are_accepted_when_piped() {
    for args in [
        vec!["-l", "json"],
        vec!["--file-name=example.json"],
        vec!["--fallback-syntax=json"],
        vec!["--file-name=example.custom", "-m", "*.custom:JSON"],
    ] {
        bat()
            .arg("--fail-if-syntax-unsupported")
            .args(args)
            .write_stdin("{}\n")
            .assert()
            .success()
            .stdout("{}\n")
            .stderr("");
    }
    bat()
        .arg("--fail-if-syntax-unsupported")
        .write_stdin("#!/bin/sh\necho hello\n")
        .assert()
        .success()
        .stdout("#!/bin/sh\necho hello\n")
        .stderr("");
}

#[test]
fn invalid_languages_and_missing_files_still_report_errors() {
    bat()
        .args(["--fail-if-syntax-unsupported", "-l", "not-a-language"])
        .write_stdin("input\n")
        .assert()
        .failure()
        .stderr(predicates::str::contains("unknown syntax"));
    bat()
        .args([
            "--fail-if-syntax-unsupported",
            "missing-file-without-syntax.xyz",
        ])
        .assert()
        .failure()
        .stderr(predicates::str::contains("missing-file-without-syntax.xyz"));
}

#[test]
fn mixed_inputs_print_supported_files_and_fail_for_unsupported_ones() {
    let dir = tempfile::tempdir().unwrap();
    let known = dir.path().join("known.json");
    let unknown = dir.path().join("unknown.custom");
    std::fs::write(&known, "{}\n").unwrap();
    std::fs::write(&unknown, "unknown\n").unwrap();
    bat()
        .arg("--fail-if-syntax-unsupported")
        .arg(unknown)
        .arg(known)
        .assert()
        .failure()
        .stdout("{}\n")
        .stderr("");
}

#[test]
#[cfg(feature = "paging")]
fn preprocess_mode_does_not_launch_a_pager() {
    bat()
        .args([
            "--fail-if-syntax-unsupported",
            "--pager=bat",
            "--paging=always",
        ])
        .write_stdin("unknown\n")
        .assert()
        .failure()
        .stdout("")
        .stderr("");
    bat()
        .args([
            "--fail-if-syntax-unsupported",
            "--pager=bat",
            "--paging=always",
            "-ljson",
        ])
        .write_stdin("{}\n")
        .assert()
        .success()
        .stdout("{}\n")
        .stderr("");
}

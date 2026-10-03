#![cfg(all(unix, feature = "paging"))]

mod utils;

use std::os::unix::fs::PermissionsExt;
use utils::command::bat;

fn mock_pager(name: &str) -> (tempfile::TempDir, String) {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join(name);
    std::fs::write(&path, "#!/bin/sh\nif [ \"$1\" = --version ]; then echo 'less 590'; else printf '[%s]\\n' \"$@\"; cat >/dev/null; fi\n").unwrap();
    std::fs::set_permissions(&path, std::fs::Permissions::from_mode(0o755)).unwrap();
    (dir, path.to_string_lossy().into_owned())
}

#[test]
fn append_literal_arguments_to_configured_pager() {
    let (_dir, pager) = mock_pager("pager");
    let output = bat()
        .env("BAT_PAGER", format!("{pager} existing"))
        .args([
            "--paging=always",
            "--pager-arg=+10",
            "--pager-arg=two words",
            "--pager-arg=$(literal)",
        ])
        .write_stdin("input\n")
        .assert()
        .success()
        .get_output()
        .stdout
        .clone();
    assert_eq!(
        String::from_utf8(output).unwrap(),
        "[existing]\n[+10]\n[two words]\n[$(literal)]\n"
    );
}

#[test]
fn less_retains_automatic_options_before_extra_arguments() {
    let (_dir, pager) = mock_pager("less");
    let output = bat()
        .arg(format!("--pager={pager}"))
        .args(["--paging=always", "--wrap=never", "--pager-arg=+42"])
        .write_stdin("input\n")
        .assert()
        .success()
        .get_output()
        .stdout
        .clone();
    let output = String::from_utf8(output).unwrap();
    assert!(output.starts_with("[-R]\n[-S]\n[-K]\n"), "{output}");
    assert!(output.ends_with("[+42]\n"), "{output}");
}

#[test]
fn explicit_less_arguments_are_not_replaced() {
    let (_dir, pager) = mock_pager("less");
    let output = bat()
        .arg(format!("--pager={pager} -RF"))
        .args(["--paging=always", "--pager-arg=-S"])
        .write_stdin("input\n")
        .assert()
        .success()
        .get_output()
        .stdout
        .clone();
    let output = String::from_utf8(output).unwrap();
    assert!(output.starts_with("[-RF]\n"), "{output}");
    assert!(output.ends_with("[-S]\n"), "{output}");
    assert!(!output.contains("[-R]\n"), "{output}");
}

#[test]
fn builtin_pager_rejects_extra_arguments_but_disabled_paging_ignores_them() {
    bat()
        .args(["--pager=builtin", "--paging=always", "--pager-arg=+1"])
        .write_stdin("input\n")
        .assert()
        .failure()
        .stderr(predicates::str::contains("built-in pager does not accept"));
    bat()
        .args([
            "--pager=builtin",
            "--paging=never",
            "--pager-arg=+1",
            "--style=plain",
        ])
        .write_stdin("input\n")
        .assert()
        .success()
        .stdout("input\n");
}

#[test]
fn help_passes_extra_arguments_to_the_pager() {
    let (_dir, pager) = mock_pager("pager");
    bat()
        .arg(format!("--pager={pager} existing"))
        .args(["--help", "--paging=always", "--pager-arg=extra"])
        .assert()
        .success()
        .stdout("[existing]\n[extra]\n");
}

#![cfg(feature = "paging")]
mod utils;

#[test]
fn reservation_is_repeatable_and_does_not_force_paging_when_disabled() {
    utils::command::bat()
        .args([
            "--paging=never",
            "--paging-reserve=3",
            "--paging-reserve=0",
            "--style=plain",
        ])
        .write_stdin("hello\n")
        .assert()
        .success()
        .stdout("hello\n");
}

#[test]
fn rejects_negative_and_out_of_range_reservations() {
    for value in ["-1", "65536", "abc"] {
        utils::command::bat()
            .arg(format!("--paging-reserve={value}"))
            .write_stdin("hello\n")
            .assert()
            .failure();
    }
}

#[cfg(unix)]
mod unix {
    use bat::{output::OutputType, PagingMode, WrappingMode};
    use std::{fs, os::unix::fs::PermissionsExt};

    fn pager(version: &str) -> (tempfile::TempDir, String) {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("less");
        let script = format!("#!/bin/sh\nif [ \"$1\" = '--version' ]; then printf '%s\\n' '{version}'; exit 0; fi\nprintf '%s\\n' \"${{LESS_LINES-unset}}\" \"$@\" > '{}'; exit 0\n", dir.path().join("received").display());
        fs::write(&path, script).unwrap();
        fs::set_permissions(&path, fs::Permissions::from_mode(0o755)).unwrap();
        let command = format!("'{}' -R", path.display());
        (dir, command)
    }

    #[test]
    fn automatic_reservation_sets_child_height_and_enables_custom_less_exit() {
        let (dir, command) = pager("less 632 (test)");
        drop(
            OutputType::from_mode_with_reserve(
                PagingMode::QuitIfOneScreen,
                WrappingMode::Character,
                Some(&command),
                4,
            )
            .unwrap(),
        );
        let received = fs::read_to_string(dir.path().join("received")).unwrap();
        assert!(
            received.starts_with("-4\n") || received.starts_with("1\n"),
            "{received}"
        );
        assert!(received.lines().any(|arg| arg == "-R"));
        assert!(received.lines().any(|arg| arg == "-F"));
    }

    #[test]
    fn old_and_unrecognized_less_versions_report_an_actionable_error() {
        for version in ["less 590 (test)", "BusyBox v1.36", "custom pager"] {
            let (dir, command) = pager(version);
            let error = OutputType::from_mode_with_reserve(
                PagingMode::QuitIfOneScreen,
                WrappingMode::Character,
                Some(&command),
                4,
            )
            .unwrap_err();
            assert!(error.to_string().contains("less 632 or newer"));
            assert!(!dir.path().join("received").exists());
        }
    }

    #[test]
    fn forced_paging_ignores_reservation_and_keeps_custom_arguments() {
        let (dir, command) = pager("less 590 (test)");
        drop(
            OutputType::from_mode_with_reserve(
                PagingMode::Always,
                WrappingMode::Character,
                Some(&command),
                4,
            )
            .unwrap(),
        );
        let received = fs::read_to_string(dir.path().join("received")).unwrap();
        assert!(!received.lines().any(|arg| arg == "-F"));
        assert!(received.lines().any(|arg| arg == "-R"));
    }
}

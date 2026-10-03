#![cfg(target_os = "linux")]

mod utils;

use std::os::unix::fs::PermissionsExt;
use std::path::Path;
use std::time::Duration;
use utils::command::bat;

fn install_gsettings(directory: &Path, body: &str) {
    let path = directory.join("gsettings");
    std::fs::write(
        &path,
        format!(
            "#!/bin/sh\n[ \"$#\" = 3 ] && [ \"$1\" = get ] && \
             [ \"$2\" = org.gnome.desktop.interface ] && \
             [ \"$3\" = color-scheme ] || exit 1\n{body}\n"
        ),
    )
    .unwrap();
    std::fs::set_permissions(path, std::fs::Permissions::from_mode(0o755)).unwrap();
}

fn render(directory: &Path, theme: &str) -> Vec<u8> {
    bat()
        .env("PATH", directory)
        .args([
            "--style=plain",
            "--paging=never",
            "--color=always",
            "--language=json",
            "--theme-dark=Monokai Extended",
            "--theme-light=GitHub",
            "--theme",
            theme,
        ])
        .timeout(Duration::from_secs(5))
        .write_stdin("{\"message\": \"hello\", \"count\": 42}\n")
        .assert()
        .success()
        .stderr("")
        .get_output()
        .stdout
        .clone()
}

#[test]
fn system_preference_selects_the_configured_dark_or_light_theme() {
    let directory = tempfile::tempdir().unwrap();
    let dark = render(directory.path(), "dark");
    let light = render(directory.path(), "light");
    assert_ne!(dark, light);
    for (setting, expected) in [("prefer-dark", dark), ("prefer-light", light)] {
        install_gsettings(directory.path(), &format!("printf \"'{setting}'\\n\""));
        assert_eq!(render(directory.path(), "auto:system"), expected);
    }
}

#[test]
fn unavailable_or_unspecified_settings_use_the_default_theme() {
    let directory = tempfile::tempdir().unwrap();
    let default = render(directory.path(), "default");
    assert_eq!(render(directory.path(), "auto:system"), default);
    for body in [
        "printf \"'default'\\n\"",
        "printf \"'unknown'\\n\"",
        "printf \"'prefer-light'\\n\"; exit 1",
        "echo 'settings unavailable' >&2; exit 1",
        "printf '\\377'",
        "while :; do :; done",
        "while :; do printf 'too much output'; done",
    ] {
        install_gsettings(directory.path(), body);
        assert_eq!(render(directory.path(), "auto:system"), default, "{body}");
    }
}

#[test]
fn fixed_themes_and_terminal_detection_do_not_query_gsettings() {
    let directory = tempfile::tempdir().unwrap();
    install_gsettings(directory.path(), "echo invoked > \"$SETTINGS_MARKER\"");
    let marker = directory.path().join("called");
    for theme in ["auto", "default", "dark", "light", "GitHub"] {
        bat()
            .env("PATH", directory.path())
            .env("SETTINGS_MARKER", &marker)
            .args(["--style=plain", "--paging=never", "--theme", theme])
            .write_stdin("hello\n")
            .assert()
            .success();
        assert!(!marker.exists(), "gsettings was invoked for {theme}");
    }
}

#![cfg(feature = "build-assets")]

mod utils;

use predicates::prelude::*;
use std::fs;
use utils::command::{bat, bat_with_config};

#[test]
fn vscode_theme_builds_and_highlights_with_inherited_and_overridden_colors() {
    let source = tempfile::tempdir().unwrap();
    let cache = tempfile::tempdir().unwrap();
    let themes = source.path().join("themes");
    fs::create_dir(&themes).unwrap();
    fs::write(
        themes.join("base.json"),
        r##"{
        "colors":{"editor.foreground":"#aaaaaa","editor.background":"#111111"},
        "tokenColors":[{"scope":"string", "settings":{"foreground":"#ff0000"}}]
    }"##,
    )
    .unwrap();
    fs::write(
        themes.join("custom.jsonc"),
        r##"{
        "include":"base.json", // inherit syntax rules
        "colors":{"editor.foreground":"#bbbbbb"},
        "tokenColors":[
            {"scope":"string", "settings":{"foreground":"#00ff00"}},
            {"scope":"constant.numeric", "settings":{"foreground":"#0000ff"}},
        ],
    }"##,
    )
    .unwrap();
    bat_with_config()
        .current_dir(source.path())
        .args(["cache", "--build", "--source"])
        .arg(source.path())
        .arg("--target")
        .arg(cache.path())
        .assert()
        .success();
    bat()
        .env("BAT_CACHE_PATH", cache.path())
        .args(["--list-themes", "--color=never"])
        .assert()
        .success()
        .stdout(predicate::str::contains("custom"));
    bat()
        .env("BAT_CACHE_PATH", cache.path())
        .env("COLORTERM", "truecolor")
        .args([
            "--theme=custom",
            "--language=JSON",
            "--style=plain",
            "--color=always",
        ])
        .write_stdin("[\"value\", 42]\n")
        .assert()
        .success()
        .stdout(
            predicate::str::contains("\x1b[38;2;0;255;0m")
                .and(predicate::str::contains("\x1b[38;2;0;0;255m"))
                .and(predicate::str::contains("\x1b[38;2;255;0;0m").not()),
        );
}

#[test]
fn invalid_json_theme_does_not_overwrite_an_existing_cache() {
    let source = tempfile::tempdir().unwrap();
    let cache = tempfile::tempdir().unwrap();
    fs::create_dir(source.path().join("themes")).unwrap();
    fs::write(source.path().join("themes/broken.json"), "{ /* incomplete").unwrap();
    fs::write(cache.path().join("themes.bin"), b"existing cache").unwrap();
    bat_with_config()
        .current_dir(source.path())
        .args(["cache", "--build", "--source"])
        .arg(source.path())
        .arg("--target")
        .arg(cache.path())
        .assert()
        .failure()
        .stderr(predicate::str::contains("broken.json"));
    assert_eq!(
        fs::read(cache.path().join("themes.bin")).unwrap(),
        b"existing cache"
    );
}

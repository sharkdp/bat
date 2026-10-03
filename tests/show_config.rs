mod utils;
use utils::command::{bat, bat_with_config};

#[test]
fn configuration_query_merges_config_environment_and_cli() {
    let dir = tempfile::tempdir().unwrap();
    let config = dir.path().join("config");
    std::fs::write(
        &config,
        "--theme=TwoDark\n--italic-text=always\n--wrap=never\n",
    )
    .unwrap();
    for (extra, expected) in [(vec![], "ansi\n"), (vec!["--theme=GitHub"], "GitHub\n")] {
        bat_with_config()
            .env("BAT_CONFIG_PATH", &config)
            .env("BAT_THEME", "ansi")
            .args(extra)
            .args(["--show-config", "theme"])
            .assert()
            .success()
            .stdout(expected);
    }
    bat_with_config()
        .env("BAT_CONFIG_PATH", &config)
        .env("BAT_THEME", "ansi")
        .arg("--show-config")
        .assert()
        .success()
        .stdout("italic-text: always\ntheme: ansi\nwrap: never\n");
}

#[test]
fn repeatable_values_keep_order_and_automatic_modes_are_not_resolved() {
    bat()
        .args([
            "--show-config",
            "map-syntax",
            "--map-syntax=*.x:XML",
            "--map-syntax=*.y:JSON",
        ])
        .assert()
        .success()
        .stdout("*.x:XML\n*.y:JSON\n");
    bat()
        .args(["--config", "theme", "--theme=auto:always"])
        .assert()
        .success()
        .stdout("auto:always\n");
    bat()
        .args(["--show-config", "color"])
        .assert()
        .success()
        .stdout("auto\n");
    bat()
        .args(["--show-config", "language"])
        .assert()
        .success()
        .stdout("");
}

#[test]
fn configuration_query_does_not_load_assets_or_start_pager() {
    let dir = tempfile::tempdir().unwrap();
    std::fs::write(
        dir.path().join("metadata.yaml"),
        "this is not cache metadata",
    )
    .unwrap();
    bat()
        .env("BAT_CACHE_PATH", dir.path())
        .env("BAT_PAGER", "nonexistent-pager-for-config-test")
        .args(["--paging=always", "--show-config", "pager"])
        .assert()
        .success()
        .stdout("nonexistent-pager-for-config-test\n");
}

#[test]
fn query_rejects_unknown_fields_and_conflicting_operations() {
    for args in [
        vec!["--show-config=unknown"],
        vec!["--show-config", "--list-languages"],
        vec!["--show-config=theme", "nonexistent-file"],
        vec!["--show-config=theme", "cache", "--build"],
    ] {
        bat().args(args).assert().failure().stdout("");
    }
}

#[test]
fn query_escapes_terminal_controls_in_values() {
    bat()
        .args(["--show-config=pager", "--pager=less\u{1b}[31m\n"])
        .assert()
        .success()
        .stdout("less^[[31m^J\n");
}

mod utils;
use utils::command::{bat, bat_with_config};

#[test]
fn xdg_directories_are_used_for_paths_and_configuration() {
    let dir = tempfile::tempdir().unwrap();
    let config_home = dir.path().join("config home");
    let cache_home = dir.path().join("cache home");
    std::fs::create_dir_all(config_home.join("bat")).unwrap();
    std::fs::write(
        config_home.join("bat/config"),
        "--style=numbers\n--decorations=always\n--color=never\n",
    )
    .unwrap();
    for (flag, expected) in [
        ("--config-dir", config_home.join("bat")),
        ("--cache-dir", cache_home.join("bat")),
    ] {
        bat()
            .env("XDG_CONFIG_HOME", &config_home)
            .env("XDG_CACHE_HOME", &cache_home)
            .arg(flag)
            .assert()
            .success()
            .stdout(format!("{}\n", expected.display()));
    }
    bat_with_config()
        .env("XDG_CONFIG_HOME", &config_home)
        .env("XDG_CACHE_HOME", &cache_home)
        .write_stdin("configured\n")
        .assert()
        .success()
        .stdout("   1 configured\n");
}

#[test]
fn bat_directory_and_file_overrides_take_precedence() {
    let dir = tempfile::tempdir().unwrap();
    let explicit = dir.path().join("explicit");
    for (flag, variable) in [
        ("--config-dir", "BAT_CONFIG_DIR"),
        ("--cache-dir", "BAT_CACHE_PATH"),
    ] {
        bat()
            .env("XDG_CONFIG_HOME", dir.path().join("xdg"))
            .env("XDG_CACHE_HOME", dir.path().join("xdg"))
            .env(variable, &explicit)
            .arg(flag)
            .assert()
            .success()
            .stdout(format!("{}\n", explicit.display()));
    }
    let file = dir.path().join("chosen.conf");
    bat()
        .env("XDG_CONFIG_HOME", dir.path())
        .env("BAT_CONFIG_DIR", &explicit)
        .env("BAT_CONFIG_PATH", &file)
        .arg("--config-file")
        .assert()
        .success()
        .stdout(format!("{}\n", file.display()));
}

#[test]
fn empty_and_relative_xdg_paths_fall_back_to_native_defaults() {
    for (flag, variable) in [
        ("--config-dir", "XDG_CONFIG_HOME"),
        ("--cache-dir", "XDG_CACHE_HOME"),
    ] {
        let expected = bat()
            .env_remove(variable)
            .arg(flag)
            .assert()
            .success()
            .get_output()
            .stdout
            .clone();
        for value in ["", "relative/path"] {
            bat()
                .env(variable, value)
                .arg(flag)
                .assert()
                .success()
                .stdout(expected.clone());
        }
    }
}

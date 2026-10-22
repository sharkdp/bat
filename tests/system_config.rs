mod utils;

use utils::command::bat_with_config;

#[test]
fn no_system_config_keeps_user_config_and_cli_overrides() {
    let dir = tempfile::tempdir().unwrap();
    let config = dir.path().join("config");
    std::fs::write(
        &config,
        "--color=never\n--style=numbers\n--decorations=always",
    )
    .unwrap();
    bat_with_config()
        .env("BAT_CONFIG_PATH", &config)
        .args(["--no-system-config", "test.txt"])
        .assert()
        .success()
        .stdout("   1 hello world\n");
    bat_with_config()
        .env("BAT_CONFIG_PATH", &config)
        .args(["--no-system-config", "--style=plain", "test.txt"])
        .assert()
        .success()
        .stdout("hello world\n");
}

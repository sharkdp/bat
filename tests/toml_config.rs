mod utils;

use predicates::prelude::*;
use std::fs;
use tempfile::TempDir;
use utils::command::bat_with_config as unformatted_bat;

fn bat_with_config() -> assert_cmd::Command {
    let mut command = unformatted_bat();
    command.arg("--decorations=always");
    command
}

fn configuration(content: &str) -> TempDir {
    let directory = tempfile::tempdir().unwrap();
    fs::write(directory.path().join("config.toml"), content).unwrap();
    directory
}

#[test]
fn toml_config_and_environment_and_cli_precedence() {
    let directory = configuration("style = 'plain'\npaging = 'never'\ntabs = 2\n");
    for (env, cli, expected) in [
        (None, None, 2),
        (Some("4"), None, 4),
        (Some("4"), Some("6"), 6),
    ] {
        let mut command = bat_with_config();
        command.env("BAT_CONFIG_DIR", directory.path());
        if let Some(value) = env {
            command.env("BAT_TABS", value);
        }
        if let Some(value) = cli {
            command.args(["--tabs", value]);
        }
        command
            .write_stdin("\ttext\n")
            .assert()
            .success()
            .stdout(format!("{}text\n", " ".repeat(expected)));
    }
}

#[test]
fn legacy_config_takes_precedence_over_toml_fallback() {
    let directory = configuration("style = 'plain'\ntabs = 2\n");
    bat_with_config()
        .env("BAT_CONFIG_DIR", directory.path())
        .arg("--config-file")
        .assert()
        .success()
        .stdout(predicate::str::contains("config.toml"));
    fs::write(directory.path().join("config"), "--style=plain\n--tabs=3\n").unwrap();
    bat_with_config()
        .env("BAT_CONFIG_DIR", directory.path())
        .write_stdin("\tx\n")
        .assert()
        .success()
        .stdout("   x\n");
    bat_with_config()
        .env("BAT_CONFIG_DIR", directory.path())
        .env("BAT_CONFIG_PATH", directory.path().join("config.toml"))
        .write_stdin("\tx\n")
        .assert()
        .success()
        .stdout("  x\n");
}

#[test]
fn toml_values_are_literal_and_arrays_repeat_mappings() {
    let directory = configuration("style = 'plain'\ncolor = 'always'\ntheme = 'ansi'\nmap-syntax = ['*.one:JSON', '*.two:JSON']\nfile-name = 'name with # spaces.one'\n");
    let expected = bat_with_config()
        .args([
            "--no-config",
            "--style=plain",
            "--color=always",
            "--theme=ansi",
            "--language=JSON",
        ])
        .write_stdin("{\"value\": 1}\n")
        .assert()
        .success()
        .get_output()
        .stdout
        .clone();
    for filename in [None, Some("other file.two")] {
        let mut command = bat_with_config();
        command.env("BAT_CONFIG_DIR", directory.path());
        if let Some(filename) = filename {
            command.args(["--file-name", filename]);
        }
        command
            .write_stdin("{\"value\": 1}\n")
            .assert()
            .success()
            .stdout(expected.clone());
    }
}

#[test]
fn flags_and_count_options_work_in_toml() {
    let directory = configuration("number = true\nplain = 2\nshow-all = false\n");
    bat_with_config()
        .env("BAT_CONFIG_DIR", directory.path())
        .write_stdin("one\ntwo\n")
        .assert()
        .success()
        .stdout("one\ntwo\n");
    fs::write(
        directory.path().join("config.toml"),
        "plain = true\nnumber = true\n",
    )
    .unwrap();
    bat_with_config()
        .env("BAT_CONFIG_DIR", directory.path())
        .args(["--decorations=always"])
        .write_stdin("one\ntwo\n")
        .assert()
        .success()
        .stdout(predicate::str::contains("1 one").and(predicate::str::contains("2 two")));
}

#[test]
fn invalid_toml_reports_the_source_and_setting_but_help_still_works() {
    for (content, message) in [
        ("tabs = [", "TOML"),
        ("unknown-setting = 1", "unknown-setting"),
        ("show-all = 'yes'", "requires a boolean"),
        ("plain = -1", "count from 0 to 255"),
        ("tabs = { nested = 2 }", "requires a string"),
    ] {
        let directory = configuration(content);
        bat_with_config()
            .env("BAT_CONFIG_DIR", directory.path())
            .write_stdin("text\n")
            .assert()
            .failure()
            .stderr(predicate::str::contains("config.toml").and(predicate::str::contains(message)));
        bat_with_config()
            .env("BAT_CONFIG_DIR", directory.path())
            .arg("--help")
            .assert()
            .success();
    }
}

#[test]
fn bat_opts_and_no_config_bypass_toml() {
    let directory = configuration("invalid = true");
    bat_with_config()
        .env("BAT_CONFIG_DIR", directory.path())
        .env("BAT_OPTS", "--style=plain --tabs=3")
        .write_stdin("\tx\n")
        .assert()
        .success()
        .stdout("   x\n");
    bat_with_config()
        .env("BAT_CONFIG_DIR", directory.path())
        .args(["--no-config", "--style=plain", "--tabs=4"])
        .write_stdin("\tx\n")
        .assert()
        .success()
        .stdout("    x\n");
}

#[test]
fn generated_toml_is_valid_and_readable() {
    let directory = tempfile::tempdir().unwrap();
    let config = directory.path().join("custom.TOML");
    bat_with_config()
        .env("BAT_CONFIG_PATH", &config)
        .arg("--generate-config-file")
        .assert()
        .success();
    let content = fs::read_to_string(&config).unwrap();
    content.parse::<toml::Table>().unwrap();
    assert!(content.contains("# map-syntax = ["));
    bat_with_config()
        .env("BAT_CONFIG_PATH", &config)
        .args(["--style=plain", "--paging=never"])
        .write_stdin("hello\n")
        .assert()
        .success()
        .stdout("hello\n");
}

#![allow(unused)] // Because indirectly included by e.g.integration_tests.rs, but not everything inside is used

use assert_cmd::cargo::CommandCargoExt;
use std::process::Command;

pub fn bat_raw_command_with_config() -> Command {
    let mut cmd = Command::cargo_bin("bat").unwrap();
    cmd.current_dir("tests/examples");
    cmd.env_remove("BAT_CACHE_PATH");
    cmd.env_remove("BAT_CONFIG_DIR");
    cmd.env_remove("BAT_CONFIG_PATH");
    cmd.env_remove("BAT_OPTS");
    cmd.env_remove("BAT_PAGER");
    cmd.env_remove("BAT_STYLE");
    cmd.env_remove("BAT_TABS");
    cmd.env_remove("BAT_THEME");
    cmd.env_remove("COLORTERM");
    cmd.env_remove("NO_COLOR");
    cmd.env_remove("PAGER");
    cmd.env_remove("LESSOPEN");
    cmd.env_remove("LESSCLOSE");
    cmd.env_remove("SHELL");
    cmd
}

#[cfg(test)]
pub fn bat_raw_command() -> Command {
    let mut cmd = bat_raw_command_with_config();
    cmd.arg("--no-config");
    cmd
}

#[cfg(test)]
pub fn bat_with_config() -> assert_cmd::Command {
    assert_cmd::Command::from_std(bat_raw_command_with_config())
}

#[cfg(test)]
pub fn bat() -> assert_cmd::Command {
    assert_cmd::Command::from_std(bat_raw_command())
}

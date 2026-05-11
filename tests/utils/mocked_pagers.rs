#![allow(unused)] // Because indirectly included by e.g. system_wide_config.rs, but not used

use assert_cmd::Command;
use predicates::prelude::predicate;
use std::env;
use std::path::{Path, PathBuf};

/// For some tests we want mocked versions of some pagers
/// This fn returns the absolute path to the directory with these mocked pagers
fn get_mocked_pagers_dir() -> PathBuf {
    let cargo_manifest_dir = env::var("CARGO_MANIFEST_DIR").expect("Missing CARGO_MANIFEST_DIR");
    Path::new(&cargo_manifest_dir)
        .join("tests")
        .join("mocked-pagers")
}

/// On Unix: 'most' -> 'most'
/// On Windows: 'most' -> 'most.bat'
pub fn from(base: &str) -> String {
    let mut cmd_and_args = shell_words::split(base).unwrap();
    let suffix = if cfg!(windows) { ".bat" } else { "" };
    let mut out_cmd = format!("{}{suffix}", cmd_and_args.first().unwrap());

    if (cmd_and_args.len() > 1) {
        out_cmd.push(' ');
        out_cmd.push_str(cmd_and_args[1..].to_vec().join(" ").as_str());
    }

    out_cmd
}

/// Prepends a directory to the PATH environment variable
/// Returns the original value for later restoration
fn prepend_dir_to_path_env_var(dir: PathBuf) -> String {
    // Get current PATH
    let original_path = env::var("PATH").expect("No PATH?!");

    // Add the new dir first
    let mut split_paths = env::split_paths(&original_path).collect::<Vec<_>>();
    split_paths.insert(0, dir);

    // Set PATH with the new dir
    let new_path = env::join_paths(split_paths).expect("Failed to join paths");
    env::set_var("PATH", new_path);

    // Return the original value for later restoration of it
    original_path
}

/// Helper to restore the value of PATH
fn restore_path(original_path: String) {
    env::set_var("PATH", original_path);
}

/// Allows test to run that require our mocked versions of 'more' and 'most'
/// in PATH. Temporarily changes PATH while the test code runs, and then restore it
/// to avoid pollution of global state
pub fn with_mocked_versions_of_more_and_most_in_path(actual_test: fn()) {
    let original_path = prepend_dir_to_path_env_var(get_mocked_pagers_dir());

    // Make sure our own variants of 'more' and 'most' are used
    Command::new(from("more"))
        .assert()
        .success()
        .stdout(predicate::str::contains("I am more"));
    Command::new(from("most"))
        .assert()
        .success()
        .stdout(predicate::str::contains("I am most"));
    Command::new(from("echo"))
        .arg("foobar")
        .assert()
        .success()
        .stdout(predicate::str::contains("foobar"));

    // Now run the actual test
    actual_test();

    // Make sure to restore PATH since it is global state
    restore_path(original_path);
}

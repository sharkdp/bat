#![cfg(feature = "build-assets")]

use std::fs;
use std::process::Command;

use assert_cmd::assert::OutputAssertExt;
use predicates::prelude::*;
use tempfile::TempDir;

mod utils;
use utils::command::{bat, bat_raw_command, bat_with_config};

struct Assets {
    source: TempDir,
    cache: TempDir,
}

impl Assets {
    fn new() -> Self {
        let assets = Self {
            source: tempfile::tempdir().unwrap(),
            cache: tempfile::tempdir().unwrap(),
        };
        fs::create_dir(assets.source.path().join("syntaxes")).unwrap();
        assets.syntax("One");
        assets
    }

    fn syntax(&self, name: &str) {
        fs::write(
            self.source.path().join("syntaxes/custom.sublime-syntax"),
            format!(
                "name: Automatic {name}\nfile_extensions: [autocustom]\nscope: source.autocustom\ncontexts:\n  main:\n    - match: custom\n      scope: keyword.control\n"
            ),
        )
        .unwrap();
    }

    fn build(&self, automatic: bool) {
        let mut command = bat_with_config();
        command
            .current_dir(self.source.path())
            .args(["cache", "--build"])
            .arg("--source")
            .arg(self.source.path())
            .arg("--target")
            .arg(self.cache.path());
        if automatic {
            command.arg("--automatic");
        }
        command.assert().success();
    }

    fn command(&self) -> Command {
        let mut command = bat_raw_command();
        command
            .env("BAT_CACHE_PATH", self.cache.path())
            .args(["--list-languages", "--color=never"]);
        command
    }

    fn names(&self, expected: &str, absent: &str) {
        self.command()
            .assert()
            .success()
            .stdout(predicate::str::contains(expected).and(predicate::str::contains(absent).not()))
            .stderr("");
    }

    fn generations(&self) -> Vec<std::path::PathBuf> {
        let root = self.cache.path().join("automatic");
        if !root.exists() {
            return vec![];
        }
        fs::read_dir(root)
            .unwrap()
            .map(|entry| entry.unwrap().path())
            .collect()
    }
}

#[test]
fn source_content_changes_rebuild_without_touching_the_explicit_cache() {
    let assets = Assets::new();
    assets.build(true);
    assets.names("Automatic One", "Automatic Two");
    assert!(assets.generations().is_empty());
    let explicit = fs::read(assets.cache.path().join("syntaxes.bin")).unwrap();
    let source = assets.source.path().join("syntaxes/custom.sublime-syntax");
    let modified = source.metadata().unwrap().modified().unwrap();
    assets.syntax("Two");
    fs::File::options()
        .write(true)
        .open(&source)
        .unwrap()
        .set_times(fs::FileTimes::new().set_modified(modified))
        .unwrap();
    assets.names("Automatic Two", "Automatic One");
    let generations = assets.generations();
    assert_eq!(generations.len(), 1);
    let generated = generations[0].join("syntaxes.bin");
    let generated_time = generated.metadata().unwrap().modified().unwrap();
    assets.names("Automatic Two", "Automatic One");
    assert_eq!(
        generated.metadata().unwrap().modified().unwrap(),
        generated_time
    );
    assert_eq!(
        fs::read(assets.cache.path().join("syntaxes.bin")).unwrap(),
        explicit
    );
}

#[test]
fn incompatible_metadata_rebuilds_in_a_separate_generation() {
    let assets = Assets::new();
    assets.build(true);
    fs::write(
        assets.cache.path().join("metadata.yaml"),
        "bat_version: 0.1.0\n",
    )
    .unwrap();
    assets.names("Automatic One", "Writing syntax set");
    assert_eq!(assets.generations().len(), 1);
    assert_eq!(
        fs::read_to_string(assets.cache.path().join("metadata.yaml")).unwrap(),
        "bat_version: 0.1.0\n"
    );
}

#[test]
fn malformed_sources_fail_without_replacing_good_caches() {
    let assets = Assets::new();
    assets.build(true);
    let original = fs::read(assets.cache.path().join("syntaxes.bin")).unwrap();
    fs::write(
        assets.source.path().join("syntaxes/custom.sublime-syntax"),
        "contexts: [",
    )
    .unwrap();
    assets
        .command()
        .assert()
        .failure()
        .stdout("")
        .stderr(predicate::str::contains("Could not automatically rebuild"));
    assert_eq!(
        fs::read(assets.cache.path().join("syntaxes.bin")).unwrap(),
        original
    );
    assert!(assets.generations().is_empty());
    assets.syntax("Two");
    assets.names("Automatic Two", "Automatic One");
}

#[test]
fn additions_and_removals_invalidate_the_cache() {
    let assets = Assets::new();
    assets.build(true);
    fs::rename(
        assets.source.path().join("syntaxes/custom.sublime-syntax"),
        assets.source.path().join("syntaxes/renamed.sublime-syntax"),
    )
    .unwrap();
    assets.names("Automatic One", "Automatic Two");
    assert_eq!(assets.generations().len(), 1);
    fs::remove_file(assets.source.path().join("syntaxes/renamed.sublime-syntax")).unwrap();
    assets.names("Rust", "Automatic One");
    assert_eq!(assets.generations().len(), 2);
}

#[test]
fn concurrent_builds_publish_one_complete_generation() {
    let assets = Assets::new();
    assets.build(true);
    assets.syntax("Two");
    let children: Vec<_> = (0..3)
        .map(|_| {
            assets
                .command()
                .stdout(std::process::Stdio::piped())
                .stderr(std::process::Stdio::piped())
                .spawn()
                .unwrap()
        })
        .collect();
    for child in children {
        let output = child.wait_with_output().unwrap();
        output
            .assert()
            .success()
            .stdout(predicate::str::contains("Automatic Two"))
            .stderr("");
    }
    assert_eq!(assets.generations().len(), 1);
}

#[test]
fn corrupt_automatic_generation_is_repaired() {
    let assets = Assets::new();
    assets.build(true);
    assets.syntax("Two");
    assets.names("Automatic Two", "Automatic One");
    let generation = assets.generations().pop().unwrap();
    fs::write(generation.join("syntaxes.bin"), "broken").unwrap();
    assets.names("Automatic Two", "Automatic One");
    assert_eq!(assets.generations(), vec![generation]);
}

#[test]
fn manual_build_disables_automatic_mode_and_clear_removes_generations() {
    let assets = Assets::new();
    assets.build(true);
    assets.syntax("Two");
    assets.names("Automatic Two", "Automatic One");
    assets.build(false);
    assert!(!assets.cache.path().join("automatic.yaml").exists());
    assets.syntax("Three");
    assets.names("Automatic Two", "Automatic Three");
    bat_with_config()
        .current_dir(assets.source.path())
        .args(["cache", "--clear"])
        .env("BAT_CACHE_PATH", assets.cache.path())
        .assert()
        .success();
    assert!(assets.generations().is_empty());
    assert!(!assets.cache.path().join("syntaxes.bin").exists());
}

#[test]
fn source_and_target_may_match_and_cache_filename_does_not_hide_rebuild() {
    let assets = Assets::new();
    bat_with_config()
        .current_dir(assets.cache.path())
        .args(["cache", "--build", "--automatic", "--blank"])
        .arg("--source")
        .arg(assets.source.path())
        .arg("--target")
        .arg(assets.source.path())
        .assert()
        .success();
    fs::write(assets.source.path().join("cache"), "a file named cache").unwrap();
    assets.syntax("Two");
    bat()
        .current_dir(assets.source.path())
        .env("BAT_CACHE_PATH", assets.source.path())
        .args(["--list-languages", "--color=never"])
        .assert()
        .success()
        .stdout(
            predicate::str::contains("Automatic Two").and(predicate::str::contains("Rust").not()),
        )
        .stderr("");
}

#[test]
fn unavailable_sources_can_be_bypassed_with_no_custom_assets() {
    let assets = Assets::new();
    assets.build(true);
    fs::remove_dir_all(assets.source.path()).unwrap();
    assets.command().assert().failure();
    assets
        .command()
        .arg("--no-custom-assets")
        .assert()
        .success()
        .stdout(predicate::str::contains("Automatic One").not());
}

#[cfg(unix)]
#[test]
fn linked_source_content_is_tracked() {
    let assets = Assets::new();
    let linked = tempfile::tempdir().unwrap();
    let source = assets.source.path().join("syntaxes/custom.sublime-syntax");
    let destination = linked.path().join("linked.sublime-syntax");
    fs::rename(&source, &destination).unwrap();
    std::os::unix::fs::symlink(&destination, &source).unwrap();
    assets.build(true);
    assets.syntax("Two");
    assets.names("Automatic Two", "Automatic One");
}

#![cfg(feature = "build-assets")]

use std::fs;
use std::path::{Path, PathBuf};

use bat::assets::HighlightingAssets;
use predicates::prelude::*;
use tempfile::{tempdir, TempDir};

mod utils;
use utils::command::{bat, bat_with_config};

struct Sources {
    system: TempDir,
    user: TempDir,
    cache: TempDir,
}

fn syntax(root: &Path, filename: &str, name: &str, scope: &str, rules: &str) -> PathBuf {
    fs::create_dir_all(root.join("syntaxes")).unwrap();
    let path = root.join("syntaxes").join(filename);
    fs::write(
        &path,
        format!("name: {name}\nfile_extensions: [layered]\nscope: {scope}\ncontexts:\n  main:\n{rules}\n"),
    )
    .unwrap();
    path
}

fn theme(root: &Path, color: &str) {
    fs::create_dir_all(root.join("themes")).unwrap();
    fs::write(root.join("themes/Layered.tmTheme"), format!(
        "<?xml version=\"1.0\"?><plist version=\"1.0\"><dict><key>settings</key><array><dict><key>settings</key><dict><key>foreground</key><string>{color}</string></dict></dict><dict><key>scope</key><string>keyword.control, constant.numeric</string><key>settings</key><dict><key>foreground</key><string>#00ff00</string></dict></dict></array></dict></plist>"
    )).unwrap();
}

impl Sources {
    fn new() -> Self {
        Self {
            system: tempdir().unwrap(),
            user: tempdir().unwrap(),
            cache: tempdir().unwrap(),
        }
    }

    fn build(&self, automatic: bool, reversed: bool) {
        let sources = if reversed {
            [self.user.path(), self.system.path()]
        } else {
            [self.system.path(), self.user.path()]
        };
        let mut command = bat_with_config();
        command
            .current_dir(self.cache.path())
            .args(["cache", "--build", "--blank", "--target"])
            .arg(self.cache.path());
        for source in sources {
            command.arg("--source").arg(source);
        }
        if automatic {
            command.arg("--automatic");
        }
        command.assert().success();
    }

    fn names(&self) -> assert_cmd::Command {
        let mut command = bat();
        command.env("BAT_CACHE_PATH", self.cache.path()).args([
            "--list-languages",
            "--color=never",
            "--paging=never",
        ]);
        command
    }
}

#[test]
fn later_sources_override_themes_and_syntax_names_extensions_and_scopes() {
    let sources = Sources::new();
    syntax(
        sources.system.path(),
        "shared.sublime-syntax",
        "Shared",
        "source.shared",
        "    - match: system\n      scope: keyword.control",
    );
    syntax(
        sources.user.path(),
        "shared.sublime-syntax",
        "Shared",
        "source.shared",
        "    - match: user\n      scope: keyword.control",
    );
    theme(sources.system.path(), "#112233");
    theme(sources.user.path(), "#abcdef");
    for (reversed, expected, keyword) in [
        (false, (0xab, 0xcd, 0xef), "user"),
        (true, (0x11, 0x22, 0x33), "system"),
    ] {
        sources.build(false, reversed);
        let assets = HighlightingAssets::from_cache(sources.cache.path()).unwrap();
        let theme = assets.get_theme("Layered");
        let color = theme.settings.foreground.unwrap();
        assert_eq!((color.r, color.g, color.b), expected);
        let syntaxes = assets.get_syntax_set().unwrap();
        for syntax in [
            syntaxes.find_syntax_by_name("Shared").unwrap(),
            syntaxes.find_syntax_by_extension("layered").unwrap(),
            syntaxes
                .find_syntax_by_scope("source.shared".parse().unwrap())
                .unwrap(),
        ] {
            let mut highlighter = syntect::easy::HighlightLines::new(syntax, theme);
            let highlighted = highlighter.highlight_line(keyword, syntaxes).unwrap();
            let color = highlighted[0].0.foreground;
            assert_eq!((color.r, color.g, color.b), (0, 255, 0));
        }
    }
}

#[test]
fn user_syntaxes_can_include_system_syntaxes_in_a_single_linked_set() {
    let sources = Sources::new();
    syntax(
        sources.system.path(),
        "base.sublime-syntax",
        "System Base",
        "source.system-base",
        "    - match: base\n      scope: constant.numeric",
    );
    syntax(
        sources.user.path(),
        "wrapper.sublime-syntax",
        "User Wrapper",
        "source.user-wrapper",
        "    - include: scope:source.system-base",
    );
    theme(sources.user.path(), "#abcdef");
    sources.build(false, false);
    let assets = HighlightingAssets::from_cache(sources.cache.path()).unwrap();
    let syntaxes = assets.get_syntax_set().unwrap();
    assert!(syntaxes.find_unlinked_contexts().is_empty());
    let mut highlighter = syntect::easy::HighlightLines::new(
        syntaxes.find_syntax_by_name("User Wrapper").unwrap(),
        assets.get_theme("Layered"),
    );
    let highlighted = highlighter.highlight_line("base", syntaxes).unwrap();
    let color = highlighted[0].0.foreground;
    assert_eq!((color.r, color.g, color.b), (0, 255, 0));
}

#[test]
fn package_updates_and_removals_rebuild_only_the_user_cache() {
    let sources = Sources::new();
    let system_file = syntax(
        sources.system.path(),
        "package.sublime-syntax",
        "Package One",
        "source.package",
        "    - match: package\n      scope: keyword.control",
    );
    syntax(
        sources.user.path(),
        "custom.sublime-syntax",
        "User Custom",
        "source.user-custom",
        "    - match: user\n      scope: keyword.control",
    );
    sources.build(true, false);
    sources.names().assert().success().stdout(
        predicate::str::contains("Package One").and(predicate::str::contains("User Custom")),
    );
    syntax(
        sources.system.path(),
        "package.sublime-syntax",
        "Package Two",
        "source.package",
        "    - match: package\n      scope: keyword.control",
    );
    sources.names().assert().success().stdout(
        predicate::str::contains("Package Two").and(predicate::str::contains("Package One").not()),
    );
    fs::remove_file(system_file).unwrap();
    sources.names().assert().success().stdout(
        predicate::str::contains("User Custom").and(predicate::str::contains("Package Two").not()),
    );
    for source in [sources.system.path(), sources.user.path()] {
        assert!(!source.join("syntaxes.bin").exists());
        assert!(!source.join("themes.bin").exists());
        assert!(!source.join("automatic.yaml").exists());
        assert!(!source.join("automatic").exists());
    }
}

#[test]
fn missing_explicit_source_fails_before_replacing_the_cache() {
    let sources = Sources::new();
    syntax(
        sources.user.path(),
        "custom.sublime-syntax",
        "User Custom",
        "source.user-custom",
        "    - match: user\n      scope: keyword.control",
    );
    sources.build(false, false);
    let old = fs::read(sources.cache.path().join("syntaxes.bin")).unwrap();
    bat_with_config()
        .current_dir(sources.cache.path())
        .args(["cache", "--build", "--source"])
        .arg(sources.system.path())
        .arg("--source")
        .arg(sources.user.path().join("missing"))
        .arg("--target")
        .arg(sources.cache.path())
        .assert()
        .failure()
        .stderr(predicate::str::contains("is not a directory"));
    assert_eq!(
        fs::read(sources.cache.path().join("syntaxes.bin")).unwrap(),
        old
    );
}

#[test]
fn acknowledgements_collect_notices_from_all_sources_once() {
    let sources = Sources::new();
    fs::write(
        sources.system.path().join("NOTICE"),
        "System package notice\n",
    )
    .unwrap();
    fs::write(sources.user.path().join("NOTICE"), "User package notice\n").unwrap();
    bat::assets::build_from_dirs(
        &[sources.system.path(), sources.user.path()],
        false,
        true,
        sources.cache.path(),
        env!("CARGO_PKG_VERSION"),
    )
    .unwrap();
    let data = fs::read(sources.cache.path().join("acknowledgements.bin")).unwrap();
    let notices: String = syntect::dumps::from_binary(&data);
    assert_eq!(notices.matches("System package notice").count(), 1);
    assert_eq!(notices.matches("User package notice").count(), 1);
}

#[test]
fn single_source_recipes_without_the_new_list_remain_readable() {
    let sources = Sources::new();
    syntax(
        sources.user.path(),
        "custom.sublime-syntax",
        "Original",
        "source.user-custom",
        "    - match: user\n      scope: keyword.control",
    );
    bat_with_config()
        .current_dir(sources.cache.path())
        .args(["cache", "--build", "--blank", "--automatic", "--source"])
        .arg(sources.user.path())
        .arg("--target")
        .arg(sources.cache.path())
        .assert()
        .success();
    let path = sources.cache.path().join("automatic.yaml");
    let mut recipe: serde_yaml::Value = serde_yaml::from_slice(&fs::read(&path).unwrap()).unwrap();
    recipe
        .as_mapping_mut()
        .unwrap()
        .remove(serde_yaml::Value::String("preceding_source_dirs".into()));
    fs::write(path, serde_yaml::to_string(&recipe).unwrap()).unwrap();
    syntax(
        sources.user.path(),
        "custom.sublime-syntax",
        "Updated",
        "source.user-custom",
        "    - match: user\n      scope: keyword.control",
    );
    sources.names().assert().success().stdout(
        predicate::str::contains("Updated").and(predicate::str::contains("Original").not()),
    );
}

#[test]
fn shared_sources_can_be_combined_when_user_source_is_also_the_cache() {
    let sources = Sources::new();
    syntax(
        sources.system.path(),
        "package.sublime-syntax",
        "Original Package",
        "source.package",
        "    - match: package\n      scope: keyword.control",
    );
    bat_with_config()
        .current_dir(sources.cache.path())
        .args(["cache", "--build", "--blank", "--automatic", "--source"])
        .arg(sources.system.path())
        .arg("--source")
        .arg(sources.user.path())
        .arg("--target")
        .arg(sources.user.path())
        .assert()
        .success();
    syntax(
        sources.system.path(),
        "package.sublime-syntax",
        "Updated Package",
        "source.package",
        "    - match: package\n      scope: keyword.control",
    );
    for _ in 0..2 {
        bat()
            .env("BAT_CACHE_PATH", sources.user.path())
            .args(["--list-languages", "--color=never", "--paging=never"])
            .assert()
            .success()
            .stdout(predicate::str::contains("Updated Package"));
        assert_eq!(
            fs::read_dir(sources.user.path().join("automatic"))
                .unwrap()
                .count(),
            1
        );
    }
}

mod utils;
use utils::command::bat;

#[test]
fn foreground_and_gutter_colors_are_independent() {
    let output = bat()
        .env("COLORTERM", "truecolor")
        .args([
            "--theme=Monokai Extended",
            "--style=numbers",
            "--decorations=always",
            "--color=always",
            "--language=txt",
            "--set-theme-color",
            "foreground",
            "#123456",
            "--set-theme-color",
            "gutterForeground",
            "abcdef",
        ])
        .write_stdin("example\n")
        .assert()
        .success()
        .get_output()
        .stdout
        .clone();
    let output = String::from_utf8(output).unwrap();
    assert!(output.contains("38;2;18;52;86mexample"), "{output:?}");
    assert!(output.contains("38;2;171;205;239m"), "{output:?}");
}

#[test]
fn line_highlight_override_respects_color_mode() {
    for (color, expected) in [("always", true), ("never", false)] {
        let output = bat()
            .env("COLORTERM", "truecolor")
            .args([
                "--theme=Monokai Extended",
                "--style=plain",
                "--language=txt",
                "--terminal-width=20",
                "--highlight-line=1",
                "--color",
                color,
                "--set-theme-color",
                "lineHighlight",
                "102030",
            ])
            .write_stdin("selected\nother\n")
            .assert()
            .success()
            .get_output()
            .stdout
            .clone();
        let output = String::from_utf8(output).unwrap();
        assert_eq!(output.contains("48;2;16;32;48m"), expected, "{output:?}");
        if !expected {
            assert_eq!(output, "selected\nother\n");
        }
    }
}

#[test]
fn cli_overrides_config_colors_and_last_assignment_wins() {
    let dir = tempfile::tempdir().unwrap();
    let config = dir.path().join("config");
    std::fs::write(
        &config,
        "--set-theme-color foreground 010203\n--set-theme-color gutterForeground 040506\n",
    )
    .unwrap();
    let output = utils::command::bat_with_config()
        .env("BAT_CONFIG_PATH", &config)
        .env("COLORTERM", "truecolor")
        .args([
            "--style=numbers",
            "--decorations=always",
            "--theme=Monokai Extended",
            "--color=always",
            "--language=txt",
            "--set-theme-color",
            "foreground",
            "070809",
            "--set-theme-color",
            "foreground",
            "0a0b0c",
        ])
        .write_stdin("example\n")
        .assert()
        .success()
        .get_output()
        .stdout
        .clone();
    let output = String::from_utf8(output).unwrap();
    assert!(output.contains("38;2;10;11;12mexample"), "{output:?}");
    assert!(output.contains("38;2;4;5;6m"), "{output:?}");
    assert!(!output.contains("38;2;7;8;9m"));
}

#[test]
fn invalid_colors_and_missing_values_fail_before_content() {
    for values in [
        vec!["unknown", "123456"],
        vec!["foreground", "abcd"],
        vec!["lineHighlight", "xx1122"],
        vec!["foreground", "ééé"],
        vec!["foreground"],
    ] {
        bat()
            .arg("--set-theme-color")
            .args(values)
            .write_stdin("private content\n")
            .assert()
            .failure()
            .stdout("");
    }
}

#[test]
fn library_overrides_do_not_mutate_shared_assets() {
    use bat::{
        assets::HighlightingAssets, config::Config, controller::Controller, input::Input,
        output::OutputHandle,
    };
    let assets = HighlightingAssets::from_binary();
    let name = "Monokai Extended";
    let before = assets.get_theme(name).settings.foreground;
    let mut config = Config {
        theme: name.into(),
        colored_output: true,
        true_color: true,
        ..Default::default()
    };
    config.theme_colors.set("foreground", "123456").unwrap();
    let controller = Controller::new(&config, &assets);
    let mut output = String::new();
    assert!(controller
        .run(
            vec![Input::from_reader(Box::new(&b"example\n"[..]))],
            Some(&mut OutputHandle::FmtWrite(&mut output))
        )
        .unwrap());
    assert!(output.contains("38;2;18;52;86mexample"), "{output:?}");
    assert_eq!(assets.get_theme(name).settings.foreground, before);
    assert!(bat::PrettyPrinter::new()
        .set_theme_color("lineHighlight", "ffffff")
        .is_ok());
}

#[test]
fn help_output_uses_overridden_foreground() {
    let output = bat()
        .args([
            "--color=always",
            "--theme=Monokai Extended",
            "--set-theme-color",
            "foreground",
            "123456",
            "--help",
        ])
        .assert()
        .success()
        .get_output()
        .stdout
        .clone();
    let output = String::from_utf8(output).unwrap();
    assert!(output.contains("38;2;18;52;86m"), "{output:?}");
}

#[test]
fn terminal_default_foreground_keeps_plain_text_uncolored_and_syntax_colored() {
    for name in ["foreground", "gutterForeground"] {
        let mut overrides = bat::theme::ThemeColorOverrides::default();
        overrides.set(name, "default").unwrap();
    }
    for earlier in ["123456", "default"] {
        bat()
            .args([
                "--theme=Monokai Extended",
                "--style=plain",
                "--color=always",
                "--language=txt",
                "--set-theme-color",
                "foreground",
                earlier,
                "--set-theme-color",
                "foreground",
                "default",
            ])
            .write_stdin("ordinary text\n")
            .assert()
            .success()
            .stdout("ordinary text\n");
    }
    let output = bat()
        .env("COLORTERM", "truecolor")
        .args([
            "--theme=Monokai Extended",
            "--style=plain",
            "--color=always",
            "--language=Rust",
            "--set-theme-color",
            "foreground",
            "default",
        ])
        .write_stdin("fn main() {}\n")
        .assert()
        .success()
        .get_output()
        .stdout
        .clone();
    let output = String::from_utf8(output).unwrap();
    assert!(output.contains("\x1b[38;2;"), "{output:?}");
    assert!(!output.contains("38;2;248;248;242m"), "{output:?}");
    assert!(bat::theme::ThemeColorOverrides::default()
        .set("lineHighlight", "default")
        .is_err());
}

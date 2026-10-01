use std::collections::HashSet;

use regex::Regex;

mod utils;
use utils::command::bat;

fn assert_neutral(output: &[u8], true_color: bool) {
    let text = std::str::from_utf8(output).unwrap();
    let colors = Regex::new(r"(?:38|48);(?:2;(\d+);(\d+);(\d+)|5;(\d+))").unwrap();
    let mut shades = HashSet::new();
    for capture in colors.captures_iter(text) {
        let (r, g, b) = if let Some(index) = capture.get(4) {
            assert!(!true_color || index.as_str() == "238"); // neutral fallback gutter
            ansi_colours::rgb_from_ansi256(index.as_str().parse().unwrap())
        } else {
            assert!(true_color);
            (
                capture[1].parse().unwrap(),
                capture[2].parse().unwrap(),
                capture[3].parse().unwrap(),
            )
        };
        assert_eq!(r, g, "{capture:?}");
        assert_eq!(g, b, "{capture:?}");
        shades.insert(r);
    }
    assert!(shades.len() >= 2, "expected distinct gray shades: {text:?}");
}

#[test]
fn syntax_decorations_and_highlights_are_gray_in_both_color_modes() {
    for true_color in [true, false] {
        let mut command = bat();
        command.args([
            "--grayscale",
            "--color=always",
            "--decorations=always",
            "--style=full",
            "--theme=gruvbox-light",
            "--highlight-line=1",
            "--language=rust",
        ]);
        if true_color {
            command.env("COLORTERM", "truecolor");
        }
        let output = command
            .write_stdin("fn main() { let value = 42; println!(\"hello\"); }\n")
            .assert()
            .success()
            .get_output()
            .stdout
            .clone();
        assert_neutral(&output, true_color);
    }
}

#[test]
fn help_and_theme_previews_honor_grayscale() {
    for args in [vec!["--help"], vec!["--list-themes"]] {
        let output = bat()
            .args(["--grayscale", "--color=always", "--paging=never"])
            .args(args)
            .env("COLORTERM", "truecolor")
            .assert()
            .success()
            .get_output()
            .stdout
            .clone();
        assert_neutral(&output, true);
    }
}

#[test]
fn no_color_and_plain_passthrough_stay_unchanged() {
    for args in [vec!["--color=never", "--style=plain"], vec!["-pp"]] {
        bat()
            .arg("--grayscale")
            .args(args)
            .write_stdin("hello\tworld\n")
            .assert()
            .success()
            .stdout("hello\tworld\n");
    }
}

#[test]
fn library_pretty_printer_supports_grayscale() {
    let mut output = String::new();
    bat::PrettyPrinter::new()
        .input_from_bytes(b"let value = 42;\n")
        .language("Rust")
        .theme("gruvbox-light")
        .colored_output(true)
        .true_color(true)
        .grayscale(true)
        .print_with_writer(Some(&mut output))
        .unwrap();
    assert_neutral(output.as_bytes(), true);
}

#[test]
fn input_colors_can_be_preserved_or_explicitly_stripped() {
    let input = "let value = \"\x1b[31mred\x1b[0m\";\n";
    for strip in [false, true] {
        let mut command = bat();
        command
            .args([
                "--grayscale",
                "--color=always",
                "--style=plain",
                "--language=rust",
            ])
            .env("COLORTERM", "truecolor");
        if strip {
            command.arg("--strip-ansi=always");
        }
        let output = command
            .write_stdin(input)
            .assert()
            .success()
            .get_output()
            .stdout
            .clone();
        assert_eq!(
            String::from_utf8_lossy(&output).contains("\x1b[31m"),
            !strip
        );
        if strip {
            assert_neutral(&output, true);
        }
    }
}

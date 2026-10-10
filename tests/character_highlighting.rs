mod utils;

use utils::command::bat;

fn output(input: &str, args: &[&str]) -> String {
    String::from_utf8(
        bat()
            .env("COLORTERM", "truecolor")
            .args([
                "--color=always",
                "--style=plain",
                "--paging=never",
                "--theme=Monokai Extended",
                "--wrap=never",
                "--tabs=4",
            ])
            .args((!args.contains(&"-A")).then_some("--language=txt"))
            .args(args)
            .write_stdin(input)
            .assert()
            .success()
            .get_output()
            .stdout
            .clone(),
    )
    .unwrap()
}

/// Extract text carrying a background or underline, accounting for SGR resets.
fn highlighted(output: &str) -> String {
    let sgr = regex::Regex::new(r"\x1b\[([0-9;]*)m").unwrap();
    let mut result = String::new();
    let mut background = false;
    let mut underline = false;
    let mut position = 0;
    for captures in sgr.captures_iter(output) {
        let escape = captures.get(0).unwrap();
        if background || underline {
            result.push_str(&output[position..escape.start()]);
        }
        let values: Vec<u16> = captures[1]
            .split(';')
            .map(|s| s.parse().unwrap_or(0))
            .collect();
        let mut i = 0;
        while i < values.len() {
            match values[i] {
                0 => {
                    background = false;
                    underline = false;
                }
                4 => underline = true,
                24 => underline = false,
                49 => background = false,
                40..=47 | 100..=107 => background = true,
                code @ (38 | 48) => {
                    if code == 48 {
                        background = true;
                    }
                    i += match values.get(i + 1) {
                        Some(2) => 4,
                        Some(5) => 2,
                        _ => 0,
                    };
                }
                _ => {}
            }
            i += 1;
        }
        position = escape.end();
    }
    if background || underline {
        result.push_str(&output[position..]);
    }
    result
}

#[test]
fn selects_exact_characters_across_lines_and_overlapping_ranges() {
    assert_eq!(highlighted(&output("abcdef\n", &["-H", "1.2:.4"])), "bcd");
    assert_eq!(highlighted(&output("abcdef\n", &["-H", "1.2"])), "b");
    assert_eq!(
        highlighted(&output("abcdef\n", &["-H", "1.2:.4", "-H", "1.3:.5"])),
        "bcde"
    );
    assert_eq!(
        highlighted(&output("abcd\nEFGH\nijkl\n", &["-H", "1.2:3.2"])),
        "bcdEFGHij"
    );
    assert_eq!(
        highlighted(&output("abcd\nEFGH\n", &["-H", ":2.2"])),
        "abcdEF"
    );
    assert_eq!(
        highlighted(&output("abcd\nEFGH\n", &["-H", "1.3:"])),
        "cdEFGH"
    );
    assert!(highlighted(&output("abc\n", &["-H", "1.20:.30"])).is_empty());
}

#[test]
fn unicode_graphemes_and_ansi_escapes_keep_their_positions() {
    let text = "ae\u{301}中👩‍💻z\r\n";
    assert_eq!(
        highlighted(&output(text, &["-H", "1.2:.4"])),
        "e\u{301}中👩‍💻"
    );
    assert_eq!(
        highlighted(&output("a\x1b[31mbc\x1b[0md\n", &["-H", "1.2:.3"])),
        "bc"
    );
    assert_eq!(highlighted(&output("ab\tcd\n", &["-H", "1.3"])), "  ");
}

#[test]
fn character_highlights_follow_wrapping_without_extending_into_padding() {
    for wrap in ["character", "word", "never"] {
        let text = output(
            "abcdefghij\n",
            &["-H", "1.3:.8", "--terminal-width=4", "--wrap", wrap],
        );
        assert_eq!(
            highlighted(&text).replace('\n', ""),
            "cdefgh",
            "{wrap}: {text:?}"
        );
    }
    assert_eq!(
        highlighted(&output("abc\n", &["-H", "1.2:.3", "--terminal-width=80"])),
        "bc"
    );
}

#[test]
fn ansi_theme_and_disabled_colors_have_defined_behavior() {
    assert_eq!(
        highlighted(&output("abc\n", &["-H", "1.2", "--theme=ansi"])),
        "b"
    );
    assert_eq!(output("abc\n", &["-H", "1.2", "--color=never"]), "abc\n");
    assert_eq!(
        highlighted(&output("a\tb\n", &["-A", "--tabs=1", "-H", "1.2"])),
        "↹"
    );
}

#[test]
fn legacy_line_highlighting_and_character_ranges_can_be_mixed() {
    let text = output(
        "abc\ndef\n",
        &["-H", "1.2", "-H", "2", "--terminal-width=3"],
    );
    assert_eq!(highlighted(&text).trim_end(), "bdef");
    for value in ["1.0", "0.1", "1.4:.2", "1.x", ".", "1.2:3.4:5"] {
        bat()
            .args(["-H", value])
            .write_stdin("abc\n")
            .assert()
            .failure()
            .stdout("");
    }
}

#[test]
fn library_callers_can_highlight_character_regions() {
    let mut text = String::new();
    bat::PrettyPrinter::new()
        .input_from_bytes(b"abcdef\n")
        .language("txt")
        .theme("Monokai Extended")
        .colored_output(true)
        .true_color(true)
        .highlight_region("1.2:.4".parse().unwrap())
        .print_with_writer(Some(&mut text))
        .unwrap();
    assert_eq!(highlighted(&text), "bcd");
}

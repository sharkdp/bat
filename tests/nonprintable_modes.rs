mod utils;

use utils::command::bat;

fn output(mode: &str, input: &[u8], extra: &[&str]) -> String {
    String::from_utf8(
        bat()
            .args([
                "-A",
                "-c",
                mode,
                "--color=never",
                "--style=plain",
                "--tabs=4",
            ])
            .args(extra)
            .write_stdin(input)
            .assert()
            .success()
            .get_output()
            .stdout
            .clone(),
    )
    .unwrap()
}

#[test]
fn new_notations_display_common_controls_and_invalid_bytes() {
    let input = b"\x00\x08\t\n\x0b\x0c\r\x1b\x7f\xff";
    assert_eq!(output("symbols", input, &[]), "␀⌫⇥ ⏎\n⤓↡←⎋⌦\\xFF");
    assert_eq!(output("period", input, &[]), ".....\n......");
    assert_eq!(output("binary", input, &[]), "..⇥ ⏎\n..←⎋..");
}

#[test]
fn notation_does_not_expose_control_bytes_or_modify_printable_punctuation() {
    let controls: Vec<u8> = (0..=31).chain([127, 128, 255]).collect();
    for mode in ["symbols", "period", "binary"] {
        let text = output(mode, &controls, &[]);
        assert!(!text.bytes().any(|b| b < 32 && b != b'\n' || b == 127));
        assert_eq!(output(mode, b"a.b;[]{}?!", &[]), "a.b;[]{}?!");
    }
}

#[test]
fn tab_stops_follow_displayed_markers() {
    for width in 1..=8 {
        for input in [b"abc\tX".as_slice(), b"\xff\tX", "é\tX".as_bytes()] {
            for mode in ["symbols", "period", "binary"] {
                let text = output(mode, input, &[&format!("--tabs={width}")]);
                let before_x = text.strip_suffix('X').unwrap();
                assert_eq!(
                    unicode_width::UnicodeWidthStr::width(before_x) % width,
                    0,
                    "{mode}, width {width}: {text:?}"
                );
            }
        }
    }
    assert_eq!(output("period", b"\xff\tX\n", &[]), "....X.\n");
}

#[test]
fn new_markers_wrap_at_display_columns() {
    for mode in ["symbols", "period", "binary"] {
        let text = output(
            mode,
            b"abc\tdef\x08ghi\n",
            &[
                "--decorations=always",
                "--wrap=character",
                "--terminal-width=4",
            ],
        );
        assert!(text.lines().count() > 1);
        for line in text.lines() {
            assert!(unicode_width::UnicodeWidthStr::width(line) <= 4, "{line:?}");
        }
    }
}

#[test]
fn notation_is_opt_in_and_available_to_library_callers() {
    for mode in ["symbols", "period", "binary"] {
        bat()
            .args(["-c", mode])
            .write_stdin("ordinary text\twith whitespace\n")
            .assert()
            .success()
            .stdout("ordinary text\twith whitespace\n");
    }
    let mut printer = bat::PrettyPrinter::new();
    let mut text = String::new();
    printer
        .input_from_bytes(b"hello\x08\x1b\n")
        .show_nonprintable(true)
        .nonprintable_notation(bat::NonprintableNotation::Symbols)
        .colored_output(false)
        .print_with_writer(Some(&mut text))
        .unwrap();
    assert_eq!(text, "hello⌫⎋⏎\n");
}

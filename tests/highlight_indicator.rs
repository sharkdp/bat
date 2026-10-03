mod utils;

use utils::command::bat;

fn render(args: &[&str], input: &str) -> String {
    let output = bat()
        .args([
            "--decorations=always",
            "--color=never",
            "--paging=never",
            "--terminal-width=30",
        ])
        .args(args)
        .write_stdin(input)
        .assert()
        .success()
        .get_output()
        .stdout
        .clone();
    String::from_utf8(output).unwrap()
}

#[test]
fn text_indicators_work_without_colors() {
    assert_eq!(
        render(
            &["--style=highlight-indicator", "-H2:3"],
            "one\ntwo\nthree\nfour\n"
        ),
        "  one\n> two\n> three\n  four\n"
    );
    assert_eq!(
        render(
            &["--style=highlight-indicator,numbers", "-H2"],
            "one\ntwo\n"
        ),
        "     1 one\n>    2 two\n"
    );
}

#[test]
fn indicators_continue_across_wrapped_lines() {
    assert_eq!(
        render(
            &[
                "--style=highlight-indicator",
                "-H2",
                "--terminal-width=8",
                "--wrap=character"
            ],
            "one\n123456789\nthree\n"
        ),
        "  one\n> 123456\n> 789\n  three\n"
    );
}

#[test]
fn indicators_respect_ranges_and_numbering_overrides() {
    assert_eq!(
        render(
            &["--style=highlight-indicator", "-H2:4", "-r2:3"],
            "one\n\nthree\nfour\n"
        ),
        "> \n> three\n"
    );
    assert_eq!(
        render(
            &[
                "--number-nonblank",
                "--style=highlight-indicator,numbers",
                "-H2"
            ],
            "one\n\nthree\n"
        ),
        "   1 one\n     \n   2 three\n"
    );
}

#[test]
fn unused_or_disabled_indicators_do_not_change_output() {
    let input = "one\ntwo\n";
    assert_eq!(
        render(&["--style=numbers,+highlight-indicator"], input),
        render(&["--style=numbers"], input)
    );
    assert_eq!(
        render(
            &["--style=full", "-H2", "--style=-highlight-indicator"],
            input
        ),
        render(&["--style=full,-highlight-indicator", "-H2"], input)
    );
    assert_eq!(
        render(
            &["--style=highlight-indicator", "-H2", "--terminal-width=3"],
            input
        ),
        input
    );
}

#[test]
fn library_callers_can_enable_indicators() {
    let mut output = String::new();
    bat::PrettyPrinter::new()
        .input_from_bytes(b"one\ntwo\n")
        .colored_output(false)
        .highlight(2)
        .highlight_indicator(true)
        .print_with_writer(Some(&mut output))
        .unwrap();
    assert_eq!(output, "  one\n> two\n");
}

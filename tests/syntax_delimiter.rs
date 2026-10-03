mod utils;

use utils::command::bat;

fn output(args: &[&str], input: &str) -> Vec<u8> {
    bat()
        .args([
            "--color=always",
            "--theme=TwoDark",
            "--language=rust",
            "--style=plain",
            "--paging=never",
        ])
        .args(args)
        .write_stdin(input)
        .assert()
        .success()
        .get_output()
        .stdout
        .clone()
}

#[test]
fn delimiter_resets_unterminated_comments_and_strings() {
    for first in ["/* unmatched\n", "let text = \"unmatched\n"] {
        let second = "---\nfn main() {}\n";
        let input = format!("{first}{second}");
        let expected = [output(&[], first), output(&[], second)].concat();
        assert_eq!(output(&["--syntax-delimiter=^---$"], &input), expected);
        assert_ne!(output(&[], &input), expected);
    }
}

#[test]
fn delimiter_in_hidden_lines_still_resets_state() {
    let input = "/* unmatched\n---\nfn main() {}\n";
    assert_eq!(
        output(&["--syntax-delimiter=^---$", "--line-range=3:"], input),
        output(&[], "fn main() {}\n")
    );
}

#[test]
fn delimiter_matching_excludes_crlf() {
    let first = "/* unmatched\r\n";
    let second = "---\r\nfn main() {}\r\n";
    assert_eq!(
        output(&["--syntax-delimiter=^---$"], &format!("{first}{second}")),
        [output(&[], first), output(&[], second)].concat()
    );
}

#[test]
fn unmatched_delimiter_keeps_multiline_highlighting() {
    let input = "/* a\nmultiline comment */\nfn main() {}\n";
    assert_eq!(
        output(&["--syntax-delimiter=^---$"], input),
        output(&[], input)
    );
}

#[test]
fn invalid_syntax_delimiter_fails_before_printing() {
    bat()
        .args(["--syntax-delimiter=[", "test.txt"])
        .assert()
        .failure()
        .stdout("");
}

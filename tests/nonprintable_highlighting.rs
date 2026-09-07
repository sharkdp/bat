mod utils;

use utils::command::bat;

fn output(args: &[&str], input: &[u8]) -> String {
    String::from_utf8(
        bat()
            .args([
                "--color=always",
                "--theme=TwoDark",
                "--style=plain",
                "--paging=never",
            ])
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

// Read the SGR style active at an ASCII substring, without depending on theme RGB values.
fn style_at(text: &str, position: usize) -> &str {
    let prefix = &text[..position];
    prefix
        .rfind("\x1b[")
        .map(|start| &prefix[start..start + prefix[start..].find('m').unwrap() + 1])
        .unwrap_or("")
}

#[test]
fn literal_hex_escapes_keep_plain_text_color() {
    let colored = output(&["--show-all"], b"\x86_64 != \\x86_64");
    let positions: Vec<_> = colored.match_indices("\\x86").map(|(i, _)| i).collect();
    assert_eq!(positions.len(), 2);
    let plain = output(&["--language=txt"], b"\\x86");
    assert_eq!(
        style_at(&colored, positions[1]),
        style_at(&plain, plain.find("\\x86").unwrap())
    );
    assert_ne!(
        style_at(&colored, positions[0]),
        style_at(&colored, positions[1])
    );
}

#[test]
fn literal_caret_notation_keeps_plain_color() {
    let colored = output(&["--show-all", "--nonprintable-notation=caret"], b"\x07^G");
    let positions: Vec<_> = colored.match_indices("^G").map(|(i, _)| i).collect();
    assert_eq!(positions.len(), 2);
    let plain = output(&["--language=txt"], b"^G");
    assert_eq!(
        style_at(&colored, positions[1]),
        style_at(&plain, plain.find("^G").unwrap())
    );
}

#[test]
fn explicit_nonprintable_syntax_still_highlights_literal_escapes() {
    let colored = output(&["--language=show-nonprintable"], b"\\x86");
    let plain = output(&["--language=txt"], b"\\x86");
    assert_ne!(
        style_at(&colored, colored.find("\\x86").unwrap()),
        style_at(&plain, plain.find("\\x86").unwrap())
    );
}

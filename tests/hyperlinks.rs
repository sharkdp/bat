mod utils;

use utils::command::bat;

fn output(args: &[&str], input: &str) -> String {
    String::from_utf8(
        bat()
            .args([
                "--decorations=always",
                "--color=never",
                "--style=header,numbers",
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

fn uris(output: &str) -> Vec<&str> {
    output
        .split("\x1b]8;;")
        .skip(1)
        .filter_map(|part| {
            let uri = part.split_once("\x1b\\").unwrap().0;
            (!uri.is_empty()).then_some(uri)
        })
        .collect()
}

fn without_links(mut text: &str) -> String {
    let mut result = String::new();
    while let Some((before, rest)) = text.split_once("\x1b]8;;") {
        result.push_str(before);
        text = rest.split_once("\x1b\\").unwrap().1;
    }
    result.push_str(text);
    result
}

#[test]
fn headers_and_numbers_have_encoded_paths_and_custom_line_targets() {
    let text = output(
        &[
            "--osc8",
            "--file-name=a b#%.rs",
            "--hyperlink-format=vscode://file{path}:{line}",
        ],
        "one\ntwo\n",
    );
    let links = uris(&text);
    assert_eq!(links.len(), 3);
    assert!(links
        .iter()
        .all(|uri| uri.starts_with("vscode://file/") && uri.contains("a%20b%23%25.rs")));
    assert!(links[0].ends_with(":1") && links[1].ends_with(":1") && links[2].ends_with(":2"));
    assert_eq!(
        without_links(&text),
        output(&["--file-name=a b#%.rs"], "one\ntwo\n")
    );
}

#[test]
fn only_highlighted_numbers_are_linked() {
    let text = output(
        &[
            "--osc8-highlight",
            "--file-name=example.txt",
            "--hyperlink-format=file://{path}#{line}",
            "--highlight-line=2",
        ],
        "one\ntwo\n",
    );
    let links = uris(&text);
    assert_eq!(links.len(), 1);
    assert!(links[0].ends_with("#2"));
}

#[test]
fn unnamed_stdin_and_hidden_decorations_do_not_emit_links() {
    assert!(uris(&output(&["--osc8"], "text\n")).is_empty());
    assert!(uris(&output(
        &["--osc8", "--file-name=example.txt", "--style=plain"],
        "text\n"
    ))
    .is_empty());
}

#[test]
fn links_do_not_change_wrapping_or_leak_to_continuations() {
    let args = [
        "--file-name=long-file-name-for-wrapping.txt",
        "--terminal-width=20",
        "--wrap=character",
    ];
    let input = "a line with several wrapped fragments\n";
    let plain = output(&args, input);
    let mut linked_args = args.to_vec();
    linked_args.push("--osc8");
    let linked = output(&linked_args, input);
    assert_eq!(without_links(&linked), plain);
    // Every printed header fragment has its own closed link; continuation numbers are blank.
    assert_eq!(uris(&linked).len(), 4); // Three header fragments and one line number.
    assert!(linked
        .lines()
        .take(3)
        .all(|line| line.ends_with("\x1b]8;;\x1b\\")));
    assert!(linked
        .lines()
        .skip(4)
        .all(|line| !line.contains("\x1b]8;;")));
}

#[test]
fn last_link_mode_wins_and_invalid_templates_fail() {
    let text = output(
        &["--osc8-highlight", "--osc8", "--file-name=example.txt"],
        "text\n",
    );
    assert_eq!(uris(&text).len(), 2);
    bat()
        .args([
            "--osc8",
            "--hyperlink-format=file://{path}\x1b\\",
            "test.txt",
        ])
        .assert()
        .failure()
        .stdout("");
}

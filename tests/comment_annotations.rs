mod utils;
use utils::command::bat;

#[test]
fn annotation_flag_preserves_plain_output_and_is_repeatable() {
    let input = "let text = \"TODO fake\"; // TODO real\n";
    bat()
        .args([
            "--paging=never",
            "--color=never",
            "--style=plain",
            "-l",
            "rs",
            "--highlight-todos",
            "--highlight-todos",
        ])
        .write_stdin(input)
        .assert()
        .success()
        .stdout(input);
}

#[test]
fn library_annotations_change_comment_styles_without_changing_source() {
    let input = b"let text = \"TODO fake\"; // TODO real\n";
    let mut plain = String::new();
    let mut highlighted = String::new();
    for (enabled, output) in [(false, &mut plain), (true, &mut highlighted)] {
        bat::PrettyPrinter::new()
            .input_from_bytes(input)
            .language("rs")
            .theme("Monokai Extended")
            .colored_output(true)
            .true_color(true)
            .highlight_todos(enabled)
            .print_with_writer(Some(output))
            .unwrap();
    }
    assert_ne!(plain, highlighted);
    let strip = regex::Regex::new("\x1b\\[[0-9;]*m").unwrap();
    assert_eq!(
        strip.replace_all(&plain, ""),
        strip.replace_all(&highlighted, "")
    );
    assert!(highlighted.contains("TODO fake"));
}

#[test]
fn annotations_do_not_leak_across_line_ranges_or_wrapping() {
    let output = bat()
        .args([
            "--paging=never",
            "--color=always",
            "--style=plain",
            "--theme=Monokai Extended",
            "-l",
            "rs",
            "--highlight-todos",
            "--line-range=2:",
            "--terminal-width=24",
            "--wrap=word",
        ])
        .write_stdin(
            "/*\nTODO long annotation that wraps over several rows\nordinary next line\n*/\n",
        )
        .assert()
        .success()
        .get_output()
        .stdout
        .clone();
    let text = String::from_utf8(output).unwrap();
    assert!(text.contains("TODO"));
    assert!(text.contains("ordinary next line"));
    let strip = regex::Regex::new("\x1b\\[[0-9;]*m").unwrap();
    assert!(strip.replace_all(&text, "").lines().count() > 3);
}

#[test]
fn long_lines_keep_the_existing_highlighting_limit() {
    let input = format!("// TODO {}\n", "x".repeat(20_000));
    let mut outputs = Vec::new();
    for enabled in [false, true] {
        let mut output = String::new();
        bat::PrettyPrinter::new()
            .input_from_bytes(input.as_bytes())
            .language("rs")
            .theme("Monokai Extended")
            .colored_output(true)
            .true_color(true)
            .highlight_todos(enabled)
            .print_with_writer(Some(&mut output))
            .unwrap();
        outputs.push(output);
    }
    assert_eq!(outputs[0], outputs[1]);
}

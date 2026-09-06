use bat::PrettyPrinter;

#[test]
fn line_number_width() {
    for (width, expected) in [
        (None, "   1 hello\n"),
        (Some(6), "     1 hello\n"),
        (Some(0), "1 hello\n"),
    ] {
        let mut output = String::new();
        PrettyPrinter::new()
            .input_from_bytes(b"hello\n")
            .colored_output(false)
            .line_numbers(true)
            .line_number_width(width)
            .print_with_writer(Some(&mut output))
            .unwrap();
        assert_eq!(output, expected);
    }
}

#[test]
fn syntaxes() {
    let printer = PrettyPrinter::new();
    let syntaxes: Vec<String> = printer.syntaxes().map(|s| s.name).collect();

    // Just do some sanity checking
    assert!(syntaxes.contains(&"Rust".to_string()));
    assert!(syntaxes.contains(&"Java".to_string()));
    assert!(!syntaxes.contains(&"this-language-does-not-exist".to_string()));

    // This language exists but is hidden, so we should not see it; it shall
    // have been filtered out before getting to us
    assert!(!syntaxes.contains(&"Git Common".to_string()));
}

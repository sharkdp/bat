use bat::PrettyPrinter;

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

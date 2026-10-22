use bat::{assets::HighlightingAssets, PrettyPrinter};

#[test]
fn explicit_embedded_assets_preserve_default_behavior() {
    let printer = PrettyPrinter::with_assets(HighlightingAssets::from_binary()).unwrap();
    assert_eq!(
        printer.syntaxes().map(|s| s.name).collect::<Vec<_>>(),
        PrettyPrinter::new()
            .syntaxes()
            .map(|s| s.name)
            .collect::<Vec<_>>()
    );
}

#[test]
fn missing_cache_fails_during_construction() {
    let dir = tempfile::tempdir().unwrap();
    assert!(PrettyPrinter::from_cache(dir.path()).is_err());
}

#[test]
#[cfg(feature = "build-assets")]
fn custom_cache_is_listed_and_used_for_highlighting() {
    let source = tempfile::tempdir().unwrap();
    let cache = tempfile::tempdir().unwrap();
    let syntaxes = source.path().join("syntaxes");
    std::fs::create_dir(&syntaxes).unwrap();
    std::fs::write(
        syntaxes.join("Example.sublime-syntax"),
        r#"%YAML 1.2
---
name: Local Library Example
file_extensions: [local-library-example]
scope: source.local-library-example
contexts:
  main:
    - match: '\bhello\b'
      scope: keyword.control
"#,
    )
    .unwrap();
    bat::assets::build(
        source.path(),
        true,
        false,
        cache.path(),
        env!("CARGO_PKG_VERSION"),
    )
    .unwrap();
    let mut printer = PrettyPrinter::from_cache(cache.path()).unwrap();
    assert!(printer
        .syntaxes()
        .any(|s| s.name == "Local Library Example"));
    assert!(!PrettyPrinter::new()
        .syntaxes()
        .any(|s| s.name == "Local Library Example"));
    let mut output = String::new();
    printer
        .language("Local Library Example")
        .theme("ansi")
        .input_from_bytes(b"hello world\n")
        .print_with_writer(Some(&mut output))
        .unwrap();
    assert_eq!(console::strip_ansi_codes(&output), "hello world\n");
    assert!(output.contains("\x1b["));

    // A cache can load its themes successfully but fail later when syntaxes
    // are deserialized. The constructor must surface that error, not panic
    // when the caller subsequently enumerates syntaxes.
    std::fs::write(cache.path().join("syntaxes.bin"), b"invalid cache").unwrap();
    assert!(PrettyPrinter::from_cache(cache.path()).is_err());
}

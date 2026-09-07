mod utils;
use utils::command::bat;

fn listing(extra: &[&str]) -> String {
    let output = bat()
        .args([
            "--list-languages",
            "--paging=never",
            "--color=never",
            "--terminal-width=500",
        ])
        .args(extra)
        .assert()
        .success()
        .get_output()
        .stdout
        .clone();
    String::from_utf8(output).unwrap()
}

#[test]
fn native_and_mapped_extensions_share_the_same_display_format() {
    for args in [vec![], vec!["--decorations=always"]] {
        let output = listing(&args);
        let diff = output
            .lines()
            .find(|line| line.starts_with("Diff"))
            .unwrap();
        assert!(diff.contains("debdiff"), "{diff}");
        assert!(!diff.contains("*.debdiff"), "{diff}");
        assert!(diff.contains("patch"), "{diff}");
    }
}

#[test]
fn complex_globs_and_exact_filenames_remain_explicit() {
    let output = listing(&[
        "--map-syntax=*.customsuffix:Rust",
        "--map-syntax=*.d.custom:Rust",
        "--map-syntax=*.[ab]:Rust",
        "--map-syntax=*.{c,h}:Rust",
        "--map-syntax=**/*.scoped:Rust",
        "--map-syntax=Project.lock:Rust",
        "--map-syntax=.hiddenconfig:Rust",
        "--map-syntax=*.rs:Rust",
    ]);
    let rust = output
        .lines()
        .find(|line| line.starts_with("Rust:"))
        .unwrap();
    let entries: Vec<_> = rust.strip_prefix("Rust:").unwrap().split(',').collect();
    for expected in [
        "customsuffix",
        "d.custom",
        "*.[ab]",
        "**/*.scoped",
        "Project.lock",
        ".hiddenconfig",
    ] {
        assert!(entries.contains(&expected), "missing {expected}: {rust}");
    }
    assert!(rust.contains("*.{c,h}"), "{rust}");
    assert_eq!(entries.iter().filter(|e| **e == "rs").count(), 1, "{rust}");
    assert!(!rust.contains("*.customsuffix"), "{rust}");
}

#[test]
fn formatting_the_listing_does_not_change_mapping_behavior() {
    let input = "fn main() {}\n";
    let render = |args: &[&str]| {
        bat()
            .args(["--color=always", "--style=plain", "--paging=never"])
            .args(args)
            .write_stdin(input)
            .assert()
            .success()
            .get_output()
            .stdout
            .clone()
    };
    assert_eq!(
        render(&[
            "--map-syntax=*.customsuffix:Rust",
            "--file-name=example.customsuffix"
        ]),
        render(&["--language=Rust"]),
    );
}

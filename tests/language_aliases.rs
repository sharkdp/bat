mod utils;
use utils::command::bat;

fn highlighted(args: &[&str], input: &str) -> Vec<u8> {
    bat()
        .args(["--style=plain", "--color=always", "--paging=never"])
        .args(args)
        .write_stdin(input)
        .assert()
        .success()
        .get_output()
        .stdout
        .clone()
}

#[test]
fn aliases_match_case_insensitively_and_preserve_canonical_names() {
    let source = "using System;\n";
    let canonical = highlighted(&["-l", "C#"], source);
    assert_eq!(
        highlighted(&["-m", "csharp=C#", "-l", "CSHARP"], source),
        canonical
    );
    assert_eq!(
        highlighted(&["-m", "csharp=C#", "-l", "C#"], source),
        canonical
    );
    assert_eq!(
        highlighted(&["-m", "csharp=C#", "--fallback-syntax=csharp"], source),
        canonical
    );
}

#[test]
fn aliases_do_not_participate_in_filename_globs() {
    let source = "using System;\n";
    assert_eq!(
        highlighted(&["-m", "csharp=C#", "--file-name=csharp"], source),
        highlighted(&["--file-name=csharp"], source)
    );
    assert_eq!(
        highlighted(&["-m", "name=value:JSON", "--file-name=name=value"], "{}\n"),
        highlighted(&["-l", "JSON"], "{}\n")
    );
}

#[test]
fn later_aliases_take_precedence_without_following_alias_chains() {
    assert_eq!(
        highlighted(
            &["-m", "custom=JSON", "-m", "CUSTOM=Python", "-lcustom"],
            "def f(): pass\n"
        ),
        highlighted(&["-lPython"], "def f(): pass\n")
    );
    bat()
        .args([
            "-m",
            "first=second",
            "-m",
            "second=first",
            "-lfirst",
            "--color=always",
        ])
        .write_stdin("input\n")
        .assert()
        .failure()
        .stderr(predicates::str::contains("unknown syntax"));
}

#[test]
fn invalid_aliases_are_rejected() {
    for mapping in ["=JSON", "custom=", "c*=JSON"] {
        bat()
            .args(["-m", mapping])
            .write_stdin("input\n")
            .assert()
            .failure()
            .stderr(predicates::str::contains("Invalid language alias"));
    }
}

#[test]
fn library_mapping_supports_language_aliases() {
    let mut mapping = bat::SyntaxMapping::new();
    mapping.insert_language_alias("csharp", "C#").unwrap();
    let mut output = String::new();
    bat::PrettyPrinter::new()
        .language("csharp")
        .syntax_mapping(mapping)
        .input_from_bytes(b"using System;\n")
        .print_with_writer(Some(&mut output))
        .unwrap();
    assert_eq!(console::strip_ansi_codes(&output), "using System;\n");
}

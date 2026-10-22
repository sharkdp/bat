mod utils;

use utils::command::bat;

const OPTIONS: &[&str] = &[
    "--color=never",
    "--decorations=always",
    "--paging=never",
    "--wrap=never",
    "--terminal-width=80",
];

fn stdin_output(args: &[&str], content: &str) -> String {
    String::from_utf8(
        bat()
            .args(OPTIONS)
            .args(args)
            .write_stdin(content)
            .assert()
            .success()
            .get_output()
            .stdout
            .clone(),
    )
    .unwrap()
}

#[test]
fn each_input_selects_its_own_style_without_color() {
    let dir = tempfile::tempdir().unwrap();
    let json = dir.path().join("example.json");
    let text = dir.path().join("example.txt");
    std::fs::write(&json, "{}\n").unwrap();
    std::fs::write(&text, "text\n").unwrap();
    let expected = format!("{{}}\n{}", stdin_output(&["--style=numbers"], "text\n"));
    bat()
        .args(OPTIONS)
        .args(["--style=numbers", "--style-for", "json", "plain"])
        .args([json, text])
        .assert()
        .success()
        .stdout(expected);
}

#[test]
fn syntax_styles_follow_detection_explicit_language_and_fallbacks() {
    let expected = stdin_output(&["--style=numbers"], "{}\n");
    for syntax_args in [
        vec!["--language=json"],
        vec!["--file-name=input.json"],
        vec!["--file-name=input.custom", "--map-syntax=*.custom:JSON"],
        vec!["--fallback-syntax=json"],
    ] {
        let mut args = vec!["--style=plain", "--style-for", "JSON", "numbers"];
        args.extend(syntax_args);
        assert_eq!(stdin_output(&args, "{}\n"), expected);
    }
    assert_eq!(
        stdin_output(
            &[
                "--style=numbers",
                "--style-for",
                "Bourne Again Shell (bash)",
                "plain"
            ],
            "#!/bin/sh\necho hello\n"
        ),
        "#!/bin/sh\necho hello\n"
    );
}

#[test]
fn repeated_syntax_styles_apply_modifiers_in_order() {
    let output = stdin_output(
        &[
            "--language=json",
            "--style=numbers,header",
            "--file-name=test.json",
            "--style-for",
            "JSON",
            "-header",
            "--style-for",
            "json",
            "+grid",
        ],
        "{}\n",
    );
    assert_eq!(output, stdin_output(&["--style=numbers,grid"], "{}\n"));
    assert_eq!(
        stdin_output(
            &[
                "--language=json",
                "--style=numbers",
                "--style-for",
                "JSON",
                "full",
                "--style-for",
                "JSON",
                "plain"
            ],
            "{}\n"
        ),
        "{}\n"
    );
}

#[test]
fn explicit_plain_number_and_decoration_flags_take_precedence() {
    for flag in ["--plain", "--number", "--decorations=never", "--unbuffered"] {
        let actual = stdin_output(
            &[
                "--language=json",
                "--style=plain",
                "--style-for",
                "JSON",
                "numbers",
                flag,
            ],
            "{}\n",
        );
        let expected = stdin_output(&["--style=plain", flag], "{}\n");
        assert_eq!(actual, expected, "{flag}");
    }
}

#[test]
fn pipe_output_stays_plain_and_invalid_styles_are_reported() {
    bat()
        .args([
            "--style-for",
            "JSON",
            "full",
            "--language=json",
            "--color=never",
        ])
        .write_stdin("{}\n")
        .assert()
        .success()
        .stdout("{}\n");
    bat()
        .args(["--style-for", "JSON", "nonexistent-style"])
        .assert()
        .failure()
        .stderr(predicates::str::contains("Unknown style"));
}

#[test]
fn library_syntax_styles_expand_aliases_and_honor_the_last_entry() {
    use bat::style::StyleComponent;
    let mut full_output = String::new();
    bat::PrettyPrinter::new()
        .input_from_bytes(b"{}\n")
        .language("json")
        .colored_output(false)
        .style_for("JSON", &[StyleComponent::Full])
        .print_with_writer(Some(&mut full_output))
        .unwrap();
    assert!(full_output.contains("Size: -"), "{full_output}");
    let mut output = String::new();
    bat::PrettyPrinter::new()
        .input_from_bytes(b"{}\n")
        .language("json")
        .colored_output(false)
        .line_numbers(true)
        .style_for("JSON", &[StyleComponent::Full])
        .style_for("json", &[StyleComponent::Plain])
        .print_with_writer(Some(&mut output))
        .unwrap();
    assert_eq!(output, "{}\n");
}

#[cfg(feature = "git")]
#[test]
fn selected_style_loads_git_changes() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("example.json");
    std::fs::write(&path, "{\n  \"value\": 1\n}\n").unwrap();
    for args in [vec!["init", "--quiet"], vec!["add", "example.json"]] {
        assert!(std::process::Command::new("git")
            .args(args)
            .current_dir(dir.path())
            .status()
            .unwrap()
            .success());
    }
    std::fs::write(&path, "{\n  \"value\": 2\n}\n").unwrap();
    let render = |args: &[&str]| {
        bat()
            .args(OPTIONS)
            .args(args)
            .arg(&path)
            .assert()
            .success()
            .get_output()
            .stdout
            .clone()
    };
    let expected = render(&["--style=changes"]);
    assert_ne!(expected, render(&["--style=plain"]));
    assert_eq!(
        expected,
        render(&["--style=plain", "--style-for", "JSON", "changes"])
    );
}

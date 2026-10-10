mod utils;

use utils::command::bat;

fn output(args: &[&str], input: &str) -> Vec<u8> {
    bat()
        .args(["--decorations=always", "--color=never", "--paging=never"])
        .args(args)
        .write_stdin(input)
        .assert()
        .success()
        .get_output()
        .stdout
        .clone()
}

const CONTEXTS: [&str; 3] = [
    "--style-single-file=numbers",
    "--style-stdin=plain",
    "--style-multiple-files=header",
];

#[test]
fn single_file_and_stdin_use_distinct_styles() {
    let mut args = CONTEXTS.to_vec();
    args.push("test.txt");
    assert_eq!(
        output(&args, ""),
        output(&["--style=numbers", "test.txt"], "")
    );
    assert_eq!(output(&CONTEXTS, "text\n"), b"text\n");
    assert_eq!(
        output(&[CONTEXTS[0], CONTEXTS[1], CONTEXTS[2], "-"], "text\n"),
        b"text\n"
    );
}

#[test]
fn mixed_inputs_all_use_multiple_file_style() {
    for inputs in [
        vec!["test.txt", "test.txt"],
        vec!["-", "test.txt"],
        vec!["test.txt", "-"],
    ] {
        let mut args = CONTEXTS.to_vec();
        args.extend(&inputs);
        let mut expected = vec!["--style=header"];
        expected.extend(&inputs);
        assert_eq!(output(&args, "stdin\n"), output(&expected, "stdin\n"));
    }
}

#[test]
fn context_modifiers_build_on_general_style() {
    assert_eq!(
        output(
            &[
                "--style=numbers,header",
                "--style-single-file=-header",
                "test.txt"
            ],
            ""
        ),
        output(&["--style=numbers", "test.txt"], "")
    );
    assert_eq!(
        output(
            &[
                "--style=numbers",
                "--style-multiple-files=plain",
                "test.txt"
            ],
            ""
        ),
        output(&["--style=numbers", "test.txt"], "")
    );
}

#[test]
fn plain_numbering_and_disabled_decorations_take_precedence() {
    for flag in [
        "--plain",
        "--number",
        "--number-nonblank",
        "--decorations=never",
    ] {
        assert_eq!(
            output(&["--style-stdin=header", flag], "one\n\ntwo\n"),
            output(&[flag], "one\n\ntwo\n")
        );
    }
}

#[test]
fn invalid_context_style_is_rejected() {
    bat()
        .args(["--style-single-file=invalid", "test.txt"])
        .assert()
        .failure()
        .stdout("");
}

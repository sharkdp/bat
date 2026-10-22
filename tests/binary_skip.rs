mod utils;

use utils::command::bat;

#[test]
fn skip_binary_inputs_when_redirected() {
    bat()
        .args(["--binary=skip", "test.txt", "test.binary", "test.txt"])
        .assert()
        .success()
        .stdout("hello world\nhello world\n")
        .stderr("");
}

#[test]
fn skip_binary_stdin_even_with_show_all() {
    for args in [vec!["--binary=skip"], vec!["--binary=skip", "--show-all"]] {
        bat()
            .args(args)
            .write_stdin(b"text\n\0binary\n" as &[u8])
            .assert()
            .success()
            .stdout("")
            .stderr("");
    }
}

#[test]
fn skipped_inputs_do_not_add_headers_or_padding() {
    let expected = bat()
        .args(["--decorations=always", "--style=header", "test.txt"])
        .assert()
        .success()
        .get_output()
        .stdout
        .clone();
    bat()
        .args([
            "--binary=skip",
            "--decorations=always",
            "--style=header",
            "test.binary",
            "test.txt",
            "test.binary",
        ])
        .assert()
        .success()
        .stdout(expected)
        .stderr("");
}

#[test]
fn skip_preserves_utf16_and_unterminated_text_when_redirected() {
    for file in [
        "test_UTF-16LE.txt",
        "test_UTF-16BE.txt",
        "single-line.txt",
        "empty.txt",
    ] {
        let expected = bat()
            .arg(file)
            .assert()
            .success()
            .get_output()
            .stdout
            .clone();
        bat()
            .args(["--binary=skip", file])
            .assert()
            .success()
            .stdout(expected);
    }
}

#[test]
fn default_binary_output_is_unchanged() {
    let input = b"text\n\0binary\n";
    bat()
        .write_stdin(input as &[u8])
        .assert()
        .success()
        .stdout(input as &[u8]);
}

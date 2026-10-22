mod utils;
use utils::command::bat;

#[test]
fn compact_mode_retains_numbers_and_vertical_grid_without_header_rules() {
    bat()
        .args([
            "--color=never",
            "--paging=never",
            "--terminal-width=80",
            "--decorations=compact",
        ])
        .write_stdin("first\nsecond\n")
        .assert()
        .success()
        .stdout("===> STDIN <===\n   1 │ first\n   2 │ second\n");
}

#[test]
fn compact_multiple_files_have_one_heading_each_and_keep_sizes_if_selected() {
    bat().args(["--color=never", "--paging=never", "--decorations=compact", "--style=header,header-filesize", "single-line.txt", "single-line.txt"])
        .assert().success().stdout("===> single-line.txt <===\nSize: 11 B\nSingle Line\n===> single-line.txt <===\nSize: 11 B\nSingle Line\n");
}

#[test]
fn compact_mode_respects_plain_and_number_only_styles() {
    for (style, expected) in [("plain", "text\n"), ("numbers", "   1 text\n")] {
        bat()
            .args([
                "--color=never",
                "--paging=never",
                "--decorations=compact",
                "--style",
                style,
            ])
            .write_stdin("text\n")
            .assert()
            .success()
            .stdout(expected);
    }
}

#[test]
fn compact_mode_preserves_binary_and_empty_metadata() {
    bat()
        .args(["--color=never", "--paging=never", "--decorations=compact"])
        .write_stdin("\0binary")
        .assert()
        .success()
        .stdout("===> STDIN   <BINARY> <===\n");
    bat()
        .args(["--color=never", "--paging=never", "--decorations=compact"])
        .write_stdin("")
        .assert()
        .success()
        .stdout("===> STDIN   <EMPTY> <===\n");
    bat()
        .args([
            "--color=never",
            "--paging=never",
            "--decorations=compact",
            "--quiet-empty",
        ])
        .write_stdin("")
        .assert()
        .success()
        .stdout("");
}

#[test]
fn compact_headings_sanitize_names_and_work_in_narrow_output() {
    bat()
        .args([
            "--color=never",
            "--paging=never",
            "--decorations=compact",
            "--file-name=long\u{1b}[31mname.txt",
            "--terminal-width=1",
        ])
        .write_stdin("xy\n")
        .assert()
        .success()
        .stdout("===> long^[[31mname.txt <===\nx\ny\n");
}

#[test]
fn library_supports_compact_headings() {
    let mut output = String::new();
    bat::PrettyPrinter::new()
        .input_from_bytes(b"text\n")
        .header(true)
        .line_numbers(true)
        .grid(true)
        .compact_headers(true)
        .colored_output(false)
        .print_with_writer(Some(&mut output))
        .unwrap();
    assert_eq!(output, "===> READER <===\n   1 │ text\n");
}

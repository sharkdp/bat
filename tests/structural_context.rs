use assert_cmd::Command;

mod utils;

fn bat() -> Command {
    let mut cmd = utils::command::bat();
    cmd.args(["--paging=never", "--color=never", "--style=plain"]);
    cmd
}

const SOURCE: &str = "int global;\nint first() {\n return 1;\n}\n\nint second() {\n return 2;\n}\n";

#[test]
fn selects_independent_functions_without_adjacent_globals() {
    bat()
        .args(["-l", "c", "-W", "3"])
        .write_stdin(SOURCE)
        .assert()
        .success()
        .stdout("int first() {\n return 1;\n}\n");
    bat()
        .args(["-l", "c", "-W1"])
        .write_stdin(SOURCE)
        .assert()
        .success()
        .stdout("int global;\n");
    bat()
        .args(["-l", "c", "-W3", "-W7", "-W3"])
        .write_stdin(SOURCE)
        .assert()
        .success()
        .stdout("int first() {\n return 1;\n}\nint second() {\n return 2;\n}\n");
}

#[test]
fn preserves_physical_line_numbers_and_snips() {
    let output = bat()
        .args([
            "-l",
            "c",
            "-W3",
            "-W7",
            "--decorations=always",
            "--style=numbers,snip",
            "--terminal-width=40",
        ])
        .write_stdin(SOURCE)
        .assert()
        .success()
        .get_output()
        .stdout
        .clone();
    let output = String::from_utf8(output).unwrap();
    assert!(output.contains("2 int first"), "{output}");
    assert!(output.contains("6 int second"), "{output}");
    assert!(output.contains("8<"), "{output}");
}

#[test]
fn utf16_and_crlf_retain_source_line_boundaries() {
    let utf16: Vec<u8> = std::iter::once(0xfeff)
        .chain(SOURCE.encode_utf16())
        .flat_map(u16::to_le_bytes)
        .collect();
    bat()
        .args(["-l", "c", "-W3", "--decorations=always"])
        .write_stdin(utf16)
        .assert()
        .success()
        .stdout("int first() {\n return 1;\n}\n");
    bat()
        .args(["-l", "c", "-W3"])
        .write_stdin(SOURCE.replace('\n', "\r\n"))
        .assert()
        .success()
        .stdout("int first() {\r\n return 1;\r\n}\r\n");
}

#[test]
fn recognizes_each_file_independently_with_auto_detection() {
    let dir = tempfile::tempdir().unwrap();
    let c = dir.path().join("a.c");
    let java = dir.path().join("b.java");
    std::fs::write(&c, SOURCE).unwrap();
    std::fs::write(&java, "class Demo {\n int value() {\n  return 2;\n }\n}\n").unwrap();
    bat()
        .arg("-W3")
        .arg(c)
        .arg(java)
        .assert()
        .success()
        .stdout("int first() {\n return 1;\n}\n int value() {\n  return 2;\n }\n");
}

#[test]
fn unknown_syntax_and_incomplete_definitions_use_selected_lines() {
    bat()
        .args(["-l", "txt", "-W2"])
        .write_stdin("hello {\nworld\n}\n")
        .assert()
        .success()
        .stdout("world\n");
    bat()
        .args(["-l", "c", "-W2"])
        .write_stdin("int broken() {\n return 1;\n")
        .assert()
        .success()
        .stdout(" return 1;\n");
    bat()
        .args(["-l", "c", "-W99"])
        .write_stdin(SOURCE)
        .assert()
        .success()
        .stdout("");
}

#[test]
fn folds_c_imports_functions_and_comments_without_changing_strings() {
    let source = "#include <stdio.h>\n#include <stdlib.h>\n\n/* comment\n * interior\n */\nint first() {\n puts(\"{ not a block }\");\n return 0;\n}\nconst char *s = \"{ untouched }\";\n";
    bat().args(["-l", "c", "--fold", "--fold"]).write_stdin(source).assert().success().stdout("#include <stdio.h>\n\n/* comment\n */\nint first() {\n}\nconst char *s = \"{ untouched }\";\n");
}

#[test]
fn folds_java_import_groups_and_outer_class_once() {
    let source = "import java.io.File;\nimport java.io.Reader;\n\nclass Demo {\n int method() {\n  return 2;\n }\n}\n";
    bat()
        .args(["-l", "java", "--fold"])
        .write_stdin(source)
        .assert()
        .success()
        .stdout("import java.io.File;\n\nclass Demo {\n}\n");
}

#[test]
fn keeps_single_line_blocks_and_unclosed_blocks_visible() {
    let source = "int first() { return 1; }\nint unfinished() {\n return 2;\n";
    bat()
        .args(["-l", "c", "--fold"])
        .write_stdin(source)
        .assert()
        .success()
        .stdout(source);
}

#[test]
fn rejects_conflicting_modes_invalid_line_numbers_and_binary_text() {
    for args in [
        vec!["-W0"],
        vec!["-W1", "--unbuffered"],
        vec!["--fold", "--line-range=2"],
        vec!["-W1", "--fold"],
    ] {
        bat().args(args).write_stdin(SOURCE).assert().failure();
    }
    bat()
        .arg("-W1")
        .write_stdin(b"\0binary".to_vec())
        .assert()
        .failure();
}

#[test]
fn library_context_and_folding_use_the_same_selection() {
    let mut context = String::new();
    bat::PrettyPrinter::new()
        .input_from_bytes(SOURCE.as_bytes())
        .language("C")
        .function_context(3)
        .colored_output(false)
        .print_with_writer(Some(&mut context))
        .unwrap();
    assert_eq!(context, "int first() {\n return 1;\n}\n");
    let mut folded = String::new();
    bat::PrettyPrinter::new()
        .input_from_bytes(SOURCE.as_bytes())
        .language("C")
        .fold(true)
        .colored_output(false)
        .print_with_writer(Some(&mut folded))
        .unwrap();
    assert_eq!(
        folded,
        "int global;\nint first() {\n}\n\nint second() {\n}\n"
    );
}

#[test]
fn colored_context_matches_explicit_range_and_highlight() {
    let expected = bat()
        .args([
            "-l",
            "c",
            "--color=always",
            "--line-range=2:4",
            "--highlight-line=3",
        ])
        .write_stdin(SOURCE)
        .assert()
        .success()
        .get_output()
        .stdout
        .clone();
    bat()
        .args(["-l", "c", "--color=always", "-W3"])
        .write_stdin(SOURCE)
        .assert()
        .success()
        .stdout(expected);
}

#[test]
fn utf16be_and_unclosed_comments_do_not_loop_or_disappear() {
    let utf16: Vec<u8> = std::iter::once(0xfeff)
        .chain(SOURCE.encode_utf16())
        .flat_map(u16::to_be_bytes)
        .collect();
    bat()
        .args(["-l", "c", "-W3", "--decorations=always"])
        .write_stdin(utf16)
        .assert()
        .success()
        .stdout("int first() {\n return 1;\n}\n");
    let text = "/* never closed\n still comment\n still comment\n";
    bat()
        .args(["-l", "c", "--fold"])
        .write_stdin(text)
        .assert()
        .success()
        .stdout(text);
}

#[test]
fn adjacent_block_comments_preserve_code_on_their_shared_line() {
    let text = "/* first\n * inside first\n */ int visible = 1; /* second\n * inside second\n */\n";
    bat()
        .args(["-l", "c", "--fold"])
        .write_stdin(text)
        .assert()
        .success()
        .stdout("/* first\n */ int visible = 1; /* second\n */\n");
}

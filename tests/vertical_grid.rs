mod utils;

use utils::command::bat;

#[test]
fn vertical_grid_has_no_horizontal_borders() {
    bat()
        .args([
            "--style=numbers,grid-vertical",
            "--decorations=always",
            "--color=never",
        ])
        .write_stdin("one\ntwo\n")
        .assert()
        .success()
        .stdout("   1 │ one\n   2 │ two\n");
}

#[test]
fn vertical_grid_connects_rules_between_files() {
    bat()
        .args([
            "--style=numbers,grid-vertical,rule",
            "--decorations=always",
            "--color=never",
            "--terminal-width=20",
            "test.txt",
            "test.txt",
        ])
        .assert()
        .success()
        .stdout("   1 │ hello world\n─────┼──────────────\n   1 │ hello world\n");
}

#[test]
fn vertical_grid_aligns_wrapped_lines_and_headers() {
    bat()
        .args([
            "--style=numbers,grid-vertical,header",
            "--decorations=always",
            "--color=never",
            "--terminal-width=12",
            "--wrap=character",
        ])
        .write_stdin("abcdefgh\n")
        .assert()
        .success()
        .stdout("     │ STDIN\n   1 │ abcde\n     │ fgh\n");
}

#[test]
fn vertical_grid_respects_small_terminals_and_missing_sidebar() {
    for style in ["numbers,grid-vertical", "grid-vertical"] {
        bat()
            .args([
                "--style",
                style,
                "--decorations=always",
                "--color=never",
                "--terminal-width=5",
            ])
            .write_stdin("abc\n")
            .assert()
            .success()
            .stdout("abc\n");
    }
}

#[test]
fn removing_grid_keeps_explicit_vertical_separator() {
    bat()
        .args([
            "--style=numbers,grid,grid-vertical",
            "--style=-grid",
            "--decorations=always",
            "--color=never",
        ])
        .write_stdin("one\n")
        .assert()
        .success()
        .stdout("   1 │ one\n");
}

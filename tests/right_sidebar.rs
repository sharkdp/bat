use bat::{PrettyPrinter, WrappingMode};
use std::fs;
use std::process::Command;
use tempfile::tempdir;

mod utils;
use utils::command::bat;

fn render(arguments: &[&str], input: &str) -> String {
    let output = bat()
        .args([
            "--color=never",
            "--decorations=always",
            "--paging=never",
            "--terminal-width=20",
            "--style=numbers,grid,sidebar-right",
        ])
        .args(arguments)
        .write_stdin(input)
        .assert()
        .success()
        .get_output()
        .stdout
        .clone();
    String::from_utf8(output).unwrap()
}

fn rows(output: &str) -> Vec<&str> {
    output.lines().filter(|line| line.contains('│')).collect()
}

#[test]
fn numbers_grid_and_horizontal_rules_move_together() {
    assert_eq!(
        render(&[], "short\nnext\n"),
        concat!(
            "──────────────┬─────\n",
            "short         │    1\n",
            "next          │    2\n",
            "──────────────┴─────\n",
        )
    );
}

#[test]
fn character_and_word_wrap_keep_the_number_on_the_first_row() {
    let output = render(&["--wrap=character"], &("界".repeat(10) + "\n"));
    assert_eq!(
        rows(&output),
        ["界界界界界界  │    1", "界界界界      │     "]
    );
    for line in rows(&output) {
        assert_eq!(console::measure_text_width(line), 20);
    }
    let output = render(&["--wrap=word"], "alpha beta gamma delta\n");
    assert_eq!(
        rows(&output),
        ["alpha beta    │    1", "gamma delta   │     "]
    );
}

#[test]
fn unwrapped_unicode_line_endings_and_long_content_are_preserved() {
    let output = render(&["--wrap=never"], "界界\r\nlast");
    assert!(output.contains("界界          │    1\r\n"));
    assert!(output.contains("last          │    2\n"));
    for line in rows(&output) {
        assert_eq!(console::measure_text_width(line), 20);
    }
    let text = "x".repeat(30);
    let output = render(&["--wrap=never"], &(text.clone() + "\n"));
    assert_eq!(rows(&output), [format!("{text} │    1")]);
}

#[test]
fn tabs_blank_numbers_and_ansi_highlights_remain_aligned() {
    let output = render(
        &["--tabs=4", "--number-nonblank", "--wrap=never"],
        "a\tb\n\n",
    );
    assert_eq!(output, format!("{:<19}1\n{}\n", "a   b", " ".repeat(20)));
    for wrap in ["never", "character", "word"] {
        let output = render(
            &[
                "--wrap",
                wrap,
                "--color=always",
                "--theme=Monokai Extended",
                "--highlight-line=1",
            ],
            "\x1b[31mred\x1b[0m text\n",
        );
        let plain = console::strip_ansi_codes(&output);
        assert_eq!(rows(&plain), ["red text      │    1"]);
        assert!(
            output.contains("48;"),
            "missing highlighted background: {output:?}"
        );
    }
}

#[test]
fn headers_and_snips_follow_the_right_grid() {
    let output = render(
        &[
            "--style=full,sidebar-right",
            "--file-name=notes.txt",
            "--line-range=1:1",
            "--line-range=3:3",
        ],
        "first\nskip\nlast\n",
    );
    for row in rows(&output) {
        assert_eq!(console::measure_text_width(row), 20, "{row:?}");
        assert_eq!(
            console::measure_text_width(row.split_once('│').unwrap().0),
            14,
            "{row:?}"
        );
    }
    let snip = output.lines().find(|line| line.contains("8<")).unwrap();
    assert!(snip.find("8<").unwrap() < snip.find("...").unwrap());
}

#[test]
fn placement_is_opt_in_and_does_not_enable_decorations() {
    assert_eq!(
        render(&["--style=plain,sidebar-right"], "unchanged"),
        render(&["--style=plain"], "unchanged")
    );
    let left = render(&["--style=-sidebar-right"], "text\n");
    assert!(left.contains("   1 │ text\n"));
    let narrow = render(&["--terminal-width=5"], "small\n");
    assert!(!narrow.contains('│'));
    assert!(narrow.contains("small\n"));
}

#[test]
fn library_callers_can_place_and_reset_the_sidebar() {
    for right in [true, false] {
        let mut output = String::new();
        PrettyPrinter::new()
            .input_from_bytes(b"text\n")
            .colored_output(false)
            .line_numbers(true)
            .grid(true)
            .term_width(20)
            .wrapping_mode(WrappingMode::Character)
            .sidebar_right(!right)
            .sidebar_right(right)
            .print_with_writer(Some(&mut output))
            .unwrap();
        assert!(output.contains(if right {
            "text          │    1"
        } else {
            "   1 │ text"
        }));
    }
}

#[cfg(feature = "git")]
#[test]
fn git_change_markers_remain_between_the_grid_and_numbers() {
    let directory = tempdir().unwrap();
    let git = |args: &[&str]| {
        let output = Command::new("git")
            .current_dir(directory.path())
            .args([
                "-c",
                "user.name=Test",
                "-c",
                "user.email=test@example.invalid",
                "-c",
                "commit.gpgsign=false",
                "-c",
                "core.autocrlf=false",
            ])
            .args(args)
            .output()
            .unwrap();
        assert!(
            output.status.success(),
            "{}",
            String::from_utf8_lossy(&output.stderr)
        );
    };
    git(&["init", "--quiet"]);
    let path = directory.path().join("example.txt");
    fs::write(&path, "old\n").unwrap();
    git(&["add", "example.txt"]);
    git(&["commit", "--quiet", "-m", "Initial"]);
    fs::write(&path, "new\n").unwrap();
    let result = bat()
        .args([
            "--paging=never",
            "--color=never",
            "--decorations=always",
            "--terminal-width=24",
            "--style=numbers,changes,grid,sidebar-right",
        ])
        .arg(path)
        .assert()
        .success()
        .get_output()
        .stdout
        .clone();
    let output = String::from_utf8(result).unwrap();
    assert_eq!(rows(&output), ["new             │ ~    1"]);
}

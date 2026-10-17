#![cfg(feature = "git")]

use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use tempfile::{tempdir, TempDir};

mod utils;
use utils::command::bat;

struct Repo {
    directory: TempDir,
    file: PathBuf,
}

fn git(path: &Path, args: &[&str]) -> String {
    let output = Command::new("git")
        .current_dir(path)
        .args(args)
        .env("GIT_CONFIG_NOSYSTEM", "1")
        .env(
            "GIT_CONFIG_GLOBAL",
            if cfg!(windows) { "NUL" } else { "/dev/null" },
        )
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "git {args:?}: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    String::from_utf8_lossy(&output.stdout).into_owned()
}

impl Repo {
    fn empty() -> Self {
        let directory = tempdir().unwrap();
        git(directory.path(), &["init", "-b", "main"]);
        git(directory.path(), &["config", "user.name", "Committer"]);
        git(
            directory.path(),
            &["config", "user.email", "committer@example.test"],
        );
        git(directory.path(), &["config", "core.autocrlf", "false"]);
        let file = directory.path().join("source.rs");
        Self { directory, file }
    }

    fn new() -> Self {
        let repo = Self::empty();
        fs::write(&repo.file, "alpha\nbravo\ncharlie\n").unwrap();
        repo.commit("Ada", "initial");
        fs::write(&repo.file, "alpha\nBETA\ncharlie\n").unwrap();
        repo.commit("Bea", "edit");
        repo
    }

    fn commit(&self, author: &str, subject: &str) {
        git(self.directory.path(), &["add", "-A"]);
        let output = Command::new("git")
            .current_dir(self.directory.path())
            .args(["commit", "-m", subject])
            .env("GIT_AUTHOR_NAME", author)
            .env("GIT_AUTHOR_EMAIL", "author@example.test")
            .env("GIT_COMMITTER_NAME", "Committer")
            .env("GIT_COMMITTER_EMAIL", "committer@example.test")
            .env("GIT_AUTHOR_DATE", "2020-02-03T04:05:06+02:00")
            .env("GIT_COMMITTER_DATE", "2021-03-04T05:06:07-03:00")
            .env("GIT_CONFIG_NOSYSTEM", "1")
            .env(
                "GIT_CONFIG_GLOBAL",
                if cfg!(windows) { "NUL" } else { "/dev/null" },
            )
            .output()
            .unwrap();
        assert!(
            output.status.success(),
            "{}",
            String::from_utf8_lossy(&output.stderr)
        );
    }

    fn output(&self, options: &[&str]) -> String {
        let mut command = bat();
        if !options
            .iter()
            .any(|option| option.starts_with("--blame-format"))
        {
            command.arg("--blame-format=%h");
        }
        let bytes = command
            .args([
                "--paging=never",
                "--color=never",
                "--style=plain",
                "--blame",
                "--terminal-width=120",
            ])
            .args(options)
            .arg(&self.file)
            .assert()
            .success()
            .get_output()
            .stdout
            .clone();
        String::from_utf8(bytes).unwrap()
    }

    fn expected_hashes(&self) -> Vec<String> {
        let relative = self.file.strip_prefix(self.directory.path()).unwrap();
        git(
            self.directory.path(),
            &[
                "blame",
                "--line-porcelain",
                "--",
                relative.to_str().unwrap(),
            ],
        )
        .lines()
        .filter_map(|line| {
            let parts: Vec<_> = line.split_whitespace().collect();
            (parts.len() >= 3
                && parts[0].len() == 40
                && parts[0].bytes().all(|b| b.is_ascii_hexdigit()))
            .then(|| parts[0][..8].to_owned())
        })
        .collect()
    }

    fn check_hashes(&self) {
        let output = self.output(&[]);
        let actual: Vec<_> = output
            .lines()
            .map(|line| line.split_whitespace().next().unwrap().to_owned())
            .collect();
        assert_eq!(actual, self.expected_hashes(), "{output}");
    }
}

#[test]
fn committed_lines_match_git_and_custom_fields_use_author_metadata() {
    let repo = Repo::new();
    repo.check_hashes();
    let output = repo.output(&["--blame-format=%an|%as|%s"]);
    let lines: Vec<_> = output.lines().collect();
    assert!(lines[0].contains("Ada|2020-02-03|initial"), "{output}");
    assert!(lines[1].contains("Bea|2020-02-03|edit"), "{output}");
    assert!(lines[2].contains("Ada|2020-02-03|initial"), "{output}");
    let output = repo.output(&["--blame-format=%cn|%cs|%%"]);
    assert!(output.contains("Committer|2021-03-04|%"), "{output}");
}

#[test]
fn insertions_deletions_and_staged_changes_keep_unchanged_attribution() {
    let repo = Repo::new();
    for content in [
        "new first\nalpha\nBETA\ncharlie\n",
        "alpha\ncharlie\n",
        "alpha\nchanged\ncharlie\nnew last\n",
        "",
    ] {
        fs::write(&repo.file, content).unwrap();
        if !content.is_empty() {
            repo.check_hashes();
        } else {
            assert_eq!(repo.output(&[]), "");
        }
    }
    fs::write(&repo.file, "alpha\nstaged\ncharlie\n").unwrap();
    git(repo.directory.path(), &["add", "source.rs"]);
    repo.check_hashes();
}

#[test]
fn renamed_files_keep_their_original_authors() {
    let mut repo = Repo::new();
    git(
        repo.directory.path(),
        &["mv", "source.rs", "a renamed file.rs"],
    );
    repo.file = repo.directory.path().join("a renamed file.rs");
    repo.commit("Ren", "rename");
    repo.check_hashes();
}

#[test]
fn merge_history_matches_git_attribution() {
    let repo = Repo::new();
    git(repo.directory.path(), &["checkout", "-b", "topic"]);
    fs::write(&repo.file, "topic\nBETA\ncharlie\n").unwrap();
    repo.commit("Topic", "topic");
    git(repo.directory.path(), &["checkout", "main"]);
    fs::write(&repo.file, "alpha\nBETA\nmain\n").unwrap();
    repo.commit("Main", "main");
    git(
        repo.directory.path(),
        &["merge", "--no-ff", "-m", "merge", "topic"],
    );
    repo.check_hashes();
}

#[test]
fn linked_worktrees_use_their_own_head_and_edits() {
    let original = Repo::new();
    let worktree = tempdir().unwrap();
    git(
        original.directory.path(),
        &[
            "worktree",
            "add",
            "--detach",
            worktree.path().to_str().unwrap(),
        ],
    );
    let repo = Repo {
        file: worktree.path().join("source.rs"),
        directory: worktree,
    };
    fs::write(&repo.file, "alpha\nlocal edit\ncharlie\n").unwrap();
    repo.check_hashes();
}

#[test]
fn untracked_and_unborn_files_are_marked_uncommitted() {
    let repo = Repo::empty();
    fs::write(&repo.file, "unborn\n").unwrap();
    assert!(repo.output(&[]).contains("00000000"));
    repo.commit("Ada", "first");
    let new_file = repo.directory.path().join("untracked.rs");
    fs::write(&new_file, "untracked\n").unwrap();
    let output = bat()
        .args([
            "--paging=never",
            "--color=never",
            "--style=plain",
            "--blame",
        ])
        .arg(new_file)
        .assert()
        .success()
        .get_output()
        .stdout
        .clone();
    assert!(String::from_utf8(output)
        .unwrap()
        .contains("Not Committed Yet"));
}

#[test]
fn non_repository_and_named_stdin_inputs_do_not_gain_false_attribution() {
    let dir = tempdir().unwrap();
    let file = dir.path().join("source.rs");
    fs::write(&file, "plain\n").unwrap();
    bat()
        .args([
            "--paging=never",
            "--color=never",
            "--style=plain",
            "--blame",
        ])
        .arg(file)
        .assert()
        .success()
        .stdout("plain\n");
    let repo = Repo::new();
    bat()
        .args([
            "--paging=never",
            "--color=never",
            "--style=plain",
            "--blame",
            "--file-name",
        ])
        .arg(&repo.file)
        .write_stdin("different\n")
        .assert()
        .success()
        .stdout("different\n");
}

#[test]
fn crlf_worktree_conversion_does_not_mark_every_line_as_modified() {
    let repo = Repo::new();
    git(repo.directory.path(), &["config", "core.autocrlf", "true"]);
    fs::write(&repo.file, "alpha\r\nBETA\r\ncharlie\r\n").unwrap();
    repo.check_hashes();
}

#[test]
fn line_ranges_squeezing_and_nonblank_numbering_keep_physical_attribution() {
    let repo = Repo::new();
    fs::write(&repo.file, "alpha\n\n\nBETA\ncharlie\n").unwrap();
    repo.commit("Extra", "spacing");
    let hashes = repo.expected_hashes();
    let output = repo.output(&["--number-nonblank", "--squeeze-blank", "--line-range=2:"]);
    assert!(
        output
            .lines()
            .any(|line| line.contains(&hashes[3]) && line.ends_with("BETA")),
        "{output}"
    );
    assert!(
        output
            .lines()
            .any(|line| line.contains(&hashes[4]) && line.ends_with("charlie")),
        "{output}"
    );
}

#[test]
fn metadata_is_sanitized_and_wide_authors_fit_the_sidebar() {
    let repo = Repo::empty();
    fs::write(&repo.file, "one\n").unwrap();
    repo.commit("Alex界界界界界界界界界界界界界界界界\x1b[31m", "metadata");
    let output = repo.output(&["--blame-format=%an", "--terminal-width=48"]);
    assert!(!output.contains('\x1b'), "{output:?}");
    assert!(output.contains('…'), "{output}");
    for line in output.lines() {
        assert!(unicode_width::UnicodeWidthStr::width(line) <= 48);
    }
    let escaped = repo.output(&["--blame-format=%s\x1b[2J"]);
    assert!(!escaped.contains('\x1b'), "{escaped:?}");
}

#[test]
fn wrapping_does_not_repeat_attribution_on_continuations() {
    let repo = Repo::new();
    fs::write(
        &repo.file,
        "a long source line that wraps into several screen rows with annotations\n",
    )
    .unwrap();
    repo.commit("Wrap", "wrap");
    let output = repo.output(&["--terminal-width=28", "--wrap=character"]);
    let hash = git(repo.directory.path(), &["rev-parse", "HEAD"]);
    assert!(output.lines().count() > 1);
    assert_eq!(output.matches(&hash[..8]).count(), 1, "{output}");
}

#[test]
fn invalid_formats_fail_and_full_style_remains_opt_in() {
    let repo = Repo::new();
    bat()
        .args(["--paging=never", "--blame", "--blame-format=%q"])
        .arg(&repo.file)
        .assert()
        .failure();
    let output = bat()
        .args([
            "--paging=never",
            "--color=never",
            "--decorations=always",
            "--style=full",
        ])
        .arg(&repo.file)
        .assert()
        .success()
        .get_output()
        .stdout
        .clone();
    let text = String::from_utf8(output).unwrap();
    for hash in repo.expected_hashes() {
        assert!(!text.contains(&hash), "{text}");
    }
    let output = repo.output(&["--decorations=never"]);
    assert_eq!(output, "alpha\nBETA\ncharlie\n");
}

#[test]
fn library_printer_exposes_blame_without_changing_the_input_text() {
    let repo = Repo::new();
    let mut text = String::new();
    bat::PrettyPrinter::new()
        .input_file(&repo.file)
        .colored_output(false)
        .term_width(120)
        .git_blame(true)
        .blame_format("%an")
        .print_with_writer(Some(&mut text))
        .unwrap();
    assert!(text.contains("Ada") && text.contains("Bea"), "{text}");
    assert!(text.lines().next().unwrap().ends_with("alpha"));
}

#[test]
fn unbuffered_mode_and_repeated_options_preserve_source_lines() {
    let repo = Repo::new();
    assert_eq!(repo.output(&["--unbuffered"]), "alpha\nBETA\ncharlie\n");
    let output = repo.output(&["--blame", "--blame-format=%h", "--blame-format=%an"]);
    assert!(output.contains("Ada") && output.contains("Bea"), "{output}");
}

#![cfg(feature = "git")]

use gix::diff::blob::pipeline::{Mode, WorktreeRoots};
use gix::diff::blob::{Algorithm, HunkIter, ResourceKind};
use gix::object::tree::EntryKind;
use path_abs::PathInfo;
use std::collections::HashMap;
use std::path::Path;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum LineChange {
    Added,
    RemovedAbove,
    RemovedBelow,
    Modified,
}

pub type LineChanges = HashMap<u32, LineChange>;

fn collect_changes_from_hunks(hunks: HunkIter) -> Option<LineChanges> {
    let mut changes: LineChanges = HashMap::new();
    for hunk in hunks {
        if hunk.before.is_empty() && !hunk.after.is_empty() {
            for line in hunk.after {
                changes.insert(line + 1, LineChange::Added);
            }
        } else if hunk.after.is_empty() && !hunk.before.is_empty() {
            if hunk.after.start == 0 {
                changes.insert(1, LineChange::RemovedAbove);
            } else {
                changes.insert(hunk.after.start, LineChange::RemovedBelow);
            }
        } else {
            for line in hunk.after {
                changes.insert(line + 1, LineChange::Modified);
            }
        }
    }

    Some(changes)
}

pub fn get_git_diff(filename: &Path) -> Option<LineChanges> {
    let filepath_absolute = filename.canonicalize().ok()?;
    let repository = gix::discover(filepath_absolute.parent().ok()?).ok()?;
    let repo_path_absolute = repository.workdir()?.canonicalize().ok()?;
    let filepath_relative_to_repo = gix::path::to_unix_separators_on_windows(gix::path::into_bstr(
        filepath_absolute.strip_prefix(&repo_path_absolute).ok()?,
    ));
    let index = repository.index_or_load_from_head_or_empty().ok()?;
    let index_entry = index.entry_by_path(filepath_relative_to_repo.as_ref())?;
    let mut cache = repository
        .diff_resource_cache(
            Mode::ToGit,
            WorktreeRoots {
                old_root: None,
                new_root: repository.workdir().map(Path::to_path_buf),
            },
        )
        .ok()?;
    cache
        .set_resource(
            index_entry.id,
            index_entry.mode.to_tree_entry_mode()?.kind(),
            filepath_relative_to_repo.as_ref(),
            ResourceKind::OldOrSource,
            &repository,
        )
        .ok()?;
    cache
        .set_resource(
            repository.object_hash().null(),
            EntryKind::Blob,
            filepath_relative_to_repo.as_ref(),
            ResourceKind::NewOrDestination,
            &repository,
        )
        .ok()?;
    let diff = gix::diff::blob::diff_with_slider_heuristics(
        Algorithm::Histogram,
        &cache.prepare_diff().ok()?.interned_input(),
    );

    collect_changes_from_hunks(diff.hunks())
}

#[cfg(test)]
mod tests {
    use super::{get_git_diff, LineChange};
    use std::path::{Path, PathBuf};
    use std::process::Command;

    fn git(repo: &Path, args: &[&str]) {
        let output = Command::new("git")
            .args(args)
            .current_dir(repo)
            .output()
            .expect("git command can run");
        assert!(
            output.status.success(),
            "git {args:?} failed\nstdout: {}\nstderr: {}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
    }

    fn setup_repo() -> tempfile::TempDir {
        let dir = tempfile::tempdir().expect("can create temporary directory");
        let repo = dir.path();
        git(repo, &["init"]);
        git(repo, &["config", "user.email", "test@test.test"]);
        git(repo, &["config", "user.name", "Test"]);
        dir
    }

    fn create_and_track_file(repo: &tempfile::TempDir, filename: &str) -> PathBuf {
        let filepath = repo.path().join(filename);
        std::fs::write(&filepath, "file\n").expect("can write file");
        git(repo.path(), &["add", filename]);
        git(repo.path(), &["commit", "-m", "initial"]);

        filepath.to_owned()
    }

    #[test]
    fn not_a_git_repository() {
        let dir = tempfile::tempdir().expect("can create temporary directory");
        let file = dir.path().join("file.txt");
        std::fs::write(&file, "line 1\n").expect("can write file");

        assert_eq!(get_git_diff(&file), None);
    }

    #[test]
    fn diff_is_calculated_against_index() {
        let repo = setup_repo();
        let file = create_and_track_file(&repo, "file.txt");

        std::fs::write(&file, "line 1\nline 2 modified\n").expect("can write file");
        // Add modified file to index -> should find 0 changes but not none
        git(repo.path(), &["add", "file.txt"]);

        assert_eq!(get_git_diff(&file).expect("empty map").len(), 0);
    }

    #[test]
    fn diff_is_calculated_against_index_2() {
        let repo = setup_repo();
        let file = create_and_track_file(&repo, "file.txt");

        std::fs::write(&file, "line 1\nline 2 modified\n").expect("can write file");
        git(repo.path(), &["add", "file.txt"]);
        // modify the second line again which should show a single change
        std::fs::write(&file, "line 1\nline 2 modified again\n").expect("can write file");

        assert_eq!(get_git_diff(&file).expect("one change").len(), 1);
    }

    #[test]
    fn diff_is_calculated_correctly() {
        let repo = setup_repo();
        create_and_track_file(&repo, "committed.txt");
        let filename = "committed2.txt";
        let file = create_and_track_file(&repo, filename);

        std::fs::write(&file, "file\nline 2 added\n").expect("can write file");
        let mut changes = get_git_diff(&file).expect("multiple changes");
        assert_eq!(changes.get(&2), Some(&LineChange::Added));
        git(repo.path(), &["add", filename]);

        std::fs::write(&file, "file\nline 2 modified\n").expect("can write file");
        changes = get_git_diff(&file).expect("multiple changes");
        assert_eq!(changes.get(&2), Some(&LineChange::Modified));
        git(repo.path(), &["add", filename]);

        std::fs::write(&file, "line 2 modified\nline 3 added").expect("can write file");
        changes = get_git_diff(&file).expect("multiple changes");
        assert_eq!(changes.get(&1), Some(&LineChange::RemovedAbove));
        assert_eq!(changes.get(&2), Some(&LineChange::Added));
    }
}

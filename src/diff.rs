#![cfg(feature = "git")]

use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};

use git2::{DiffOptions, IntoCString, Repository};

#[derive(Copy, Clone, Debug)]
pub enum LineChange {
    Added,
    RemovedAbove,
    RemovedBelow,
    Modified,
}

pub type LineChanges = HashMap<u32, LineChange>;

/// Walk up from a path to find the directory containing `.git`.
/// This correctly handles git worktrees where the file sits under
/// the worktree directory, not the main repository workdir.
fn find_repo_root(path: &Path) -> Option<PathBuf> {
    let mut current = path.parent()?.to_path_buf();
    loop {
        if current.join(".git").exists() {
            return fs::canonicalize(&current).ok();
        }
        current = current.parent()?.to_path_buf();
    }
}

pub fn get_git_diff(filename: &Path) -> Option<LineChanges> {
    let repo = Repository::discover(filename).ok()?;

    let filepath_absolute = fs::canonicalize(filename).ok()?;

    // Determine the repo root by walking up from the file to find .git.
    // Uses filesystem lookup instead of repo.workdir() to correctly handle
    // git worktrees, where the file lives under the worktree directory
    // rather than the main repository workdir.
    let repo_root = find_repo_root(&filepath_absolute)?;
    let filepath_relative_to_repo = filepath_absolute.strip_prefix(&repo_root).ok()?;

    let mut diff_options = DiffOptions::new();
    let pathspec = filepath_relative_to_repo.into_c_string().ok()?;
    diff_options.pathspec(pathspec);
    diff_options.context_lines(0);

    let diff = repo
        .diff_index_to_workdir(None, Some(&mut diff_options))
        .ok()?;

    let mut line_changes: LineChanges = HashMap::new();

    let mark_section =
        |line_changes: &mut LineChanges, start: u32, end: i32, change: LineChange| {
            for line in start..=end as u32 {
                line_changes.insert(line, change);
            }
        };

    let _ = diff.foreach(
        &mut |_, _| true,
        None,
        Some(&mut |delta, hunk| {
            let path = delta.new_file().path().unwrap_or_else(|| Path::new(""));

            if filepath_relative_to_repo != path {
                return false;
            }

            let old_lines = hunk.old_lines();
            let new_start = hunk.new_start();
            let new_lines = hunk.new_lines();
            let new_end = (new_start + new_lines) as i32 - 1;

            if old_lines == 0 && new_lines > 0 {
                mark_section(&mut line_changes, new_start, new_end, LineChange::Added);
            } else if new_lines == 0 && old_lines > 0 {
                if new_start == 0 {
                    mark_section(&mut line_changes, 1, 1, LineChange::RemovedAbove);
                } else {
                    mark_section(
                        &mut line_changes,
                        new_start,
                        new_start as i32,
                        LineChange::RemovedBelow,
                    );
                }
            } else {
                mark_section(&mut line_changes, new_start, new_end, LineChange::Modified);
            }

            true
        }),
        None,
    );

    Some(line_changes)
}

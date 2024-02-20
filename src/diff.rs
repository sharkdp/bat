#![cfg(feature = "git")]

use git2::{Blame, BlameHunk, BlameOptions, Commit, DiffOptions, IntoCString, Repository};
use std::collections::HashMap;
use std::fs;
use std::path::Path;

#[derive(Copy, Clone, Debug)]
pub enum LineChange {
    Added,
    RemovedAbove,
    RemovedBelow,
    Modified,
}

pub type LineChanges = HashMap<u32, LineChange>;
pub type LineBlames = HashMap<u32, String>;

pub fn get_git_diff(filename: &Path) -> Option<LineChanges> {
    let repo = Repository::discover(filename).ok()?;

    let repo_path_absolute = fs::canonicalize(repo.workdir()?).ok()?;

    let filepath_absolute = fs::canonicalize(filename).ok()?;
    let filepath_relative_to_repo = filepath_absolute.strip_prefix(&repo_path_absolute).ok()?;

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

pub fn get_blame_line(
    blame: &Blame,
    filename: &Path,
    line: u32,
    blame_format: &str,
) -> Option<String> {
    let repo = Repository::discover(filename).ok()?;
    let default_return = "Unknown".to_string();
    let diff = get_git_diff(filename).unwrap();
    if diff.contains_key(&line) {
        return Some(format!("{} <{}>", default_return, default_return));
    }
    if let Some(line_blame) = blame.get_line(line as usize) {
        let signature = line_blame.final_signature();
        let name = signature.name().unwrap_or(default_return.as_str());
        let email = signature.email().unwrap_or(default_return.as_str());
        if blame_format.is_empty() {
            return Some(format!("{} <{}>", name, email));
        }
        let commit_id = line_blame.final_commit_id();
        let commit = repo.find_commit(commit_id).ok()?;

        return Some(format_blame(&line_blame, &commit, blame_format));
    }
    Some(default_return)
}

pub fn format_blame(blame_hunk: &BlameHunk, commit: &Commit, blame_format: &str) -> String {
    let mut result = String::from(blame_format);
    let abbreviated_id_buf = commit.as_object().short_id();
    let abbreviated_id = abbreviated_id_buf
        .as_ref()
        .ok()
        .map(|id| id.as_str())
        .unwrap_or(Some(""));
    let signature = blame_hunk.final_signature();
    result = result.replace("%an", signature.name().unwrap_or("Unknown"));
    result = result.replace("%ae", signature.email().unwrap_or("Unknown"));
    result = result.replace("%H", commit.id().to_string().as_str());
    result = result.replace("%h", abbreviated_id.unwrap());
    result = result.replace("%s", commit.summary().unwrap_or("Unknown"));
    result = result.replace("%cn", commit.author().name().unwrap_or("Unknown"));
    result = result.replace("%ce", commit.author().email().unwrap_or("Unknown"));
    result = result.replace("%b", commit.message().unwrap_or("Unknown"));
    result = result.replace("%N", commit.parents().len().to_string().as_str());

    result
}

pub fn get_blame_file(filename: &Path, blame_format: &str) -> Option<LineBlames> {
    let lines_in_file = fs::read_to_string(filename).ok()?.lines().count();
    let mut result = LineBlames::new();
    let mut blame_options = BlameOptions::new();
    let repo = Repository::discover(filename).ok()?;
    let repo_path_absolute = fs::canonicalize(repo.workdir()?).ok()?;
    let filepath_absolute = fs::canonicalize(filename).ok()?;
    let filepath_relative_to_repo = filepath_absolute.strip_prefix(&repo_path_absolute).ok()?;

    let blame = repo
        .blame_file(filepath_relative_to_repo, Some(&mut blame_options))
        .ok()?;
    for i in 0..lines_in_file {
        if let Some(str_result) = get_blame_line(&blame, filename, i as u32, blame_format) {
            result.insert(i as u32, str_result);
        }
    }
    Some(result)
}

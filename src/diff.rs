#![cfg(feature = "git")]

use gix::diff::blob::pipeline::{Mode, WorktreeRoots};
use gix::diff::blob::{Algorithm, HunkIter, ResourceKind};
use gix::index::hash::Kind;
use gix::object::tree::EntryKind;
use gix::{self, ObjectId};
use path_abs::PathInfo;
use std::collections::HashMap;
use std::path::Path;

#[derive(Copy, Clone, Debug)]
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
    let repository = gix::discover(filepath_absolute.parent().ok()?).unwrap();
    let repo_path_absolute = repository.workdir()?.canonicalize().ok()?;
    let filepath_relative_to_repo = filepath_absolute.strip_prefix(&repo_path_absolute).ok()?;
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
            repository
                .head_tree()
                .ok()?
                .lookup_entry_by_path(filepath_relative_to_repo.to_str()?)
                .ok()??
                .object_id(),
            EntryKind::Blob,
            filepath_relative_to_repo.to_str()?.into(),
            ResourceKind::OldOrSource,
            &repository,
        )
        .ok()?;
    cache
        .set_resource(
            ObjectId::null(Kind::Sha1),
            EntryKind::Blob,
            filepath_relative_to_repo.to_str()?.into(),
            ResourceKind::NewOrDestination,
            &repository,
        )
        .ok()?;
    let diff = gix::diff::blob::diff_with_slider_heuristics(
        Algorithm::Myers,
        &cache.prepare_diff().ok()?.interned_input(),
    );

    collect_changes_from_hunks(diff.hunks())
}

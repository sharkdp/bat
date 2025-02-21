#![cfg(feature = "git")]

use gix::diff::blob::pipeline::{Mode, WorktreeRoots};
use gix::diff::blob::{Algorithm, ResourceKind, Sink};
use gix::index::hash::Kind;
use gix::object::tree::EntryKind;
use gix::{self, ObjectId};
use path_abs::PathInfo;
use std::collections::HashMap;
use std::ops::Range;
use std::path::Path;

#[derive(Copy, Clone, Debug)]
pub enum LineChange {
    Added,
    RemovedAbove,
    RemovedBelow,
    Modified,
}

pub type LineChanges = HashMap<u32, LineChange>;

struct DiffStepper(LineChanges);

impl Sink for DiffStepper {
    type Out = LineChanges;

    fn process_change(&mut self, before: Range<u32>, after: Range<u32>) {
        if before.is_empty() && !after.is_empty() {
            for line in after {
                self.0.insert(line + 1, LineChange::Added);
            }
        } else if after.is_empty() && !before.is_empty() {
            if after.start == 0 {
                self.0.insert(1, LineChange::RemovedAbove);
            } else {
                self.0.insert(after.start, LineChange::RemovedBelow);
            }
        } else {
            for line in after {
                self.0.insert(line + 1, LineChange::Modified);
            }
        };
    }

    fn finish(self) -> Self::Out {
        self.0
    }
}

pub fn get_git_diff(filename: &Path) -> Option<LineChanges> {
    let filepath_absolute = filename.canonicalize().ok()?;
    let repository = gix::discover(filepath_absolute.parent().ok()?).unwrap();
    let repo_path_absolute = repository.work_dir()?.canonicalize().ok()?;
    let filepath_relative_to_repo = filepath_absolute.strip_prefix(&repo_path_absolute).ok()?;
    let mut cache = repository
        .diff_resource_cache(
            Mode::ToGit,
            WorktreeRoots {
                old_root: None,
                new_root: repository.work_dir().map(Path::to_path_buf),
            },
        )
        .ok()?;
    cache
        .set_resource(
            repository
                .head_tree()
                .ok()?
                .lookup_entry_by_path(filepath_relative_to_repo.to_str()?).ok()??
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
    return Some(gix::diff::blob::diff(
        Algorithm::Myers,
        &cache.prepare_diff().ok()?.interned_input(),
        DiffStepper(HashMap::new()),
    ));
}

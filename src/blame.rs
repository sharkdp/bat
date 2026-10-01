//! Git attribution for the current working tree, including uncommitted lines.

use std::collections::HashMap;
use std::path::Path;

use gix::diff::blob::pipeline::{Mode, WorktreeRoots};
use gix::diff::blob::{Algorithm, ResourceKind};
use gix::object::tree::EntryKind;

use crate::error::{Error, Result};

pub(crate) const DEFAULT_FORMAT: &str = "%h %an";

#[derive(Clone, Debug)]
enum Part {
    Literal(String),
    Field(&'static str),
}

#[derive(Clone, Debug)]
pub(crate) struct BlameFormat(Vec<Part>);

impl BlameFormat {
    pub(crate) fn parse(value: &str) -> Result<Self> {
        let mut parts = Vec::new();
        let mut rest = value;
        while let Some(index) = rest.find('%') {
            if index > 0 {
                parts.push(Part::Literal(rest[..index].to_owned()));
            }
            rest = &rest[index + 1..];
            if let Some(next) = rest.strip_prefix('%') {
                parts.push(Part::Literal("%".to_owned()));
                rest = next;
                continue;
            }
            let field = ["an", "ae", "as", "at", "cn", "ce", "cs", "ct", "h", "H", "s"]
                .into_iter().find(|field| rest.starts_with(field))
                .ok_or_else(|| Error::Msg("Unknown --blame-format field; supported fields: %h %H %an %ae %as %at %cn %ce %cs %ct %s %%".into()))?;
            parts.push(Part::Field(field));
            rest = &rest[field.len()..];
        }
        if !rest.is_empty() {
            parts.push(Part::Literal(rest.to_owned()));
        }
        Ok(Self(parts))
    }

    fn render(&self, info: &CommitInfo) -> String {
        let mut output = String::new();
        for part in &self.0 {
            output.push_str(match part {
                Part::Literal(text) => text,
                Part::Field("h") => &info.hash[..info.hash.len().min(8)],
                Part::Field("H") => &info.hash,
                Part::Field("an") => &info.author,
                Part::Field("ae") => &info.author_email,
                Part::Field("as") => &info.author_date,
                Part::Field("at") => &info.author_time,
                Part::Field("cn") => &info.committer,
                Part::Field("ce") => &info.committer_email,
                Part::Field("cs") => &info.committer_date,
                Part::Field("ct") => &info.committer_time,
                Part::Field("s") => &info.summary,
                Part::Field(_) => unreachable!("only validated fields are stored"),
            });
        }
        crate::sanitize_for_terminal(&output)
    }
}

#[derive(Default)]
struct CommitInfo {
    hash: String,
    author: String,
    author_email: String,
    author_date: String,
    author_time: String,
    committer: String,
    committer_email: String,
    committer_date: String,
    committer_time: String,
    summary: String,
}

impl CommitInfo {
    fn uncommitted(hash: String) -> Self {
        Self {
            hash,
            author: "Not Committed Yet".into(),
            committer: "Not Committed Yet".into(),
            summary: "Uncommitted changes".into(),
            ..Self::default()
        }
    }

    fn load(repository: &gix::Repository, id: gix::ObjectId) -> Result<Self> {
        let commit = repository.find_commit(id).map_err(git_error)?;
        let author = commit.author().map_err(git_error)?;
        let committer = commit.committer().map_err(git_error)?;
        let author_time = author.time().map_err(git_error)?;
        let committer_time = committer.time().map_err(git_error)?;
        let message = commit.message_raw().map_err(git_error)?;
        Ok(Self {
            hash: id.to_string(),
            author: String::from_utf8_lossy(author.name.as_ref()).into_owned(),
            author_email: String::from_utf8_lossy(author.email.as_ref()).into_owned(),
            author_date: author_time.format_or_unix(gix::date::time::format::SHORT),
            author_time: author_time.seconds.to_string(),
            committer: String::from_utf8_lossy(committer.name.as_ref()).into_owned(),
            committer_email: String::from_utf8_lossy(committer.email.as_ref()).into_owned(),
            committer_date: committer_time.format_or_unix(gix::date::time::format::SHORT),
            committer_time: committer_time.seconds.to_string(),
            summary: String::from_utf8_lossy(message.as_ref())
                .lines()
                .next()
                .unwrap_or_default()
                .to_owned(),
        })
    }
}

pub(crate) struct BlameLines {
    pub labels: Vec<String>,
    // Index zero is the uncommitted label. Store each commit's label only once.
    pub lines: Vec<usize>,
}

pub(crate) fn get_git_blame(filename: &Path, format: &BlameFormat) -> Result<Option<BlameLines>> {
    let absolute = filename.canonicalize()?;
    if !absolute.metadata()?.is_file() {
        return Ok(None);
    }
    let Some(parent) = absolute.parent() else {
        return Ok(None);
    };
    let Ok(repository) = gix::discover(parent) else {
        return Ok(None);
    };
    let Some(workdir) = repository.workdir() else {
        return Ok(None);
    };
    let workdir = workdir.canonicalize()?;
    let relative = absolute.strip_prefix(&workdir).map_err(git_error)?;
    let git_path = gix::path::to_unix_separators_on_windows(gix::path::into_bstr(relative));
    let working = std::fs::read(&absolute)?;
    // Blame indexes Git's byte lines, so a decoded UTF-16 input cannot use this map.
    if std::str::from_utf8(&working).is_err() || working.contains(&0) {
        return Ok(None);
    }
    let line_count = working.split_inclusive(|&b| b == b'\n').count();
    let mut labels = vec![format.render(&CommitInfo::uncommitted(
        repository.object_hash().null().to_string(),
    ))];
    let mut output = vec![0; line_count];
    if repository.head().map_err(git_error)?.is_unborn() {
        return Ok(Some(BlameLines {
            labels,
            lines: output,
        }));
    }
    let head = repository.head_commit().map_err(git_error)?;
    let tree = head.tree().map_err(git_error)?;
    let Some(entry) = tree.lookup_entry_by_path(relative).map_err(git_error)? else {
        return Ok(Some(BlameLines {
            labels,
            lines: output,
        }));
    };
    let outcome = repository
        .blame_file(
            git_path.as_ref(),
            head.id,
            gix::repository::blame_file::Options {
                rewrites: Some(Default::default()),
                ..Default::default()
            },
        )
        .map_err(git_error)?;
    let mut old_lines = vec![0; outcome.blob.split_inclusive(|&b| b == b'\n').count()];
    let mut commits = HashMap::new();
    for entry in &outcome.entries {
        let label = match commits.get(&entry.commit_id) {
            Some(&index) => index,
            None => {
                let index = labels.len();
                labels.push(format.render(&CommitInfo::load(&repository, entry.commit_id)?));
                commits.insert(entry.commit_id, index);
                index
            }
        };
        let range = entry.start_in_blamed_file as usize
            ..(entry.start_in_blamed_file as usize + entry.len.get() as usize);
        old_lines
            .get_mut(range)
            .ok_or("Invalid Git blame line range")?
            .fill(label);
    }
    // Use Git's clean-filter pipeline, including core.autocrlf and attributes,
    // so worktree edits shift attribution without mislabelling unchanged lines.
    let mut cache = repository
        .diff_resource_cache(
            Mode::ToGit,
            WorktreeRoots {
                old_root: None,
                new_root: Some(workdir),
            },
        )
        .map_err(git_error)?;
    cache
        .set_resource(
            entry.id().detach(),
            EntryKind::Blob,
            git_path.as_ref(),
            ResourceKind::OldOrSource,
            &repository,
        )
        .map_err(git_error)?;
    cache
        .set_resource(
            repository.object_hash().null(),
            EntryKind::Blob,
            git_path.as_ref(),
            ResourceKind::NewOrDestination,
            &repository,
        )
        .map_err(git_error)?;
    let prepared = cache.prepare_diff().map_err(git_error)?;
    let input = prepared.interned_input();
    if input.after.len() != output.len() || input.before.len() != old_lines.len() {
        // A custom clean filter may change the line count. Its positions do not
        // describe the file bat prints, so omit the annotation in that case.
        return Ok(None);
    }
    let diff = gix::diff::blob::diff_with_slider_heuristics(Algorithm::Histogram, &input);
    let (mut before, mut after) = (0usize, 0usize);
    for hunk in diff.hunks() {
        let unchanged = hunk.before.start as usize - before;
        output[after..after + unchanged].copy_from_slice(&old_lines[before..before + unchanged]);
        before = hunk.before.end as usize;
        after = hunk.after.end as usize;
    }
    output[after..].copy_from_slice(&old_lines[before..]);
    Ok(Some(BlameLines {
        labels,
        lines: output,
    }))
}

fn git_error(error: impl std::fmt::Display) -> Error {
    Error::Msg(format!("Could not read Git blame: {error}"))
}

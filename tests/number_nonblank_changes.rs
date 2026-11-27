//! `number_nonblank` combined with Git change markers. The command line only
//! enables the numbers column with `-b`, so this goes through the library.
#![cfg(feature = "git")]

use std::fs;

use bat::assets::HighlightingAssets;
use bat::config::Config;
use bat::controller::Controller;
use bat::input::Input;
use bat::output::OutputHandle;
use bat::style::{StyleComponent, StyleComponents};
use gix::actor::SignatureRef;
use gix::bstr::{BString, ByteSlice};
use gix::date::time::Format;
use gix::date::Time;
use gix::objs::tree;
use tempfile::TempDir;

/// Commits `committed` as `file.txt` in a new repository, then writes
/// `modified` to the working tree.
fn repo_with_modified_file(committed: &str, modified: &str) -> TempDir {
    let temp_dir = TempDir::new().expect("temporary directory");
    let repo = gix::init(&temp_dir).expect("init repository");

    let blob_id = repo.write_blob(committed.as_bytes()).expect("write blob");
    let mut tree = gix::objs::Tree::empty();
    tree.entries.push(tree::Entry {
        mode: tree::EntryMode::from(tree::EntryKind::Blob),
        oid: blob_id.detach(),
        filename: BString::from("file.txt"),
    });
    let tree_id = repo.write_object(tree).expect("write tree");

    let time = Time::now_local_or_utc().format_or_unix(Format::Raw);
    let author = SignatureRef {
        name: "test".as_bytes().as_bstr(),
        email: "test@test.test".as_bytes().as_bstr(),
        time: &time,
    };
    repo.commit_as(
        author,
        author,
        "HEAD",
        "initial commit",
        tree_id,
        gix::commit::NO_PARENT_IDS,
    )
    .expect("commit");

    fs::write(temp_dir.path().join("file.txt"), modified).expect("write modified file");
    temp_dir
}

#[test]
fn number_nonblank_keeps_git_changes_on_file_lines() {
    // Line 3 ("beta") is modified. With number-nonblank it is displayed as
    // line 2, but the change marker must stay on it rather than move to the
    // line displayed as 3 ("gamma").
    let repo = repo_with_modified_file("alpha\n\nbeta\ngamma\n", "alpha\n\nBETA\ngamma\n");
    let path = repo.path().join("file.txt");

    let config = Config {
        term_width: 80,
        number_nonblank: true,
        style_components: StyleComponents::new(&[
            StyleComponent::LineNumbers,
            StyleComponent::Changes,
        ]),
        ..Default::default()
    };
    let assets = HighlightingAssets::from_binary();
    let mut output = String::new();
    Controller::new(&config, &assets)
        .run(
            vec![Input::ordinary_file(&path)],
            Some(&mut OutputHandle::FmtWrite(&mut output)),
        )
        .expect("bat runs");

    assert_eq!(output, "   1   alpha\n       \n   2 ~ BETA\n   3   gamma\n");
}

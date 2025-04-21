use std::env;
use std::fs::{self, File};
use std::io::Read;
use std::path::PathBuf;
use std::process::Command;

use gix::actor::SignatureRef;
use gix::bstr::BString;
use gix::date::Time;
use gix::bstr::ByteSlice;
use gix::objs::tree;
use tempfile::TempDir;

pub struct BatTester {
    /// Temporary working directory
    temp_dir: TempDir,

    /// Path to the *bat* executable
    exe: PathBuf,
}

impl BatTester {
    pub fn test_snapshot(&self, name: &str, style: &str) {
        let output = Command::new(&self.exe)
            .current_dir(self.temp_dir.path())
            .args([
                "sample.rs",
                "--no-config",
                "--paging=never",
                "--color=never",
                "--decorations=always",
                "--terminal-width=80",
                &format!("--style={style}"),
            ])
            .output()
            .expect("bat failed");
        // have to do the replace because the filename in the header changes based on the current working directory
        let actual = String::from_utf8_lossy(&output.stdout)
            .as_ref()
            .replace("tests/snapshots/", "");

        let mut expected = String::new();
        let mut file = File::open(format!("tests/snapshots/output/{name}.snapshot.txt"))
            .expect("snapshot file missing");
        file.read_to_string(&mut expected)
            .expect("could not read snapshot file");

        assert_eq!(expected, actual);
    }
}

impl Default for BatTester {
    fn default() -> Self {
        let temp_dir = create_sample_directory().expect("sample directory");

        let root = env::current_exe()
            .expect("tests executable")
            .parent()
            .expect("tests executable directory")
            .parent()
            .expect("bat executable directory")
            .to_path_buf();

        let exe_name = if cfg!(windows) { "bat.exe" } else { "bat" };
        let exe = root.join(exe_name);

        BatTester { temp_dir, exe }
    }
}

fn create_sample_directory() -> Result<TempDir, Box<dyn std::error::Error>> {
    // Create temp directory and initialize repository
    let temp_dir = TempDir::new().expect("Temp directory");
    let repo = gix::init(&temp_dir)?;
    let mut tree = gix::objs::Tree::empty();

    // Create sample.rs from snapshot file
    let blob_id = repo.write_blob_stream(File::open("tests/snapshots/sample.rs")?)?;
    let entry = tree::Entry {
        mode: tree::EntryMode::from(tree::EntryKind::Blob),
        oid: blob_id.object()?.id,
        filename: BString::from("sample.rs"),
    };
    tree.entries.push(entry);
    let tree_id = repo.write_object(tree)?;

    let author = SignatureRef {
        name: "test".as_bytes().as_bstr(),
        email: "test@test.test".as_bytes().as_bstr(),
        time: Time::now_utc(),
    };
    let commit_id = repo.commit_as(
        author,
        author,
        "HEAD",
        "initial commit",
        tree_id,
        gix::commit::NO_PARENT_IDS,
    )?;
    assert_eq!(commit_id, repo.head_id()?);

    fs::copy(
        "tests/snapshots/sample.modified.rs",
        temp_dir.path().join("sample.rs"),
    )
    .expect("successful copy");

    Ok(temp_dir)
}

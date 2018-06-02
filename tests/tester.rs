use std::env;
use std::fs::{self, File};
use std::io::{self, Read};
use std::path::{Path, PathBuf};
use std::process::Command;

extern crate tempdir;
use self::tempdir::TempDir;

extern crate git2;
use self::git2::build::CheckoutBuilder;
use self::git2::Error;
use self::git2::Repository;
use self::git2::Signature;

pub struct BatTester {
    /// Temporary working directory
    temp_dir: TempDir,

    /// Path to the *bat* executable
    exe: PathBuf,
}

impl BatTester {
    pub fn new() -> Self {
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

    pub fn test_snapshot(&self, style: &str) {
        let output = Command::new(&self.exe)
            .current_dir(self.temp_dir.path())
            .args(&["sample.rs", &format!("--style={}", style)])
            .output()
            .expect("bat failed");

        // have to do the replace because the filename in the header changes based on the current working directory
        let actual = String::from_utf8_lossy(&output.stdout)
            .as_ref()
            .replace("tests/snapshots/", "");

        let mut expected = String::new();
        let mut file = File::open(format!("tests/snapshots/output/{}.snapshot.txt", style))
            .expect("snapshot file missing");
        file.read_to_string(&mut expected)
            .expect("could not read snapshot file");

        assert_eq!(expected, actual);
    }
}

fn create_sample_directory() -> Result<TempDir, git2::Error> {
    // Create temp directory and initialize repository
    let temp_dir = TempDir::new("bat-tests").expect("Temp directory");
    let repo = Repository::init(&temp_dir)?;

    // Copy over `sample.rs`
    let sample_path = temp_dir.path().join("sample.rs");
    println!("{:?}", &sample_path);
    fs::copy("tests/snapshots/sample.rs", &sample_path).expect("successful copy");

    // Commit
    let mut index = repo.index()?;
    index.add_path(Path::new("sample.rs"))?;

    let oid = index.write_tree()?;
    let signature = Signature::now("bat test runner", "bat@test.runner")?;
    let tree = repo.find_tree(oid)?;
    repo.commit(
        Some("HEAD"), //  point HEAD to our new commit
        &signature,   // author
        &signature,   // committer
        "initial commit",
        &tree,
        &[],
    );
    let mut opts = CheckoutBuilder::new();
    repo.checkout_head(Some(opts.force()))?;

    fs::copy("tests/snapshots/sample.modified.rs", &sample_path).expect("successful copy");

    Ok(temp_dir)
}

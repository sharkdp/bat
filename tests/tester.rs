use std::env;
use std::fs::{self, File};
use std::io::Read;
use std::path::PathBuf;
use std::process::Command;

pub struct BatTester {
    exe: PathBuf,
}

impl BatTester {
    pub fn new() -> Self {
        modify_sample_file();

        let root = env::current_exe()
            .expect("tests executable")
            .parent()
            .expect("tests executable directory")
            .parent()
            .expect("bat executable directory")
            .to_path_buf();

        let exe_name = if cfg!(windows) { "bat.exe" } else { "bat" };

        BatTester {
            exe: root.join(exe_name),
        }
    }

    pub fn test_snapshot(&self, style: &str) {
        let output = Command::new(&self.exe)
            .args(&["tests/snapshots/sample.rs", &format!("--style={}", style)])
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

impl Drop for BatTester {
    fn drop(&mut self) {
        undo_sample_file_modification();
    }
}

fn modify_sample_file() {
    fs::copy(
        "tests/snapshots/sample.modified.rs",
        "tests/snapshots/sample.rs",
    ).expect("generating modified sample file failed");
}

fn undo_sample_file_modification() {
    let output = Command::new("git")
        .args(&["checkout", "--", "tests/snapshots/sample.rs"])
        .output()
        .expect("git checkout failed");

    if !output.status.success() {
        panic!("undoing modified sample changes failed")
    }
}

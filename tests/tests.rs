mod tester;

use std::process::{Command, Stdio};
use tester::BatTester;

static STYLES: &'static [&'static str] = &[
    "changes",
    "grid",
    "header",
    "numbers",
    "changes,grid",
    "changes,header",
    "changes,numbers",
    "grid,header",
    "grid,numbers",
    "header,numbers",
    "changes,grid,header",
    "changes,grid,numbers",
    "changes,header,numbers",
    "grid,header,numbers",
    "changes,grid,header,numbers",
    "full",
    "plain",
];

#[test]
fn test_snapshots() {
    let status = Command::new("git")
        .arg("rev-parse")
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status();

    if !status.map(|s| s.success()).unwrap_or(false) {
        // Git not available or not a git repository. Skipping snapshot test.
        return;
    }

    let bat_tester = BatTester::new();

    for style in STYLES {
        println!("testing {}", style);
        bat_tester.test_snapshot(&*style);
    }
}

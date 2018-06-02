mod tester;

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
    let bat_tester = BatTester::new();

    for style in STYLES {
        println!("testing {}", style);
        bat_tester.test_snapshot(&*style);
    }
}

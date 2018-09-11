mod tester;

use tester::BatTester;

macro_rules! snapshot_tests {
    ($($test_name: ident: $style: expr => [tabs: $tabs:expr],)*) => {
        $(
            #[test]
            fn $test_name() {
                let bat_tester = BatTester::new();
                bat_tester.test_snapshot(stringify!($test_name), $style, $tabs);
            }
        )*
    };
}

snapshot_tests! {
    changes: "changes" => [tabs: 8],
    grid: "grid" => [tabs: 8],
    header: "header" => [tabs: 8],
    numbers: "numbers" => [tabs: 8],
    changes_grid: "changes,grid" => [tabs: 8],
    changes_header: "changes,header" => [tabs: 8],
    changes_numbers: "changes,numbers" => [tabs: 8],
    grid_header: "grid,header" => [tabs: 8],
    grid_numbers: "grid,numbers" => [tabs: 8],
    header_numbers: "header,numbers" => [tabs: 8],
    changes_grid_header: "changes,grid,header" => [tabs: 8],
    changes_grid_numbers: "changes,grid,numbers" => [tabs: 8],
    changes_header_numbers: "changes,header,numbers" => [tabs: 8],
    grid_header_numbers: "grid,header,numbers" => [tabs: 8],
    changes_grid_header_numbers: "changes,grid,header,numbers" => [tabs: 8],
    full: "full" => [tabs: 8],
    plain: "plain" => [tabs: 8],
    tabs_passthrough: "full" => [tabs: 0],
    tabs_4: "full" => [tabs: 4],
    tabs_8: "full" => [tabs: 8],
}

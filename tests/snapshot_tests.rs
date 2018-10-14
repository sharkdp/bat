mod tester;

use tester::BatTester;

macro_rules! snapshot_tests {
    ($($test_name: ident: $style: expr => [wrap: $wrap:expr, tabs: $tabs:expr],)*) => {
        $(
            #[test]
            fn $test_name() {
                let bat_tester = BatTester::new();
                bat_tester.test_snapshot(stringify!($test_name), $style, $tabs, $wrap);
            }
        )*
    };
}

snapshot_tests! {
    changes:                     "changes"                     => [wrap: false, tabs: 8],
    grid:                        "grid"                        => [wrap: false, tabs: 8],
    header:                      "header"                      => [wrap: false, tabs: 8],
    numbers:                     "numbers"                     => [wrap: false, tabs: 8],
    changes_grid:                "changes,grid"                => [wrap: false, tabs: 8],
    changes_header:              "changes,header"              => [wrap: false, tabs: 8],
    changes_numbers:             "changes,numbers"             => [wrap: false, tabs: 8],
    grid_header:                 "grid,header"                 => [wrap: false, tabs: 8],
    grid_numbers:                "grid,numbers"                => [wrap: false, tabs: 8],
    header_numbers:              "header,numbers"              => [wrap: false, tabs: 8],
    changes_grid_header:         "changes,grid,header"         => [wrap: false, tabs: 8],
    changes_grid_numbers:        "changes,grid,numbers"        => [wrap: false, tabs: 8],
    changes_header_numbers:      "changes,header,numbers"      => [wrap: false, tabs: 8],
    grid_header_numbers:         "grid,header,numbers"         => [wrap: false, tabs: 8],
    changes_grid_header_numbers: "changes,grid,header,numbers" => [wrap: false, tabs: 8],
    full:                        "full"                        => [wrap: false, tabs: 8],
    plain:                       "plain"                       => [wrap: false, tabs: 0],
}

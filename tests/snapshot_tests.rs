mod tester;

use crate::tester::BatTester;

macro_rules! snapshot_tests {
    ($($test_name: ident: $style: expr,)*) => {
        $(
            #[test]
            fn $test_name() {
                let bat_tester = BatTester::new();
                bat_tester.test_snapshot(stringify!($test_name), $style);
            }
        )*
    };
}

snapshot_tests! {
    changes:                     "changes",
    grid:                        "grid",
    header:                      "header",
    numbers:                     "numbers",
    changes_grid:                "changes,grid",
    changes_header:              "changes,header",
    changes_numbers:             "changes,numbers",
    grid_header:                 "grid,header",
    grid_numbers:                "grid,numbers",
    header_numbers:              "header,numbers",
    changes_grid_header:         "changes,grid,header",
    changes_grid_numbers:        "changes,grid,numbers",
    changes_header_numbers:      "changes,header,numbers",
    grid_header_numbers:         "grid,header,numbers",
    changes_grid_header_numbers: "changes,grid,header,numbers",
    full:                        "full",
    plain:                       "plain",
}

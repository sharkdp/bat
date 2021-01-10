mod tester;

use crate::tester::BatTester;

macro_rules! snapshot_tests {
    ($($test_name: ident: $style: expr,)*) => {
        $(
            #[test]
            fn $test_name() {
                let bat_tester = BatTester::default();
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
    rule:                        "rule",
    changes_grid:                "changes,grid",
    changes_header:              "changes,header",
    changes_numbers:             "changes,numbers",
    changes_rule:                "changes,rule",
    grid_header:                 "grid,header",
    grid_numbers:                "grid,numbers",
    grid_rule:                   "grid,rule",
    header_numbers:              "header,numbers",
    header_rule:                 "header,rule",
    changes_grid_header:         "changes,grid,header",
    changes_grid_numbers:        "changes,grid,numbers",
    changes_grid_rule:           "changes,grid,rule",
    changes_header_numbers:      "changes,header,numbers",
    changes_header_rule:         "changes,header,rule",
    grid_header_numbers:         "grid,header,numbers",
    grid_header_rule:            "grid,header,rule",
    header_numbers_rule:         "header,numbers,rule",
    changes_grid_header_numbers: "changes,grid,header,numbers",
    changes_grid_header_rule:    "changes,grid,header,rule",
    changes_grid_header_numbers_rule: "changes,grid,header,numbers,rule",
    full:                        "full",
    plain:                       "plain",
}

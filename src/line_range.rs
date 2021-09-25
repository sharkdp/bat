use crate::error::*;

#[derive(Debug, Clone)]
pub struct LineRange {
    lower: usize,
    upper: usize,
}

impl Default for LineRange {
    fn default() -> LineRange {
        LineRange {
            lower: usize::min_value(),
            upper: usize::max_value(),
        }
    }
}

impl LineRange {
    pub fn new(from: usize, to: usize) -> Self {
        LineRange {
            lower: from,
            upper: to,
        }
    }

    pub fn from(range_raw: &str) -> Result<Vec<LineRange>> {
        LineRange::parse_range(range_raw)
    }

    fn parse_range(range_raw: &str) -> Result<Vec<LineRange>> {
        let mut range_vec = Vec::new();
        let mut new_range = LineRange::default();

        if range_raw.bytes().next().ok_or("Empty line range")? == b':' {
            new_range.upper = range_raw[1..].parse()?;
            range_vec.push(new_range);
            return Ok(range_vec);
        } else if range_raw.bytes().last().ok_or("Empty line range")? == b':' {
            new_range.lower = range_raw[..range_raw.len() - 1].parse()?;
            range_vec.push(new_range);
            return Ok(range_vec);
        }

        let line_numbers: Vec<&str> = range_raw.split(':').collect();
        match line_numbers.len() {
            1 => {
                new_range.lower = line_numbers[0].parse()?;
                new_range.upper = new_range.lower;
                range_vec.push(new_range);
                Ok(range_vec)
            }
            2 => {
                let tilde_splited_line_numbers: Vec<&str> = line_numbers[1].split("~").collect();
                let lower: usize = line_numbers[0].parse()?;
                let upper: usize = if tilde_splited_line_numbers[0].bytes().next() == Some(b'+') {
                    let more_lines = &tilde_splited_line_numbers[0][1..]
                        .parse()
                        .map_err(|_| "Invalid character after +")?;
                    lower + more_lines
                } else {
                    tilde_splited_line_numbers[0].parse()?
                };

                if tilde_splited_line_numbers.len() > 1 {
                    // when increment value is set.
                    let increment: usize = tilde_splited_line_numbers[1].parse()?;
                    for i in 0..=upper - lower {
                        if i % increment == 0 {
                            range_vec.push(LineRange::new(lower + i, lower + i));
                        }
                    }
                } else {
                    // when increment value is not set.
                    new_range.lower = lower;
                    new_range.upper = upper;
                    range_vec.push(new_range);
                }
                Ok(range_vec)
            }
            _ => Err(
                "Line range contained more than one ':' character. Expected format: 'N' or 'N:M'"
                    .into(),
            ),
        }
    }

    pub(crate) fn is_inside(&self, line: usize) -> bool {
        line >= self.lower && line <= self.upper
    }
}

#[test]
fn test_parse_full() {
    let range = LineRange::from("40:50").expect("Shouldn't fail on test!");
    assert_eq!(40, range[0].lower);
    assert_eq!(50, range[0].upper);
}

#[test]
fn test_parse_increment() {
    let range = LineRange::from("40:50~2").expect("Shouldn't fail on test!");
    assert_eq!(6, range.len());
    assert_eq!(40, range[0].lower);
    assert_eq!(40, range[0].upper);
    let len = range.len();
    assert_eq!(50, range[len - 1].lower);
    assert_eq!(50, range[len - 1].upper);
}

#[test]
fn test_parse_partial_min() {
    let range = LineRange::from(":50").expect("Shouldn't fail on test!");
    assert_eq!(usize::min_value(), range[0].lower);
    assert_eq!(50, range[0].upper);
}

#[test]
fn test_parse_partial_max() {
    let range = LineRange::from("40:").expect("Shouldn't fail on test!");
    assert_eq!(40, range[0].lower);
    assert_eq!(usize::max_value(), range[0].upper);
}

#[test]
fn test_parse_single() {
    let range = LineRange::from("40").expect("Shouldn't fail on test!");
    assert_eq!(40, range[0].lower);
    assert_eq!(40, range[0].upper);
}

#[test]
fn test_parse_fail() {
    let range = LineRange::from("40:50:80:90");
    assert!(range.is_err());
    let range = LineRange::from("40::80");
    assert!(range.is_err());
    let range = LineRange::from(":40:");
    assert!(range.is_err());
}

#[test]
fn test_parse_plus() {
    let range = LineRange::from("40:+10").expect("Shouldn't fail on test!");
    assert_eq!(40, range[0].lower);
    assert_eq!(50, range[0].upper);
}

#[test]
fn test_parse_plus_fail() {
    let range = LineRange::from("40:+z");
    assert!(range.is_err());
    let range = LineRange::from("40:+-10");
    assert!(range.is_err());
    let range = LineRange::from("40:+");
    assert!(range.is_err());
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum RangeCheckResult {
    // Within one of the given ranges
    InRange,

    // Before the first range or within two ranges
    BeforeOrBetweenRanges,

    // Line number is outside of all ranges and larger than the last range.
    AfterLastRange,
}

#[derive(Debug, Clone)]
pub struct LineRanges {
    ranges: Vec<LineRange>,
    largest_upper_bound: usize,
}

impl LineRanges {
    pub fn none() -> LineRanges {
        LineRanges::from(vec![])
    }

    pub fn all() -> LineRanges {
        LineRanges::from(vec![LineRange::default()])
    }

    pub fn from(ranges: Vec<LineRange>) -> LineRanges {
        let largest_upper_bound = ranges
            .iter()
            .map(|r| r.upper)
            .max()
            .unwrap_or(usize::max_value());
        LineRanges {
            ranges,
            largest_upper_bound,
        }
    }

    pub(crate) fn check(&self, line: usize) -> RangeCheckResult {
        if self.ranges.iter().any(|r| r.is_inside(line)) {
            RangeCheckResult::InRange
        } else if line < self.largest_upper_bound {
            RangeCheckResult::BeforeOrBetweenRanges
        } else {
            RangeCheckResult::AfterLastRange
        }
    }
}

impl Default for LineRanges {
    fn default() -> Self {
        Self::all()
    }
}

#[derive(Debug, Clone)]
pub struct HighlightedLineRanges(pub LineRanges);

impl Default for HighlightedLineRanges {
    fn default() -> Self {
        HighlightedLineRanges(LineRanges::none())
    }
}

#[cfg(test)]
fn ranges(rs: &[&str]) -> LineRanges {
    LineRanges::from(
        rs.iter()
            .map(|r| LineRange::from(r).unwrap())
            .flatten()
            .collect(),
    )
}

#[test]
fn test_ranges_simple() {
    let ranges = ranges(&["3:8"]);

    assert_eq!(RangeCheckResult::BeforeOrBetweenRanges, ranges.check(2));
    assert_eq!(RangeCheckResult::InRange, ranges.check(5));
    assert_eq!(RangeCheckResult::AfterLastRange, ranges.check(9));
}

#[test]
fn test_ranges_increment() {
    let ranges = ranges(&["3:8~2"]);

    assert_eq!(RangeCheckResult::BeforeOrBetweenRanges, ranges.check(2));
    assert_eq!(RangeCheckResult::InRange, ranges.check(5));
    assert_eq!(RangeCheckResult::BeforeOrBetweenRanges, ranges.check(6));
    assert_eq!(RangeCheckResult::AfterLastRange, ranges.check(9));
}

#[test]
fn test_ranges_advanced() {
    let ranges = ranges(&["3:8", "11:20", "25:30"]);

    assert_eq!(RangeCheckResult::BeforeOrBetweenRanges, ranges.check(2));
    assert_eq!(RangeCheckResult::InRange, ranges.check(5));
    assert_eq!(RangeCheckResult::BeforeOrBetweenRanges, ranges.check(9));
    assert_eq!(RangeCheckResult::InRange, ranges.check(11));
    assert_eq!(RangeCheckResult::BeforeOrBetweenRanges, ranges.check(22));
    assert_eq!(RangeCheckResult::InRange, ranges.check(28));
    assert_eq!(RangeCheckResult::AfterLastRange, ranges.check(31));
}

#[test]
fn test_ranges_open_low() {
    let ranges = ranges(&["3:8", ":5"]);

    assert_eq!(RangeCheckResult::InRange, ranges.check(1));
    assert_eq!(RangeCheckResult::InRange, ranges.check(3));
    assert_eq!(RangeCheckResult::InRange, ranges.check(7));
    assert_eq!(RangeCheckResult::AfterLastRange, ranges.check(9));
}

#[test]
fn test_ranges_open_high() {
    let ranges = ranges(&["3:", "2:5"]);

    assert_eq!(RangeCheckResult::BeforeOrBetweenRanges, ranges.check(1));
    assert_eq!(RangeCheckResult::InRange, ranges.check(3));
    assert_eq!(RangeCheckResult::InRange, ranges.check(5));
    assert_eq!(RangeCheckResult::InRange, ranges.check(9));
}

#[test]
fn test_ranges_all() {
    let ranges = LineRanges::all();

    assert_eq!(RangeCheckResult::InRange, ranges.check(1));
}

#[test]
fn test_ranges_none() {
    let ranges = LineRanges::none();

    assert_ne!(RangeCheckResult::InRange, ranges.check(1));
}

use crate::error::*;
use itertools::{Itertools, MinMaxResult};

#[derive(Debug, Copy, Clone)]
pub struct LineRange {
    lower: RangeBound,
    upper: RangeBound,
}

/// Defines a boundary for a range
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub(crate) enum RangeBound {
    // An absolute line number marking the boundary of a range
    Absolute(usize),
    // A relative (implicitly negative) offset from the end of the file as a boundary
    OffsetFromEnd(usize),
}

impl Default for LineRange {
    fn default() -> LineRange {
        LineRange {
            lower: RangeBound::Absolute(usize::MIN),
            upper: RangeBound::Absolute(usize::MAX),
        }
    }
}

impl LineRange {
    pub fn new(from: usize, to: usize) -> Self {
        LineRange {
            lower: RangeBound::Absolute(from),
            upper: RangeBound::Absolute(to),
        }
    }

    pub fn from(range_raw: &str) -> Result<LineRange> {
        LineRange::parse_range(range_raw)
    }

    fn parse_range(range_raw: &str) -> Result<LineRange> {
        let mut new_range = LineRange::default();
        let mut raw_range_iter = range_raw.bytes();
        let first_byte = raw_range_iter.next().ok_or("Empty line range")?;

        if first_byte == b':' {
            if raw_range_iter.next() == Some(b'-') {
                // E.g. ':-3'
                let value = range_raw[2..].parse()?;
                new_range.upper = RangeBound::OffsetFromEnd(value);
            } else {
                let value = range_raw[1..].parse()?;
                new_range.upper = RangeBound::Absolute(value);
            }
            return Ok(new_range);
        } else if range_raw.bytes().last().ok_or("Empty line range")? == b':' {
            if first_byte == b'-' {
                // E.g. '-3:'
                let value = range_raw[1..range_raw.len() - 1].parse()?;
                new_range.lower = RangeBound::OffsetFromEnd(value);
            } else {
                let value = range_raw[..range_raw.len() - 1].parse()?;
                new_range.lower = RangeBound::Absolute(value);
            }
            return Ok(new_range);
        }

        let line_numbers: Vec<&str> = range_raw.split(':').collect();
        match line_numbers.len() {
            1 => {
                new_range.lower = RangeBound::Absolute(line_numbers[0].parse()?);
                new_range.upper = new_range.lower;
                Ok(new_range)
            }
            2 => {
                let mut lower_absolute_bound: usize = line_numbers[0].parse()?;
                let first_byte = line_numbers[1].bytes().next();

                let upper_absolute_bound = if first_byte == Some(b'+') {
                    let more_lines = &line_numbers[1][1..]
                        .parse()
                        .map_err(|_| "Invalid character after +")?;
                    lower_absolute_bound.saturating_add(*more_lines)
                } else if first_byte == Some(b'-') {
                    // this will prevent values like "-+5" even though "+5" is valid integer
                    if line_numbers[1][1..].bytes().next() == Some(b'+') {
                        return Err("Invalid character after -".into());
                    }
                    let prior_lines = &line_numbers[1][1..]
                        .parse()
                        .map_err(|_| "Invalid character after -")?;
                    let prev_lower = lower_absolute_bound;
                    lower_absolute_bound = lower_absolute_bound.saturating_sub(*prior_lines);
                    prev_lower
                } else {
                    line_numbers[1].parse()?
                };
                new_range.lower = RangeBound::Absolute(lower_absolute_bound);
                new_range.upper = RangeBound::Absolute(upper_absolute_bound);
                Ok(new_range)
            }
            3 => {
                // Handle context syntax: N::C or N:M:C
                if line_numbers[1].is_empty() {
                    // Format: N::C - single line with context
                    let line_number: usize = line_numbers[0].parse()
                        .map_err(|_| "Invalid line number in N::C format")?;
                    let context: usize = line_numbers[2].parse()
                        .map_err(|_| "Invalid context number in N::C format")?;

                    new_range.lower = RangeBound::Absolute(line_number.saturating_sub(context));
                    new_range.upper = RangeBound::Absolute(line_number.saturating_add(context));
                } else {
                    // Format: N:M:C - range with context
                    let start_line: usize = line_numbers[0].parse()
                        .map_err(|_| "Invalid start line number in N:M:C format")?;
                    let end_line: usize = line_numbers[1].parse()
                        .map_err(|_| "Invalid end line number in N:M:C format")?;
                    let context: usize = line_numbers[2].parse()
                        .map_err(|_| "Invalid context number in N:M:C format")?;

                    new_range.lower = RangeBound::Absolute(start_line.saturating_sub(context));
                    new_range.upper = RangeBound::Absolute(end_line.saturating_add(context));
                }
                Ok(new_range)
            }
            _ => Err(
                "Line range contained too many ':' characters. Expected format: 'N', 'N:M', 'N::C', or 'N:M:C'"
                    .into(),
            ),
        }
    }

    /// Checks if a line number is inside the range.
    /// For ranges with relative offsets range bounds `max_buffered_line_number` is necessary
    /// to convert the offset to an absolute value.
    pub(crate) fn is_inside(
        &self,
        line: usize,
        max_buffered_line_number: MaxBufferedLineNumber,
    ) -> bool {
        match (self.lower, self.upper, max_buffered_line_number) {
            (RangeBound::Absolute(lower), RangeBound::Absolute(upper), _) => {
                lower <= line && line <= upper
            }
            (
                RangeBound::Absolute(lower),
                RangeBound::OffsetFromEnd(offset),
                MaxBufferedLineNumber::Final(last_line_number),
            ) => lower <= line && line <= last_line_number.saturating_sub(offset),
            (
                RangeBound::Absolute(lower),
                RangeBound::OffsetFromEnd(_),
                MaxBufferedLineNumber::Tentative(_),
            ) => {
                // We don't know the final line number yet, so the assumption is that the line is
                // still far enough away from the upper end of the range
                lower <= line
            }
            (
                RangeBound::OffsetFromEnd(offset),
                RangeBound::Absolute(upper),
                MaxBufferedLineNumber::Final(last_line_number),
            ) => last_line_number.saturating_sub(offset) <= line && line <= upper,
            (
                RangeBound::OffsetFromEnd(_),
                RangeBound::Absolute(_),
                MaxBufferedLineNumber::Tentative(_),
            ) => {
                // We don't know the final line number yet, so the assumption is that the line is
                // still too far away from the having reached the lower end of the range
                false
            }
            (
                RangeBound::OffsetFromEnd(lower),
                RangeBound::OffsetFromEnd(upper),
                MaxBufferedLineNumber::Final(last_line_number),
            ) => {
                last_line_number.saturating_sub(lower) <= line
                    && line <= last_line_number.saturating_sub(upper)
            }
            (
                RangeBound::OffsetFromEnd(_),
                RangeBound::OffsetFromEnd(_),
                MaxBufferedLineNumber::Tentative(_),
            ) => {
                // We don't know the final line number yet, so the assumption is that we're still
                // too far away from the having reached the lower end of the range
                false
            }
        }
    }
}

#[test]
fn test_parse_full() {
    let range = LineRange::from("40:50").expect("Shouldn't fail on test!");
    assert_eq!(RangeBound::Absolute(40), range.lower);
    assert_eq!(RangeBound::Absolute(50), range.upper);
}

#[test]
fn test_parse_partial_min() {
    let range = LineRange::from(":50").expect("Shouldn't fail on test!");
    assert_eq!(RangeBound::Absolute(usize::MIN), range.lower);
    assert_eq!(RangeBound::Absolute(50), range.upper);
}

#[test]
fn test_parse_partial_relative_negative_from_back() {
    let range = LineRange::from(":-5").expect("Shouldn't fail on test!");
    assert_eq!(RangeBound::Absolute(usize::MIN), range.lower);
    assert_eq!(RangeBound::OffsetFromEnd(5), range.upper);
}

#[test]
fn test_parse_relative_negative_from_back_partial() {
    let range = LineRange::from("-5:").expect("Shouldn't fail on test!");
    assert_eq!(RangeBound::OffsetFromEnd(5), range.lower);
    assert_eq!(RangeBound::Absolute(usize::MAX), range.upper);
}

#[test]
fn test_parse_partial_max() {
    let range = LineRange::from("40:").expect("Shouldn't fail on test!");
    assert_eq!(RangeBound::Absolute(40), range.lower);
    assert_eq!(RangeBound::Absolute(usize::MAX), range.upper);
}

#[test]
fn test_parse_single() {
    let range = LineRange::from("40").expect("Shouldn't fail on test!");
    assert_eq!(RangeBound::Absolute(40), range.lower);
    assert_eq!(RangeBound::Absolute(40), range.upper);
}

#[test]
fn test_parse_fail() {
    // Test 4+ colon parts should still fail
    let range = LineRange::from("40:50:80:90");
    assert!(range.is_err());
    // Test invalid formats that should still fail
    let range = LineRange::from("-2:5");
    assert!(range.is_err());
    let range = LineRange::from(":40:");
    assert!(range.is_err());
    // Test completely malformed input
    let range = LineRange::from("abc:def");
    assert!(range.is_err());
}

#[test]
fn test_parse_plus() {
    let range = LineRange::from("40:+10").expect("Shouldn't fail on test!");
    assert_eq!(RangeBound::Absolute(40), range.lower);
    assert_eq!(RangeBound::Absolute(50), range.upper);
}

#[test]
fn test_parse_plus_overflow() {
    let range = LineRange::from(&format!("{}:+1", usize::MAX)).expect("Shouldn't fail on test!");
    assert_eq!(RangeBound::Absolute(usize::MAX), range.lower);
    assert_eq!(RangeBound::Absolute(usize::MAX), range.upper);
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

#[test]
fn test_parse_minus_success() {
    let range = LineRange::from("40:-10").expect("Shouldn't fail on test!");
    assert_eq!(RangeBound::Absolute(30), range.lower);
    assert_eq!(RangeBound::Absolute(40), range.upper);
}

#[test]
fn test_parse_minus_edge_cases_success() {
    let range = LineRange::from("5:-4").expect("Shouldn't fail on test!");
    assert_eq!(RangeBound::Absolute(1), range.lower);
    assert_eq!(RangeBound::Absolute(5), range.upper);
    let range = LineRange::from("5:-5").expect("Shouldn't fail on test!");
    assert_eq!(RangeBound::Absolute(0), range.lower);
    assert_eq!(RangeBound::Absolute(5), range.upper);
    let range = LineRange::from("5:-100").expect("Shouldn't fail on test!");
    assert_eq!(RangeBound::Absolute(0), range.lower);
    assert_eq!(RangeBound::Absolute(5), range.upper);
}

#[test]
fn test_parse_minus_fail() {
    let range = LineRange::from("40:-z");
    assert!(range.is_err());
    let range = LineRange::from("40:-+10");
    assert!(range.is_err());
    let range = LineRange::from("40:-");
    assert!(range.is_err());
}

#[test]
fn test_parse_context_single_line() {
    let range = LineRange::from("35::5").expect("Shouldn't fail on test!");
    assert_eq!(RangeBound::Absolute(30), range.lower);
    assert_eq!(RangeBound::Absolute(40), range.upper);
}

#[test]
fn test_parse_context_range() {
    let range = LineRange::from("30:40:2").expect("Shouldn't fail on test!");
    assert_eq!(RangeBound::Absolute(28), range.lower);
    assert_eq!(RangeBound::Absolute(42), range.upper);

    // Test the case that used to fail but should now work
    let range = LineRange::from("40:50:80").expect("Shouldn't fail on test!");
    assert_eq!(RangeBound::Absolute(0), range.lower); // 40 - 80 = 0 (saturated)
    assert_eq!(RangeBound::Absolute(130), range.upper); // 50 + 80 = 130
}

#[test]
fn test_parse_context_edge_cases() {
    // Test with small line numbers that would underflow
    let range = LineRange::from("5::10").expect("Shouldn't fail on test!");
    assert_eq!(RangeBound::Absolute(0), range.lower);
    assert_eq!(RangeBound::Absolute(15), range.upper);

    // Test with zero context
    let range = LineRange::from("50::0").expect("Shouldn't fail on test!");
    assert_eq!(RangeBound::Absolute(50), range.lower);
    assert_eq!(RangeBound::Absolute(50), range.upper);

    // Test range with zero context
    let range = LineRange::from("30:40:0").expect("Shouldn't fail on test!");
    assert_eq!(RangeBound::Absolute(30), range.lower);
    assert_eq!(RangeBound::Absolute(40), range.upper);
}

#[test]
fn test_parse_context_fail() {
    let range = LineRange::from("40::z");
    assert!(range.is_err());
    let range = LineRange::from("::5");
    assert!(range.is_err());
    let range = LineRange::from("40::");
    assert!(range.is_err());
    let range = LineRange::from("30:40:z");
    assert!(range.is_err());
    let range = LineRange::from("30::40:5");
    assert!(range.is_err());
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum RangeCheckResult {
    // Within one of the given ranges
    InRange,

    // Before the first range or within two ranges
    BeforeOrBetweenRanges,

    // Line number is outside of all ranges and larger than the last range.
    AfterLastRange,
}

/// Represents the maximum line number in the buffer when reading a file.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub(crate) enum MaxBufferedLineNumber {
    // The currently known maximum line number, may not be the final line number
    Tentative(usize),
    // The final line number, when EOF has been reached
    Final(usize),
}

#[derive(Debug, Clone)]
pub struct LineRanges {
    ranges: Vec<LineRange>,
    // The largest absolute upper line number of all ranges
    largest_absolute_upper_bound: usize,
    // The smallest relative offset from the end of all ranges
    smallest_offset_from_end: usize,
    // The largest relative offset from the end of all ranges
    largest_offset_from_end: usize,
}

impl LineRanges {
    pub fn none() -> LineRanges {
        LineRanges::from(vec![])
    }

    pub fn all() -> LineRanges {
        LineRanges::from(vec![LineRange::default()])
    }

    pub fn from(ranges: Vec<LineRange>) -> LineRanges {
        let largest_absolute_upper_bound = ranges
            .iter()
            .filter_map(|r| match r.upper {
                RangeBound::Absolute(upper) => Some(upper),
                _ => None,
            })
            .max()
            .unwrap_or(usize::MAX);

        let offsets_min_max = ranges
            .iter()
            .flat_map(|r| [r.lower, r.upper])
            .filter_map(|r| match r {
                RangeBound::OffsetFromEnd(offset) => Some(offset),
                _ => None,
            })
            .minmax();

        let (smallest_offset_from_end, largest_offset_from_end) = match offsets_min_max {
            MinMaxResult::NoElements => (usize::MIN, usize::MIN),
            MinMaxResult::OneElement(offset) => (offset, offset),
            MinMaxResult::MinMax(min, max) => (min, max),
        };

        LineRanges {
            ranges,
            largest_absolute_upper_bound,
            smallest_offset_from_end,
            largest_offset_from_end,
        }
    }

    pub(crate) fn check(
        &self,
        line: usize,
        max_buffered_line_number: MaxBufferedLineNumber,
    ) -> RangeCheckResult {
        if self
            .ranges
            .iter()
            .any(|r| r.is_inside(line, max_buffered_line_number))
        {
            RangeCheckResult::InRange
        } else if matches!(max_buffered_line_number, MaxBufferedLineNumber::Final(final_line_number) if line > final_line_number.saturating_sub(self.smallest_offset_from_end))
        {
            RangeCheckResult::AfterLastRange
        } else if line < self.largest_absolute_upper_bound {
            RangeCheckResult::BeforeOrBetweenRanges
        } else {
            RangeCheckResult::AfterLastRange
        }
    }

    pub(crate) fn largest_offset_from_end(&self) -> usize {
        self.largest_offset_from_end
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
    LineRanges::from(rs.iter().map(|r| LineRange::from(r).unwrap()).collect())
}

#[test]
fn test_ranges_simple() {
    let ranges = ranges(&["3:8"]);

    assert_eq!(
        RangeCheckResult::BeforeOrBetweenRanges,
        ranges.check(2, MaxBufferedLineNumber::Tentative(2))
    );
    assert_eq!(
        RangeCheckResult::InRange,
        ranges.check(5, MaxBufferedLineNumber::Tentative(5))
    );
    assert_eq!(
        RangeCheckResult::AfterLastRange,
        ranges.check(9, MaxBufferedLineNumber::Tentative(9))
    );
}

#[test]
fn test_ranges_advanced() {
    let ranges = ranges(&["3:8", "11:20", "25:30"]);

    assert_eq!(
        RangeCheckResult::BeforeOrBetweenRanges,
        ranges.check(2, MaxBufferedLineNumber::Tentative(2))
    );
    assert_eq!(
        RangeCheckResult::InRange,
        ranges.check(5, MaxBufferedLineNumber::Tentative(5))
    );
    assert_eq!(
        RangeCheckResult::BeforeOrBetweenRanges,
        ranges.check(9, MaxBufferedLineNumber::Tentative(9))
    );
    assert_eq!(
        RangeCheckResult::InRange,
        ranges.check(11, MaxBufferedLineNumber::Tentative(11))
    );
    assert_eq!(
        RangeCheckResult::BeforeOrBetweenRanges,
        ranges.check(22, MaxBufferedLineNumber::Tentative(22))
    );
    assert_eq!(
        RangeCheckResult::InRange,
        ranges.check(28, MaxBufferedLineNumber::Tentative(28))
    );
    assert_eq!(
        RangeCheckResult::AfterLastRange,
        ranges.check(31, MaxBufferedLineNumber::Tentative(31))
    );
}

#[test]
fn test_ranges_open_low() {
    let ranges = ranges(&["3:8", ":5"]);

    assert_eq!(
        RangeCheckResult::InRange,
        ranges.check(1, MaxBufferedLineNumber::Tentative(1))
    );
    assert_eq!(
        RangeCheckResult::InRange,
        ranges.check(3, MaxBufferedLineNumber::Tentative(3))
    );
    assert_eq!(
        RangeCheckResult::InRange,
        ranges.check(7, MaxBufferedLineNumber::Tentative(7))
    );
    assert_eq!(
        RangeCheckResult::AfterLastRange,
        ranges.check(9, MaxBufferedLineNumber::Tentative(9))
    );
}

#[test]
fn test_ranges_open_high() {
    let ranges = ranges(&["3:", "2:5"]);
    assert_eq!(
        RangeCheckResult::BeforeOrBetweenRanges,
        ranges.check(1, MaxBufferedLineNumber::Tentative(1))
    );

    assert_eq!(
        RangeCheckResult::BeforeOrBetweenRanges,
        ranges.check(1, MaxBufferedLineNumber::Final(10))
    );
    assert_eq!(
        RangeCheckResult::InRange,
        ranges.check(2, MaxBufferedLineNumber::Final(10))
    );
    assert_eq!(
        RangeCheckResult::InRange,
        ranges.check(9, MaxBufferedLineNumber::Final(10))
    );
    assert_eq!(
        RangeCheckResult::InRange,
        ranges.check(10, MaxBufferedLineNumber::Final(10))
    );
    assert_eq!(
        RangeCheckResult::InRange,
        ranges.check(3, MaxBufferedLineNumber::Tentative(3))
    );
    assert_eq!(
        RangeCheckResult::InRange,
        ranges.check(5, MaxBufferedLineNumber::Tentative(5))
    );
    assert_eq!(
        RangeCheckResult::InRange,
        ranges.check(9, MaxBufferedLineNumber::Tentative(9))
    );
}

#[test]
fn test_ranges_open_up_to_3_from_end() {
    let ranges = ranges(&[":-3"]);
    assert_eq!(
        RangeCheckResult::InRange,
        ranges.check(1, MaxBufferedLineNumber::Tentative(1))
    );
    assert_eq!(
        RangeCheckResult::InRange,
        ranges.check(3, MaxBufferedLineNumber::Tentative(3))
    );
    assert_eq!(
        RangeCheckResult::InRange,
        ranges.check(5, MaxBufferedLineNumber::Tentative(8))
    );
    assert_eq!(
        RangeCheckResult::InRange,
        ranges.check(1, MaxBufferedLineNumber::Final(6))
    );
    assert_eq!(
        RangeCheckResult::InRange,
        ranges.check(2, MaxBufferedLineNumber::Final(6))
    );
    assert_eq!(
        RangeCheckResult::InRange,
        ranges.check(3, MaxBufferedLineNumber::Final(6))
    );
    assert_eq!(
        RangeCheckResult::AfterLastRange,
        ranges.check(4, MaxBufferedLineNumber::Final(6))
    );
    assert_eq!(
        RangeCheckResult::AfterLastRange,
        ranges.check(5, MaxBufferedLineNumber::Final(6))
    );
    assert_eq!(
        RangeCheckResult::AfterLastRange,
        ranges.check(6, MaxBufferedLineNumber::Final(6))
    );
}

#[test]
fn test_ranges_multiple_negative_from_back() {
    let ranges = ranges(&[":-3", ":-9"]);
    assert_eq!(
        RangeCheckResult::InRange,
        ranges.check(1, MaxBufferedLineNumber::Tentative(1))
    );
    assert_eq!(
        RangeCheckResult::InRange,
        ranges.check(3, MaxBufferedLineNumber::Tentative(3))
    );
    assert_eq!(
        RangeCheckResult::InRange,
        ranges.check(5, MaxBufferedLineNumber::Tentative(14))
    );
    assert_eq!(
        RangeCheckResult::InRange,
        ranges.check(1, MaxBufferedLineNumber::Final(16))
    );
    assert_eq!(
        RangeCheckResult::InRange,
        ranges.check(7, MaxBufferedLineNumber::Final(16))
    );
    assert_eq!(
        RangeCheckResult::InRange,
        ranges.check(13, MaxBufferedLineNumber::Final(16))
    );
    assert_eq!(
        RangeCheckResult::AfterLastRange,
        ranges.check(14, MaxBufferedLineNumber::Final(16))
    );
    assert_eq!(
        RangeCheckResult::AfterLastRange,
        ranges.check(16, MaxBufferedLineNumber::Final(16))
    );
}

#[test]
fn test_ranges_3_from_back_up_to_end() {
    let ranges = ranges(&["-3:"]);

    assert_eq!(
        RangeCheckResult::BeforeOrBetweenRanges,
        ranges.check(1, MaxBufferedLineNumber::Tentative(1))
    );
    assert_eq!(
        RangeCheckResult::BeforeOrBetweenRanges,
        ranges.check(3, MaxBufferedLineNumber::Tentative(3))
    );
    assert_eq!(
        RangeCheckResult::BeforeOrBetweenRanges,
        ranges.check(5, MaxBufferedLineNumber::Tentative(8))
    );
    assert_eq!(
        RangeCheckResult::BeforeOrBetweenRanges,
        ranges.check(1, MaxBufferedLineNumber::Final(5))
    );
    assert_eq!(
        RangeCheckResult::InRange,
        ranges.check(2, MaxBufferedLineNumber::Final(5))
    );
    assert_eq!(
        RangeCheckResult::InRange,
        ranges.check(3, MaxBufferedLineNumber::Final(5))
    );
    assert_eq!(
        RangeCheckResult::InRange,
        ranges.check(4, MaxBufferedLineNumber::Final(5))
    );
    assert_eq!(
        RangeCheckResult::InRange,
        ranges.check(5, MaxBufferedLineNumber::Final(5))
    );
}

#[test]
fn test_ranges_multiple_negative_offsets_to_end() {
    let ranges = ranges(&["-3:", "-12:"]);
    assert_eq!(
        RangeCheckResult::BeforeOrBetweenRanges,
        ranges.check(5, MaxBufferedLineNumber::Tentative(8))
    );
    assert_eq!(
        RangeCheckResult::BeforeOrBetweenRanges,
        ranges.check(5, MaxBufferedLineNumber::Tentative(17))
    );
    assert_eq!(
        RangeCheckResult::InRange,
        ranges.check(8, MaxBufferedLineNumber::Final(20))
    );
    assert_eq!(
        RangeCheckResult::InRange,
        ranges.check(9, MaxBufferedLineNumber::Final(20))
    );
}

#[test]
fn test_ranges_absolute_bound_and_offset() {
    let ranges = ranges(&["5:", ":-2"]);
    assert_eq!(
        RangeCheckResult::InRange,
        ranges.check(4, MaxBufferedLineNumber::Tentative(6))
    );
    assert_eq!(
        RangeCheckResult::InRange,
        ranges.check(5, MaxBufferedLineNumber::Tentative(7))
    );
    assert_eq!(
        RangeCheckResult::InRange,
        ranges.check(8, MaxBufferedLineNumber::Final(10))
    );
    assert_eq!(
        RangeCheckResult::InRange,
        ranges.check(9, MaxBufferedLineNumber::Final(10))
    );
    assert_eq!(
        RangeCheckResult::InRange,
        ranges.check(10, MaxBufferedLineNumber::Final(10))
    );
}

#[test]
fn test_ranges_all() {
    let ranges = LineRanges::all();

    assert_eq!(
        RangeCheckResult::InRange,
        ranges.check(1, MaxBufferedLineNumber::Tentative(1))
    );
}

#[test]
fn test_ranges_none() {
    let ranges = LineRanges::none();

    assert_ne!(
        RangeCheckResult::InRange,
        ranges.check(1, MaxBufferedLineNumber::Tentative(1))
    );
}

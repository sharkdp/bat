use std::fmt::Debug;

use colors_transform::{Color as RgbColor, Rgb};
use syntect::highlighting::Color;

use crate::error::*;

pub trait BasicLineRange: Default + Debug {
    fn parse_range(range_raw: &str) -> Result<Self>
    where
        Self: Sized;
    fn from(range_raw: &str) -> Result<Self>
    where
        Self: Sized;
    fn get_lower(&self) -> usize;
    fn get_upper(&self) -> usize;
    fn is_inside(&self, line: usize) -> bool {
        line >= self.get_lower() && line <= self.get_upper()
    }
    fn get_color(&self) -> Option<Color> {
        None
    }
}

#[derive(Debug, Clone)]
pub struct LineRange {
    lower: usize,
    upper: usize,
}

#[derive(Debug, Clone)]
pub struct ColoredLineRange {
    lower: usize,
    upper: usize,
    color: Option<Color>,
}

impl Default for LineRange {
    fn default() -> Self {
        Self {
            lower: usize::min_value(),
            upper: usize::max_value(),
        }
    }
}

impl Default for ColoredLineRange {
    fn default() -> Self {
        Self {
            lower: usize::min_value(),
            upper: usize::max_value(),
            color: None,
        }
    }
}

impl BasicLineRange for LineRange {
    fn parse_range(range_raw: &str) -> Result<LineRange> {
        let mut new_range = LineRange::default();

        if range_raw.bytes().next().ok_or("Empty line range")? == b':' {
            new_range.upper = range_raw[1..].parse()?;
            return Ok(new_range);
        } else if range_raw.bytes().last().ok_or("Empty line range")? == b':' {
            new_range.lower = range_raw[..range_raw.len() - 1].parse()?;
            return Ok(new_range);
        }

        let line_numbers: Vec<&str> = range_raw.split(':').collect();
        match line_numbers.len() {
            1 => {
                new_range.lower = line_numbers[0].parse()?;
                new_range.upper = new_range.lower;
                Ok(new_range)
            }
            2 => {
                new_range.lower = line_numbers[0].parse()?;

                new_range.upper = if line_numbers[1].bytes().next() == Some(b'+') {
                    let more_lines = &line_numbers[1][1..]
                        .parse()
                        .map_err(|_| "Invalid character after +")?;
                    new_range.lower + more_lines
                } else {
                    line_numbers[1].parse()?
                };

                Ok(new_range)
            }
            _ => Err(
                "Line range contained more than one ':' character. Expected format: 'N' or 'N:M'"
                    .into(),
            ),
        }
    }

    fn from(range_raw: &str) -> Result<Self> {
        Self::parse_range(range_raw)
    }

    fn get_lower(&self) -> usize {
        self.lower
    }

    fn get_upper(&self) -> usize {
        self.upper
    }
}

impl BasicLineRange for ColoredLineRange {
    fn parse_range(range_raw: &str) -> Result<Self> {
        let mut new_range = Self::default();
        let parsing_error_msg = "Invalid range string";
        let mut range_raw_parts = range_raw.split('#');
        let range_raw = range_raw_parts.next().ok_or(parsing_error_msg)?;

        if let Some(color) = range_raw_parts.next() {
            let rgb_color = Rgb::from_hex_str(color).or(Err("Invalid range color"))?;
            new_range.color = Some(Color {
                r: rgb_color.get_red() as u8,
                g: rgb_color.get_green() as u8,
                b: rgb_color.get_blue() as u8,
                a: 0xFF,
            });
        }

        if range_raw.bytes().next().ok_or("Empty line range")? == b':' {
            new_range.upper = range_raw[1..].parse().or(Err(parsing_error_msg))?;
            return Ok(new_range);
        } else if range_raw.bytes().last().ok_or("Empty line range")? == b':' {
            new_range.lower = range_raw[..range_raw.len() - 1]
                .parse()
                .or(Err(parsing_error_msg))?;
            return Ok(new_range);
        }

        let line_numbers: Vec<&str> = range_raw.split(':').collect();
        match line_numbers.len() {
            1 => {
                new_range.lower = line_numbers[0].parse()?;
                new_range.upper = new_range.lower;
                Ok(new_range)
            }
            2 => {
                new_range.lower = line_numbers[0].parse()?;

                new_range.upper = if line_numbers[1].bytes().next() == Some(b'+') {
                    let more_lines = &line_numbers[1][1..]
                        .parse()
                        .map_err(|_| "Invalid character after +")?;
                    new_range.lower + more_lines
                } else {
                    line_numbers[1].parse()?
                };

                Ok(new_range)
            }
            _ => Err(
                "Line range contained more than one ':' character. Expected format: 'N' or 'N:M'"
                    .into(),
            ),
        }
    }

    fn from(range_raw: &str) -> Result<Self> {
        Self::parse_range(range_raw)
    }

    fn get_lower(&self) -> usize {
        self.lower
    }

    fn get_upper(&self) -> usize {
        self.upper
    }

    fn get_color(&self) -> Option<Color> {
        self.color
    }
}

impl LineRange {
    pub fn new(from: usize, to: usize) -> Self {
        LineRange {
            lower: from,
            upper: to,
        }
    }
}

impl ColoredLineRange {
    pub fn new(from: usize, to: usize, color: Option<Color>) -> Self {
        Self {
            lower: from,
            upper: to,
            color,
        }
    }
}

#[test]
fn test_parse_full() {
    let range: LineRange = BasicLineRange::from("40:50").expect("Shouldn't fail on test!");
    assert_eq!(40, range.lower);
    assert_eq!(50, range.upper);
}

#[test]
fn test_parse_partial_min() {
    let range: LineRange = BasicLineRange::from(":50").expect("Shouldn't fail on test!");
    assert_eq!(usize::min_value(), range.lower);
    assert_eq!(50, range.upper);
}

#[test]
fn test_parse_partial_max() {
    let range: LineRange = BasicLineRange::from("40:").expect("Shouldn't fail on test!");
    assert_eq!(40, range.lower);
    assert_eq!(usize::max_value(), range.upper);
}

#[test]
fn test_parse_single() {
    let range: LineRange = BasicLineRange::from("40").expect("Shouldn't fail on test!");
    assert_eq!(40, range.lower);
    assert_eq!(40, range.upper);
}

#[test]
fn test_parse_fail() {
    let range: Result<LineRange> = BasicLineRange::from("40:50:80");
    assert!(range.is_err());
    let range: Result<LineRange> = BasicLineRange::from("40::80");
    assert!(range.is_err());
    let range: Result<LineRange> = BasicLineRange::from(":40:");
    assert!(range.is_err());
}

#[test]
fn test_parse_plus() {
    let range: LineRange = BasicLineRange::from("40:+10").expect("Shouldn't fail on test!");
    assert_eq!(40, range.lower);
    assert_eq!(50, range.upper);
}

#[test]
fn test_parse_plus_fail() {
    let range: Result<LineRange> = BasicLineRange::from("40:+z");
    assert!(range.is_err());
    let range: Result<LineRange> = BasicLineRange::from("40:+-10");
    assert!(range.is_err());
    let range: Result<LineRange> = BasicLineRange::from("40:+");
    assert!(range.is_err());
}

#[test]
fn test_parse_colored_white() {
    let range: ColoredLineRange =
        BasicLineRange::from("40:50#fff").expect("Shouldn't fail on test!");
    assert_eq!(40, range.lower);
    assert_eq!(50, range.upper);
    assert!(range.color.is_some());
    assert_eq!(Color::WHITE, range.color.unwrap());
}

#[test]
fn test_parse_colored_black_long() {
    let range: ColoredLineRange =
        BasicLineRange::from("40:50#000000").expect("Shouldn't fail on test!");
    assert_eq!(40, range.lower);
    assert_eq!(50, range.upper);
    assert!(range.color.is_some());
    assert_eq!(Color::BLACK, range.color.unwrap());
}

#[test]
fn test_parse_invalid_colored_value() {
    let range: Result<ColoredLineRange> = BasicLineRange::from("40:50#00");
    assert!(range.is_err());
    let range: Result<ColoredLineRange> = BasicLineRange::from("40:50#0000");
    assert!(range.is_err());
    assert_eq!(range.unwrap_err().to_string(), "Invalid range color");
}

#[test]
fn test_parse_single_colored_line() {
    let range: ColoredLineRange = BasicLineRange::from("40#fff").expect("Shouldn't fail on test!");
    assert_eq!(Color::WHITE, range.color.unwrap());
}

#[test]
fn test_parse_colored_plus() {
    let range: ColoredLineRange =
        BasicLineRange::from("40:+5#000000").expect("Shouldn't fail on test!");
    assert_eq!(40, range.lower);
    assert_eq!(45, range.upper);
    assert!(range.color.is_some());
    assert_eq!(Color::BLACK, range.color.unwrap());
}

#[test]
fn test_parse_colored_second_only() {
    let range: ColoredLineRange =
        BasicLineRange::from(":40#000000").expect("Shouldn't fail on test!");
    assert_eq!(0, range.lower);
    assert_eq!(40, range.upper);
    assert!(range.color.is_some());
    assert_eq!(Color::BLACK, range.color.unwrap());
}

#[test]
fn test_parse_colored_uppercase() {
    let range: ColoredLineRange = BasicLineRange::from("40#FFF").expect("Shouldn't fail on test!");
    assert!(range.color.is_some());
    assert_eq!(Color::WHITE, range.color.unwrap());
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum RangeCheckResult {
    // Within one of the given ranges
    InRange(Option<Color>),

    // Before the first range or within two ranges
    BeforeOrBetweenRanges,

    // Line number is outside of all ranges and larger than the last range.
    AfterLastRange,
}

#[derive(Debug, Clone)]
pub struct LineRanges<T: BasicLineRange> {
    ranges: Vec<T>,
    largest_upper_bound: usize,
}

impl<T: BasicLineRange> LineRanges<T> {
    pub fn none() -> LineRanges<T> {
        LineRanges::from(vec![])
    }

    pub fn all() -> LineRanges<T> {
        LineRanges::from(vec![T::default()])
    }

    pub fn from(ranges: Vec<T>) -> LineRanges<T> {
        let largest_upper_bound: usize = ranges
            .iter()
            .map(|r| r.get_upper())
            .max()
            .unwrap_or(usize::max_value());
        LineRanges {
            ranges,
            largest_upper_bound,
        }
    }

    pub(crate) fn check(&self, line: usize) -> RangeCheckResult {
        let matching_line_range = self.ranges.iter().find(|r| r.is_inside(line));
        if let Some(line_range) = matching_line_range {
            RangeCheckResult::InRange(line_range.get_color())
        } else if line < self.largest_upper_bound {
            RangeCheckResult::BeforeOrBetweenRanges
        } else {
            RangeCheckResult::AfterLastRange
        }
    }
}

impl<T: BasicLineRange> Default for LineRanges<T> {
    fn default() -> Self {
        Self::all()
    }
}

#[derive(Debug, Clone)]
pub struct HighlightedLineRanges<T: BasicLineRange>(pub LineRanges<T>);

impl<T: BasicLineRange> Default for HighlightedLineRanges<T> {
    fn default() -> Self {
        HighlightedLineRanges(LineRanges::none())
    }
}

#[cfg(test)]
fn ranges<T: BasicLineRange>(rs: &[&str]) -> LineRanges<T> {
    LineRanges::from(
        rs.iter()
            .map(|r| BasicLineRange::from(r).unwrap())
            .collect(),
    )
}

#[test]
fn test_ranges_simple() {
    let ranges: LineRanges<LineRange> = ranges(&["3:8"]);

    assert_eq!(RangeCheckResult::BeforeOrBetweenRanges, ranges.check(2));
    assert_eq!(RangeCheckResult::InRange(None), ranges.check(5));
    assert_eq!(RangeCheckResult::AfterLastRange, ranges.check(9));
}

#[test]
fn test_ranges_advanced() {
    let ranges: LineRanges<LineRange> = ranges(&["3:8", "11:20", "25:30"]);

    assert_eq!(RangeCheckResult::BeforeOrBetweenRanges, ranges.check(2));
    assert_eq!(RangeCheckResult::InRange(None), ranges.check(5));
    assert_eq!(RangeCheckResult::BeforeOrBetweenRanges, ranges.check(9));
    assert_eq!(RangeCheckResult::InRange(None), ranges.check(11));
    assert_eq!(RangeCheckResult::BeforeOrBetweenRanges, ranges.check(22));
    assert_eq!(RangeCheckResult::InRange(None), ranges.check(28));
    assert_eq!(RangeCheckResult::AfterLastRange, ranges.check(31));
}

#[test]
fn test_ranges_open_low() {
    let ranges: LineRanges<LineRange> = ranges(&["3:8", ":5"]);

    assert_eq!(RangeCheckResult::InRange(None), ranges.check(1));
    assert_eq!(RangeCheckResult::InRange(None), ranges.check(3));
    assert_eq!(RangeCheckResult::InRange(None), ranges.check(7));
    assert_eq!(RangeCheckResult::AfterLastRange, ranges.check(9));
}

#[test]
fn test_ranges_open_high() {
    let ranges: LineRanges<LineRange> = ranges(&["3:", "2:5"]);

    assert_eq!(RangeCheckResult::BeforeOrBetweenRanges, ranges.check(1));
    assert_eq!(RangeCheckResult::InRange(None), ranges.check(3));
    assert_eq!(RangeCheckResult::InRange(None), ranges.check(5));
    assert_eq!(RangeCheckResult::InRange(None), ranges.check(9));
}

#[test]
fn test_ranges_all() {
    let ranges: LineRanges<LineRange> = LineRanges::all();

    assert_eq!(RangeCheckResult::InRange(None), ranges.check(1));
}

#[test]
fn test_ranges_none() {
    let ranges: LineRanges<LineRange> = LineRanges::none();

    assert_ne!(RangeCheckResult::InRange(None), ranges.check(1));
}

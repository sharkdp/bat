use errors::*;

#[derive(Clone)]
pub struct LineRange {
    pub lower: usize,
    pub upper: usize,
}

impl LineRange {
    pub fn from(range_raw: &str) -> Result<LineRange> {
        LineRange::parse_range(range_raw)
    }

    pub fn new() -> LineRange {
        LineRange {
            lower: usize::min_value(),
            upper: usize::max_value(),
        }
    }

    pub fn parse_range(range_raw: &str) -> Result<LineRange> {
        let mut new_range = LineRange::new();

        if range_raw.bytes().nth(0).ok_or("Empty line range")? == b':' {
            new_range.upper = range_raw[1..].parse()?;
            return Ok(new_range);
        } else if range_raw.bytes().last().ok_or("Empty line range")? == b':' {
            new_range.lower = range_raw[..range_raw.len() - 1].parse()?;
            return Ok(new_range);
        }

        let line_numbers: Vec<&str> = range_raw.split(':').collect();
        if line_numbers.len() == 2 {
            new_range.lower = line_numbers[0].parse()?;
            new_range.upper = line_numbers[1].parse()?;
            return Ok(new_range);
        }

        Err("expected single ':' character".into())
    }
}

#[test]
fn test_parse_full() {
    let range = LineRange::from("40:50").expect("Shouldn't fail on test!");
    assert_eq!(40, range.lower);
    assert_eq!(50, range.upper);
}

#[test]
fn test_parse_partial_min() {
    let range = LineRange::from(":50").expect("Shouldn't fail on test!");
    assert_eq!(usize::min_value(), range.lower);
    assert_eq!(50, range.upper);
}

#[test]
fn test_parse_partial_max() {
    let range = LineRange::from("40:").expect("Shouldn't fail on test!");
    assert_eq!(40, range.lower);
    assert_eq!(usize::max_value(), range.upper);
}

#[test]
fn test_parse_fail() {
    let range = LineRange::from("40:50:80");
    assert!(range.is_err());
    let range = LineRange::from("40::80");
    assert!(range.is_err());
    let range = LineRange::from(":40:");
    assert!(range.is_err());
    let range = LineRange::from("40");
    assert!(range.is_err());
}

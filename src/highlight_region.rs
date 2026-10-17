//! Character regions for partial-line highlighting.

use std::ops::Range;
use std::str::FromStr;

use unicode_segmentation::UnicodeSegmentation;

use crate::error::{Error, Result};
use crate::vscreen::{EscapeSequenceOffsets, EscapeSequenceOffsetsIterator};

/// An inclusive range of one-based line and character positions.
///
/// Columns count Unicode grapheme clusters before tab expansion. ANSI escape
/// sequences do not count as characters. With `--show-all`, columns refer to the
/// resulting marker text.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct HighlightRegion {
    start: (usize, usize),
    end: (usize, usize),
}

impl HighlightRegion {
    pub fn new(start: (usize, usize), end: (usize, usize)) -> Result<Self> {
        if start.0 == 0 || start.1 == 0 || end.0 == 0 || end.1 == 0 || start > end {
            return Err("Highlight positions must start at 1 and be in ascending order".into());
        }
        Ok(Self { start, end })
    }

    pub fn contains(&self, line: usize, column: usize) -> bool {
        self.start <= (line, column) && (line, column) <= self.end
    }
}

impl FromStr for HighlightRegion {
    type Err = Error;

    fn from_str(value: &str) -> Result<Self> {
        if !value.contains('.') || !value.bytes().any(|b| b.is_ascii_digit()) {
            return Err("Character ranges require a dot between line and column".into());
        }
        let (left, right) = value
            .split_once(':')
            .map_or((value, None), |(a, b)| (a, Some(b)));
        let start = parse_position(left, 1, 1)?;
        let end = match right {
            None => start,
            Some("") => (usize::MAX, usize::MAX),
            Some(right) => parse_position(right, start.0, usize::MAX)?,
        };
        Self::new(start, end)
    }
}

fn parse_position(
    value: &str,
    default_line: usize,
    default_column: usize,
) -> Result<(usize, usize)> {
    let (line, column) = value.split_once('.').unwrap_or((value, ""));
    let number = |raw: &str, default| -> Result<usize> {
        if raw.is_empty() {
            Ok(default)
        } else if raw.bytes().all(|b| b.is_ascii_digit()) {
            Ok(raw.parse()?)
        } else {
            Err(format!("Invalid character position: '{value}'").into())
        }
    };
    Ok((number(line, default_line)?, number(column, default_column)?))
}

pub(crate) fn selected_byte_ranges(
    line: &str,
    line_number: usize,
    regions: &[HighlightRegion],
) -> Vec<Range<usize>> {
    if !regions
        .iter()
        .any(|r| r.start.0 <= line_number && line_number <= r.end.0)
    {
        return Vec::new();
    }
    let mut visible = String::new();
    let mut segments = Vec::new();
    for sequence in EscapeSequenceOffsetsIterator::new(line.trim_end_matches(['\r', '\n'])) {
        if let EscapeSequenceOffsets::Text { .. } = sequence {
            let source = sequence.index_of_start()..sequence.index_past_end();
            let start = visible.len();
            visible.push_str(&line[source.clone()]);
            segments.push((start..visible.len(), source));
        }
    }

    let mut result: Vec<Range<usize>> = Vec::new();
    let mut previous_selected = false;
    for (column, (start, grapheme)) in visible.grapheme_indices(true).enumerate() {
        let selected = regions.iter().any(|r| r.contains(line_number, column + 1));
        if selected {
            let end = start + grapheme.len();
            let first = &segments[segments.partition_point(|(range, _)| range.end <= start)];
            let last = &segments[segments.partition_point(|(range, _)| range.start < end) - 1];
            let source_start = first.1.start + start - first.0.start;
            let source_end = last.1.start + end - last.0.start;
            if previous_selected {
                result.last_mut().unwrap().end = source_end;
            } else {
                result.push(source_start..source_end);
            }
        }
        previous_selected = selected;
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_inclusive_positions_and_open_bounds() {
        for (value, start, end) in [
            ("2.3", (2, 3), (2, 3)),
            ("2.3:.7", (2, 3), (2, 7)),
            ("2.3:4.5", (2, 3), (4, 5)),
            ("2.:.7", (2, 1), (2, 7)),
            (":4.5", (1, 1), (4, 5)),
            ("2.3:", (2, 3), (usize::MAX, usize::MAX)),
            ("2.3:4", (2, 3), (4, usize::MAX)),
        ] {
            assert_eq!(
                value.parse::<HighlightRegion>().unwrap(),
                HighlightRegion::new(start, end).unwrap()
            );
        }
        for value in [
            "",
            "2",
            "0.2",
            "2.0",
            "3.2:2.4",
            "2.7:.3",
            "2.1:3.2:4",
            "2.x",
            "2.-1",
            "2.+1",
            "2..3",
            "2.999999999999999999999999999999999",
        ] {
            assert!(value.parse::<HighlightRegion>().is_err(), "{value}");
        }
    }

    #[test]
    fn coordinates_ignore_escapes_and_keep_graphemes_intact() {
        let text = "a\x1b[31me\x1b[0m\u{301}中👩‍💻z\r\n";
        let regions = ["1.2:.4".parse().unwrap()];
        let ranges = selected_byte_ranges(text, 1, &regions);
        assert_eq!(ranges.len(), 1);
        assert_eq!(&text[ranges[0].clone()], "e\x1b[0m\u{301}中👩‍💻");
        assert!(selected_byte_ranges(text, 2, &regions).is_empty());
    }

    #[test]
    fn overlapping_and_out_of_bounds_ranges_are_clipped_to_content() {
        let ranges = ["1.2:.4", "1.3:.6", "1.99:.100"].map(|s| s.parse().unwrap());
        assert_eq!(selected_byte_ranges("abcdef\n", 1, &ranges), vec![1..6]);
        assert!(selected_byte_ranges("\x1b[31m\n", 1, &ranges).is_empty());
    }
}

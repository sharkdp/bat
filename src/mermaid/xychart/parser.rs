use super::{AxisRange, SeriesType, XyAxis, XyChart, XyChartSeries};
use crate::mermaid::lex::Cursor;

struct AxisCategories<'a> {
    title: Option<&'a str>,
    values: &'a str,
}

struct AxisRangeMatch<'a> {
    title: Option<&'a str>,
    range: AxisRange,
}

/// Parse Mermaid `xychart-beta` source into its logical chart representation.
#[allow(clippy::suboptimal_flops)]
pub fn parse_xy_chart(text: &str) -> XyChart {
    let mut x_axis = XyAxis::default();
    let mut y_axis = XyAxis::default();
    let mut series = Vec::new();
    let mut title = None;
    let mut horizontal = false;

    for line in text
        .split('\n')
        .map(str::trim)
        .filter(|line| !line.is_empty() && !line.starts_with("%%"))
    {
        if parse_header(line) {
            if contains_horizontal(line) {
                horizontal = true;
            }
            continue;
        }
        if let Some(value) = parse_title(line) {
            title = Some(value.to_owned());
            continue;
        }
        if let Some(axis) = parse_axis_categories(line) {
            if let Some(axis_title) = axis.title.filter(|value| !value.is_empty()) {
                x_axis.title = Some(axis_title.to_owned());
            }
            x_axis.categories = Some(
                axis.values
                    .split(',')
                    .map(|value| value.trim().to_owned())
                    .collect(),
            );
            continue;
        }
        if let Some(axis) = parse_axis_range(line, "x-axis") {
            if let Some(axis_title) = axis.title.filter(|value| !value.is_empty()) {
                x_axis.title = Some(axis_title.to_owned());
            }
            x_axis.range = Some(axis.range);
            continue;
        }
        if let Some(axis) = parse_axis_range(line, "y-axis") {
            if let Some(axis_title) = axis.title.filter(|value| !value.is_empty()) {
                y_axis.title = Some(axis_title.to_owned());
            }
            y_axis.range = Some(axis.range);
            continue;
        }
        if let Some(axis_title) = parse_y_title(line) {
            y_axis.title = Some(axis_title.to_owned());
            continue;
        }
        if let Some(values) = parse_series(line, "bar") {
            series.push(XyChartSeries {
                kind: SeriesType::Bar,
                data: parse_numeric_array(values),
            });
            continue;
        }
        if let Some(values) = parse_series(line, "line") {
            series.push(XyChartSeries {
                kind: SeriesType::Line,
                data: parse_numeric_array(values),
            });
        }
    }

    if y_axis.range.is_none() && !series.is_empty() {
        let mut values = series.iter().flat_map(|item| item.data.iter()).copied();
        if let Some(first) = values.next() {
            let (mut min, mut max) = (first, first);
            for value in values {
                if min.is_nan() || max.is_nan() || value.is_nan() {
                    min = f64::NAN;
                    max = f64::NAN;
                    break;
                }
                min = min.min(value);
                max = max.max(value);
            }
            let difference = max - min;
            let span = if difference == 0.0 || difference.is_nan() {
                1.0
            } else {
                difference
            };
            min -= span * 0.1;
            max += span * 0.1;
            if min > 0.0 && min < span * 0.5 {
                min = 0.0;
            }
            y_axis.range = Some(AxisRange { min, max });
        } else {
            // `Math.min(...[])` and `Math.max(...[])` in the source renderer.
            y_axis.range = Some(AxisRange {
                min: f64::INFINITY,
                max: f64::NEG_INFINITY,
            });
        }
    }
    if y_axis.range.is_none() {
        y_axis.range = Some(AxisRange {
            min: 0.0,
            max: 100.0,
        });
    }

    XyChart {
        title,
        horizontal,
        x_axis,
        y_axis,
        series,
    }
}

/// Header grammar: ASCII-case-insensitive `xychart`, optional `-beta`, then a
/// JavaScript word boundary. The optional suffix may backtrack before `-`.
fn parse_header(line: &str) -> bool {
    let mut cursor = Cursor::new(line);
    if !cursor.eat_ignore_ascii_case("xychart") {
        return false;
    }
    let after_name = cursor.pos();
    if cursor.eat_ignore_ascii_case("-beta") && cursor.at_word_boundary() {
        return true;
    }
    cursor.reset(after_name);
    cursor.at_word_boundary()
}

/// Search the header for ASCII-case-insensitive `horizontal` bounded on both
/// sides by JavaScript word boundaries.
fn contains_horizontal(line: &str) -> bool {
    let mut cursor = Cursor::new(line);
    while !cursor.at_end() {
        let start = cursor.pos();
        if cursor.at_word_boundary()
            && cursor.eat_ignore_ascii_case("horizontal")
            && cursor.at_word_boundary()
        {
            return true;
        }
        cursor.reset(start);
        cursor.bump();
    }
    false
}

/// Title grammar: `title`, whitespace, then a non-empty double-quoted body.
/// Text after the closing quote is intentionally ignored.
fn parse_title(line: &str) -> Option<&str> {
    let mut cursor = Cursor::new(line);
    if !cursor.eat("title") || !cursor.expect_ws() {
        return None;
    }
    let title = cursor.quoted()?;
    (!title.is_empty()).then_some(title)
}

/// Categorical x-axis grammar: `x-axis`, whitespace, an optional quoted title
/// followed by optional whitespace, then a non-empty bracketed value list.
fn parse_axis_categories(line: &str) -> Option<AxisCategories<'_>> {
    let mut cursor = Cursor::new(line);
    if !cursor.eat("x-axis") || !cursor.expect_ws() {
        return None;
    }
    let title = if cursor.peek() == Some('"') {
        let title = cursor.quoted()?;
        cursor.skip_ws();
        Some(title)
    } else {
        None
    };
    if !cursor.eat_char('[') {
        return None;
    }
    let values = cursor.take_until_char(']')?;
    if values.is_empty() || !cursor.eat_char(']') {
        return None;
    }
    Some(AxisCategories { title, values })
}

/// Numeric axis grammar: the axis name, whitespace, an optional quoted title
/// plus required whitespace, then two decimal numbers separated by `-->` and
/// optional surrounding whitespace.
fn parse_axis_range<'a>(line: &'a str, axis_name: &str) -> Option<AxisRangeMatch<'a>> {
    let mut cursor = Cursor::new(line);
    if !cursor.eat(axis_name) || !cursor.expect_ws() {
        return None;
    }
    let title = if cursor.peek() == Some('"') {
        let title = cursor.quoted()?;
        if !cursor.expect_ws() {
            return None;
        }
        Some(title)
    } else {
        None
    };
    let min = parse_axis_number(&mut cursor)?;
    cursor.skip_ws();
    if !cursor.eat("-->") {
        return None;
    }
    cursor.skip_ws();
    let max = parse_axis_number(&mut cursor)?;
    Some(AxisRangeMatch {
        title,
        range: AxisRange { min, max },
    })
}

/// Axis numbers are an optional minus, one or more ASCII digits, and an
/// optional fractional part containing at least one digit.
fn parse_axis_number(cursor: &mut Cursor<'_>) -> Option<f64> {
    let start = cursor.pos();
    cursor.eat_char('-');
    if cursor.take_while(|c| c.is_ascii_digit()).is_empty() {
        cursor.reset(start);
        return None;
    }
    let before_fraction = cursor.pos();
    if cursor.eat_char('.') && cursor.take_while(|c| c.is_ascii_digit()).is_empty() {
        cursor.reset(before_fraction);
    }
    cursor.since(start).parse().ok()
}

/// Y-axis title grammar: `y-axis`, whitespace, a non-empty quoted title, then
/// only whitespace through the end of the line.
fn parse_y_title(line: &str) -> Option<&str> {
    let mut cursor = Cursor::new(line);
    if !cursor.eat("y-axis") || !cursor.expect_ws() {
        return None;
    }
    let title = cursor.quoted()?;
    if title.is_empty() {
        return None;
    }
    cursor.skip_ws();
    cursor.at_end().then_some(title)
}

/// Series grammar: the series name, whitespace, then a non-empty bracketed
/// value list. Text after the closing bracket is intentionally ignored.
fn parse_series<'a>(line: &'a str, series_name: &str) -> Option<&'a str> {
    let mut cursor = Cursor::new(line);
    if !cursor.eat(series_name) || !cursor.expect_ws() || !cursor.eat_char('[') {
        return None;
    }
    let values = cursor.take_until_char(']')?;
    if values.is_empty() || !cursor.eat_char(']') {
        return None;
    }
    Some(values)
}

fn parse_numeric_array(values: &str) -> Vec<f64> {
    values.split(',').map(parse_float).collect()
}

/// JavaScript `parseFloat` prefix grammar: optional sign; either digits with
/// an optional dot and trailing digits, or a dot followed by digits; then an
/// optional complete exponent.
fn parse_float(value: &str) -> f64 {
    let value = value.trim_start_matches(|c: char| c.is_whitespace() || c == '\u{feff}');
    for (prefix, number) in [
        ("-Infinity", f64::NEG_INFINITY),
        ("+Infinity", f64::INFINITY),
        ("Infinity", f64::INFINITY),
    ] {
        if value.starts_with(prefix) {
            return number;
        }
    }

    let mut cursor = Cursor::new(value);
    let start = cursor.pos();
    if matches!(cursor.peek(), Some('+' | '-')) {
        cursor.bump();
    }
    let integer = cursor.take_while(|c| c.is_ascii_digit());
    if integer.is_empty() {
        if !cursor.eat_char('.') || cursor.take_while(|c| c.is_ascii_digit()).is_empty() {
            return f64::NAN;
        }
    } else if cursor.eat_char('.') {
        cursor.take_while(|c| c.is_ascii_digit());
    }

    let exponent = cursor.pos();
    if matches!(cursor.peek(), Some('e' | 'E')) {
        cursor.bump();
        if matches!(cursor.peek(), Some('+' | '-')) {
            cursor.bump();
        }
        if cursor.take_while(|c| c.is_ascii_digit()).is_empty() {
            cursor.reset(exponent);
        }
    }

    cursor.since(start).parse().unwrap_or(f64::NAN)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn header_and_horizontal_use_javascript_word_boundaries() {
        assert!(parse_header("XYCHART-beta horizontal"));
        assert!(parse_header("xychart-betaSuffix"));
        assert!(!parse_header("xychart_name"));
        assert!(contains_horizontal("xychart-beta HORIZONTAL!"));
        assert!(!contains_horizontal("xychart-beta ahorizontal"));
        assert!(!contains_horizontal("xychart-beta horizontal_name"));
    }

    #[test]
    fn statement_whitespace_matches_each_axis_form() {
        assert!(parse_axis_categories(r#"x-axis "Label"[A, B]"#).is_some());
        assert!(parse_axis_range(r#"x-axis "Label"0 --> 1"#, "x-axis").is_none());
        assert!(parse_axis_range(r#"x-axis "Label" 0-->1 trailing"#, "x-axis").is_some());
    }

    #[test]
    fn parse_float_scans_only_a_valid_javascript_prefix() {
        assert_eq!(parse_float("\u{feff}  +12.5e-1tail"), 1.25);
        assert_eq!(parse_float("1e"), 1.0);
        assert_eq!(parse_float(".5"), 0.5);
        assert_eq!(parse_float("-Infinity suffix"), f64::NEG_INFINITY);
        assert!(parse_float(".e2").is_nan());
        assert!(parse_float("١").is_nan());
    }
}

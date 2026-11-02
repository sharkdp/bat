//! Optional display-only alignment of top-level Markdown pipe tables.
//!
//! Keep source line counts and cell contents intact so the existing highlighter,
//! line ranges, Git decorations and wrapping continue to operate normally.

use std::collections::VecDeque;
use std::io;

use unicode_width::UnicodeWidthStr;

use crate::input::InputReader;

const MAX_TABLE_BYTES: usize = 1024 * 1024;

#[derive(Clone, Copy)]
enum Alignment {
    Left,
    Center,
    Right,
}

fn text(line: &[u8]) -> Option<(&str, &str)> {
    let line = std::str::from_utf8(line).ok()?;
    let body = line.trim_end_matches(['\r', '\n']);
    Some((body, &line[body.len()..]))
}

fn table_row(line: &[u8]) -> bool {
    let body = line.trim_ascii_start();
    line.contains(&b'|')
        && !line.starts_with(b"    ")
        && !body.starts_with(b">")
        && !body.starts_with(b"```")
        && !body.starts_with(b"~~~")
}

fn cells(line: &[u8]) -> Option<Vec<&str>> {
    let (body, _) = text(line)?;
    // Nested blocks and tab-containing rows are deliberately left untouched.
    if !table_row(line) || body.starts_with(' ') || body.chars().any(char::is_control) {
        return None;
    }
    let body = body.trim_matches(' ');
    let mut result = Vec::new();
    let mut start = 0;
    let mut escaped = false;
    let mut has_pipe = false;
    for (index, ch) in body.char_indices() {
        if ch == '|' && !escaped {
            result.push(body[start..index].trim_matches(' '));
            start = index + 1;
            has_pipe = true;
        }
        escaped = ch == '\\' && !escaped;
    }
    if !has_pipe {
        return None;
    }
    result.push(body[start..].trim_matches(' '));
    if body.starts_with('|') {
        result.remove(0);
    }
    if start == body.len() {
        result.pop();
    }
    (!result.is_empty()).then_some(result)
}

fn alignments(line: &[u8]) -> Option<Vec<Alignment>> {
    cells(line)?
        .into_iter()
        .map(|cell| {
            let middle = cell.strip_prefix(':').unwrap_or(cell);
            let middle = middle.strip_suffix(':').unwrap_or(middle);
            if middle.is_empty() || !middle.bytes().all(|ch| ch == b'-') {
                return None;
            }
            Some(match (cell.starts_with(':'), cell.ends_with(':')) {
                (true, true) => Alignment::Center,
                (false, true) => Alignment::Right,
                _ => Alignment::Left,
            })
        })
        .collect()
}

fn format_table(lines: &mut VecDeque<Vec<u8>>, alignment: &[Alignment]) {
    let rows: Option<Vec<_>> = lines.iter().map(|line| cells(line)).collect();
    let Some(rows) = rows else { return };
    // Never truncate or invent cells in irregular tables.
    if rows.iter().any(|row| row.len() != alignment.len()) {
        return;
    }
    // The source delimiter row already accounts for its alignment colons.
    let mut widths = vec![3; alignment.len()];
    for row in &rows {
        for (width, cell) in widths.iter_mut().zip(row) {
            *width = (*width).max(UnicodeWidthStr::width(*cell));
        }
    }
    // Padding sparse tables can multiply their size. Check each row's byte
    // budget before allocating it; preserve the originals if the table grows
    // too large. UTF-8 bytes and terminal columns need separate accounting.
    let row_width = 1 + widths.iter().map(|width| width + 3).sum::<usize>();
    let mut output_bytes = 0;
    let mut formatted = VecDeque::with_capacity(lines.len());
    for (index, (line, row)) in lines.iter().zip(&rows).enumerate() {
        let (_, ending) = text(line).expect("table rows are UTF-8");
        let extra_bytes = if index == 1 {
            0
        } else {
            row.iter()
                .map(|cell| cell.len() - UnicodeWidthStr::width(*cell))
                .sum()
        };
        output_bytes += row_width + extra_bytes + ending.len();
        if output_bytes > MAX_TABLE_BYTES {
            return;
        }
        let mut result = String::from("|");
        for ((cell, width), align) in row.iter().zip(&widths).zip(alignment) {
            result.push(' ');
            if index == 1 {
                let left = cell.starts_with(':');
                let right = cell.ends_with(':');
                if left {
                    result.push(':');
                }
                result.push_str(&"-".repeat(width - usize::from(left) - usize::from(right)));
                if right {
                    result.push(':');
                }
            } else {
                let padding = width - UnicodeWidthStr::width(*cell);
                let left = match align {
                    Alignment::Right => padding,
                    Alignment::Center => padding / 2,
                    Alignment::Left => 0,
                };
                result.push_str(&" ".repeat(left));
                result.push_str(cell);
                result.push_str(&" ".repeat(padding - left));
            }
            result.push_str(" |");
        }
        result.push_str(ending);
        formatted.push_back(result.into_bytes());
    }
    *lines = formatted;
}

/// Read ahead only far enough to measure a table. Ordinary lines keep streaming.
pub(crate) struct TableReader<'a, 'input> {
    reader: &'a mut InputReader<'input>,
    enabled: bool,
    pending: Option<Vec<u8>>,
    output: VecDeque<Vec<u8>>,
    fence: Option<(u8, usize)>,
    oversized: bool,
}

impl<'a, 'input> TableReader<'a, 'input> {
    pub(crate) fn new(reader: &'a mut InputReader<'input>, enabled: bool) -> Self {
        Self {
            reader,
            enabled,
            pending: None,
            output: VecDeque::new(),
            fence: None,
            oversized: false,
        }
    }

    fn raw_line(&mut self) -> io::Result<Option<Vec<u8>>> {
        if let Some(line) = self.pending.take() {
            return Ok(Some(line));
        }
        let mut line = Vec::new();
        Ok(self.reader.read_line(&mut line)?.then_some(line))
    }

    fn in_code(&mut self, line: &[u8]) -> bool {
        let Some((body, _)) = text(line) else {
            return false;
        };
        let trimmed = body.trim_start_matches(' ');
        if body.len() - trimmed.len() >= 4 {
            return true;
        }
        let marker = trimmed.as_bytes().first().copied().unwrap_or_default();
        let length = trimmed.bytes().take_while(|&ch| ch == marker).count();
        if let Some((open, minimum)) = self.fence {
            if marker == open && length >= minimum && trimmed[length..].trim().is_empty() {
                self.fence = None;
            }
            return true;
        }
        if matches!(marker, b'`' | b'~') && length >= 3 {
            self.fence = Some((marker, length));
            return true;
        }
        false
    }

    pub(crate) fn read_line(&mut self, buffer: &mut Vec<u8>) -> io::Result<bool> {
        if !self.enabled {
            return self.reader.read_line(buffer);
        }
        if let Some(line) = self.output.pop_front() {
            buffer.extend(line);
            return Ok(true);
        }
        let Some(first) = self.raw_line()? else {
            return Ok(false);
        };
        if self.in_code(&first) || cells(&first).is_none() {
            self.oversized = false;
            buffer.extend(first);
            return Ok(true);
        }
        if self.oversized {
            // Remaining rows belong to the same oversized table, not new headers.
            buffer.extend(first);
            return Ok(true);
        }
        let Some(second) = self.raw_line()? else {
            buffer.extend(first);
            return Ok(true);
        };
        let alignment = alignments(&second).filter(|alignment| {
            cells(&first).is_some_and(|header| header.len() == alignment.len())
        });
        let Some(alignment) = alignment else {
            self.pending = Some(second);
            buffer.extend(first);
            return Ok(true);
        };
        let mut size = first.len() + second.len();
        self.output.extend([first, second]);
        while size <= MAX_TABLE_BYTES {
            let Some(line) = self.raw_line()? else { break };
            // Include unsupported pipe rows in the candidate table so a tab or
            // invalid UTF-8 byte leaves the entire table unchanged.
            if !table_row(&line) {
                self.pending = Some(line);
                break;
            }
            size += line.len();
            self.output.push_back(line);
        }
        self.oversized = size > MAX_TABLE_BYTES;
        if !self.oversized {
            format_table(&mut self.output, &alignment);
        }
        buffer.extend(self.output.pop_front().expect("table contains a header"));
        Ok(true)
    }
}

//! Underline recognizable literal paths that exist on the local filesystem.
use std::collections::HashMap;
use std::ops::Range;
use std::path::{Path, PathBuf};

use syntect::highlighting::{FontStyle, Style};

pub(crate) struct PathAnnotations {
    base: PathBuf,
    home: Option<PathBuf>,
    exists: HashMap<PathBuf, bool>,
}

impl PathAnnotations {
    pub(crate) fn new(base: PathBuf) -> Self {
        Self {
            base,
            home: std::env::var_os(if cfg!(windows) { "USERPROFILE" } else { "HOME" })
                .map(PathBuf::from),
            exists: HashMap::new(),
        }
    }

    fn resolve(&self, text: &str, quoted: bool) -> Option<PathBuf> {
        // URLs and shell expansion expressions are not filesystem locations.
        // Inspect literal spellings only; never evaluate escapes or variables.
        if text.is_empty() || text.contains("://") || text.contains(['$', '`', '\0', '\r', '\n']) {
            return None;
        }
        let drive =
            text.as_bytes().get(1) == Some(&b':') && text.as_bytes()[0].is_ascii_alphabetic();
        if drive && !cfg!(windows) {
            return None;
        }
        if !(text.contains('/')
            || (cfg!(windows) && text.contains('\\'))
            || (quoted && text.contains('.')))
        {
            return None;
        }
        let path = if let Some(relative) = text.strip_prefix("~/").or_else(|| {
            if cfg!(windows) {
                text.strip_prefix("~\\")
            } else {
                None
            }
        }) {
            self.home.as_ref()?.join(relative)
        } else {
            PathBuf::from(text)
        };
        Some(if path.is_absolute() {
            path
        } else {
            self.base.join(path)
        })
    }

    fn ranges(&mut self, line: &str) -> Vec<Range<usize>> {
        let mut ranges = Vec::new();
        let mut chars = line.char_indices().peekable();
        while let Some((start, character)) = chars.next() {
            if character == '\'' || character == '"' {
                let begin = start + character.len_utf8();
                let mut end = None;
                for (index, next) in chars.by_ref() {
                    if next == character {
                        end = Some(index);
                        break;
                    }
                    if next == '\r' || next == '\n' {
                        break;
                    }
                }
                if let Some(end) = end {
                    self.add_if_present(line, begin..end, true, &mut ranges);
                }
            } else if !delimiter(character) {
                let mut end = start + character.len_utf8();
                while let Some(&(index, next)) = chars.peek() {
                    if delimiter(next) {
                        break;
                    }
                    chars.next();
                    end = index + next.len_utf8();
                }
                self.add_if_present(line, start..end, false, &mut ranges);
            }
        }
        ranges
    }

    fn add_if_present(
        &mut self,
        line: &str,
        range: Range<usize>,
        quoted: bool,
        ranges: &mut Vec<Range<usize>>,
    ) {
        let Some(path) = self.resolve(&line[range.clone()], quoted) else {
            return;
        };
        let present = if let Some(&present) = self.exists.get(&path) {
            present
        } else {
            let present = std::fs::metadata(&path).is_ok();
            // Limit retained paths when processing long streams with many unique candidates.
            if self.exists.len() >= 4096 {
                self.exists.clear();
            }
            self.exists.insert(path, present);
            present
        };
        if present {
            ranges.push(range);
        }
    }

    pub(crate) fn highlight<'a>(
        &mut self,
        line: &'a str,
        regions: Vec<(Style, &'a str)>,
    ) -> Vec<(Style, &'a str)> {
        let ranges = self.ranges(line);
        if ranges.is_empty() {
            return regions;
        }
        let mut output = Vec::new();
        let mut offset = 0;
        let mut first_range = 0;
        for (style, text) in regions {
            let end = offset + text.len();
            let mut cursor = offset;
            while first_range < ranges.len() && ranges[first_range].end <= cursor {
                first_range += 1;
            }
            for range in ranges[first_range..]
                .iter()
                .take_while(|range| range.start < end)
            {
                let lo = range.start.max(cursor);
                let hi = range.end.min(end);
                if lo >= hi {
                    continue;
                }
                if cursor < lo {
                    output.push((style, &line[cursor..lo]));
                }
                let mut marked = style;
                marked.font_style.insert(FontStyle::UNDERLINE);
                output.push((marked, &line[lo..hi]));
                cursor = hi;
            }
            if cursor < end {
                output.push((style, &line[cursor..end]));
            }
            offset = end;
        }
        output
    }
}

fn delimiter(character: char) -> bool {
    character.is_whitespace()
        || matches!(
            character,
            '\'' | '"' | '(' | ')' | '{' | '}' | '[' | ']' | '<' | '>' | '=' | ',' | ';'
        )
}

pub(crate) fn base_for_file(path: &Path) -> PathBuf {
    path.parent()
        .filter(|parent| !parent.as_os_str().is_empty())
        .unwrap_or_else(|| Path::new("."))
        .to_owned()
}

#[cfg(test)]
mod tests {
    use super::*;
    use syntect::highlighting::Color;
    use tempfile::tempdir;

    #[test]
    fn annotations_preserve_existing_colors_and_font_attributes() {
        let dir = tempdir().unwrap();
        std::fs::write(dir.path().join("present.txt"), "exists").unwrap();
        let line = "\"./present.txt\"";
        let ordinary = Style::default();
        let special = Style {
            foreground: Color {
                r: 255,
                g: 0,
                b: 0,
                a: 255,
            },
            font_style: FontStyle::BOLD | FontStyle::ITALIC,
            ..ordinary
        };
        let original = vec![
            (ordinary, &line[..3]),
            (special, &line[3..7]),
            (ordinary, &line[7..]),
        ];
        let styles: Vec<_> = original
            .iter()
            .flat_map(|(style, text)| std::iter::repeat_n(*style, text.len()))
            .collect();
        let output = PathAnnotations::new(dir.path().to_owned()).highlight(line, original);
        assert_eq!(
            output.iter().map(|(_, text)| *text).collect::<String>(),
            line
        );
        let mut offset = 0;
        for (style, text) in output {
            for (index, original_style) in styles.iter().enumerate().skip(offset).take(text.len()) {
                let mut expected = *original_style;
                if index > 0 && index < line.len() - 1 {
                    expected.font_style.insert(FontStyle::UNDERLINE);
                }
                assert_eq!(style, expected);
            }
            offset += text.len();
        }
    }

    #[test]
    fn tilde_paths_use_home_without_evaluating_other_expressions() {
        let dir = tempdir().unwrap();
        std::fs::write(dir.path().join("present.txt"), "exists").unwrap();
        let mut paths = PathAnnotations {
            base: PathBuf::from("unused"),
            home: Some(dir.path().to_owned()),
            exists: HashMap::new(),
        };
        let line = "~/present.txt $HOME/present.txt `pwd`/present.txt";
        let ranges = paths.ranges(line);
        assert_eq!(
            ranges
                .iter()
                .map(|range| &line[range.clone()])
                .collect::<Vec<_>>(),
            ["~/present.txt"]
        );
    }
}

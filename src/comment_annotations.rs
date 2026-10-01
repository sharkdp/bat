//! Optional annotations derived from syntax comment scopes.
use std::ops::Range;

use syntect::easy::HighlightLines;
use syntect::highlighting::{
    Color, FontStyle, HighlightIterator, HighlightState, Highlighter, Style, Theme,
};
use syntect::parsing::{ParseState, Scope, ScopeStack, SyntaxReference, SyntaxSet};
use unicode_segmentation::UnicodeSegmentation;

pub(crate) enum LineHighlighter<'a> {
    Standard(HighlightLines<'a>),
    Annotated(CommentHighlighter<'a>),
}

impl<'a> LineHighlighter<'a> {
    pub(crate) fn new(syntax: &SyntaxReference, theme: &'a Theme, annotate: bool) -> Self {
        if annotate {
            Self::Annotated(CommentHighlighter::new(syntax, theme))
        } else {
            Self::Standard(HighlightLines::new(syntax, theme))
        }
    }

    pub(crate) fn highlight_line<'b>(
        &mut self,
        line: &'b str,
        set: &SyntaxSet,
    ) -> Result<Vec<(Style, &'b str)>, syntect::Error> {
        match self {
            Self::Standard(highlighter) => highlighter.highlight_line(line, set),
            Self::Annotated(highlighter) => highlighter.highlight_line(line, set),
        }
    }
}

pub(crate) struct CommentHighlighter<'a> {
    highlighter: Highlighter<'a>,
    parse_state: ParseState,
    highlight_state: HighlightState,
    comment_stack: ScopeStack,
    comment_scope: Scope,
    color: Color,
}

impl<'a> CommentHighlighter<'a> {
    fn new(syntax: &SyntaxReference, theme: &'a Theme) -> Self {
        let highlighter = Highlighter::new(theme);
        let highlight_state = HighlightState::new(&highlighter, ScopeStack::new());
        let background = theme.settings.background.unwrap_or(Color {
            r: 0,
            g: 0,
            b: 0,
            a: 1,
        });
        let color = if background.a <= 1 {
            // Palette-based themes should retain the terminal's yellow instead
            // of introducing a fixed RGB color into an otherwise adaptive theme.
            Color {
                r: 3,
                g: 0,
                b: 0,
                a: 0,
            }
        } else if u32::from(background.r) * 299
            + u32::from(background.g) * 587
            + u32::from(background.b) * 114
            > 128_000
        {
            Color {
                r: 153,
                g: 76,
                b: 0,
                a: 255,
            }
        } else {
            Color {
                r: 255,
                g: 175,
                b: 0,
                a: 255,
            }
        };
        Self {
            highlighter,
            parse_state: ParseState::new(syntax),
            highlight_state,
            comment_stack: ScopeStack::new(),
            comment_scope: Scope::new("comment").unwrap(),
            color,
        }
    }

    fn highlight_line<'b>(
        &mut self,
        line: &'b str,
        set: &SyntaxSet,
    ) -> Result<Vec<(Style, &'b str)>, syntect::Error> {
        let operations = self.parse_state.parse_line(line, set)?;
        let mut comments: Vec<Range<usize>> = Vec::new();
        let mut start = 0;
        for (end, operation) in &operations {
            self.add_comment_range(start..*end, &mut comments);
            self.comment_stack.apply(operation)?;
            start = *end;
        }
        self.add_comment_range(start..line.len(), &mut comments);
        let regions: Vec<_> = HighlightIterator::new(
            &mut self.highlight_state,
            &operations,
            line,
            &self.highlighter,
        )
        .collect();
        let annotations: Vec<_> = comments
            .into_iter()
            .filter_map(|range| {
                let text = line[range.clone()]
                    .split(['\r', '\n'])
                    .next()
                    .unwrap_or_default();
                text.split_word_bound_indices()
                    .find(|(_, word)| {
                        ["TODO", "TODOS", "FIXME", "FIXMES"]
                            .iter()
                            .any(|marker| word.eq_ignore_ascii_case(marker))
                    })
                    .map(|(start, _)| range.start + start..range.start + text.len())
            })
            .collect();
        if annotations.is_empty() {
            return Ok(regions);
        }
        let mut output = Vec::new();
        let mut offset = 0;
        for (style, region) in regions {
            let end = offset + region.len();
            let mut cursor = offset;
            for annotation in &annotations {
                let lo = annotation.start.max(cursor);
                let hi = annotation.end.min(end);
                if lo >= hi {
                    continue;
                }
                if cursor < lo {
                    output.push((style, &line[cursor..lo]));
                }
                let mut annotated = style;
                annotated.foreground = self.color;
                annotated.font_style.insert(FontStyle::BOLD);
                output.push((annotated, &line[lo..hi]));
                cursor = hi;
            }
            if cursor < end {
                output.push((style, &line[cursor..end]));
            }
            offset = end;
        }
        Ok(output)
    }

    fn add_comment_range(&self, range: Range<usize>, ranges: &mut Vec<Range<usize>>) {
        if range.is_empty()
            || !self
                .comment_stack
                .as_slice()
                .iter()
                .any(|scope| self.comment_scope.is_prefix_of(*scope))
        {
            return;
        }
        if let Some(previous) = ranges
            .last_mut()
            .filter(|previous| previous.end == range.start)
        {
            previous.end = range.end;
        } else {
            ranges.push(range);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::assets::HighlightingAssets;

    fn changes(language: &str, source: &str, theme_name: &str) -> String {
        let assets = HighlightingAssets::from_binary();
        let syntaxes = assets.get_syntax_set().unwrap();
        let syntax = syntaxes
            .find_syntax_by_name(language)
            .unwrap_or_else(|| panic!("missing syntax {language}"));
        let theme = assets.get_theme(theme_name);
        let mut standard = LineHighlighter::new(syntax, theme, false);
        let mut annotated = LineHighlighter::new(syntax, theme, true);
        let mut changed = String::new();
        for line in source.split_inclusive('\n') {
            let plain = standard.highlight_line(line, syntaxes).unwrap();
            let highlighted = annotated.highlight_line(line, syntaxes).unwrap();
            assert_eq!(
                highlighted
                    .iter()
                    .map(|(_, text)| *text)
                    .collect::<String>(),
                line
            );
            let styles: Vec<_> = plain
                .iter()
                .flat_map(|(style, text)| std::iter::repeat_n(*style, text.len()))
                .collect();
            let mut offset = 0;
            for (style, text) in highlighted {
                for (index, character) in text.char_indices() {
                    if style != styles[offset + index] {
                        assert!(style.font_style.contains(FontStyle::BOLD));
                        changed.push(character);
                    }
                }
                offset += text.len();
            }
        }
        changed
    }

    #[test]
    fn comments_are_annotated_in_multiple_languages() {
        for (language, source, expected) in [
            (
                "Rust",
                "let text = \"TODO fake\"; // TODO real\n",
                "TODO real",
            ),
            ("Python", "text = 'TODO fake' # todo real\n", "todo real"),
            (
                "Bourne Again Shell (bash)",
                "echo 'TODO fake' # FIXMES real\n",
                "FIXMES real",
            ),
            (
                "JavaScript (Babel)",
                "let text = 'TODO fake'; // fixme real\n",
                "fixme real",
            ),
            ("CSS", "a { /* TODO real */ color: red; }\n", "TODO real */"),
            ("SQL", "SELECT 'TODO fake'; -- TODO real\n", "TODO real"),
            (
                "HTML",
                "<p>TODO fake</p><!-- TODO real -->\n",
                "TODO real -->",
            ),
        ] {
            assert_eq!(
                changes(language, source, "Monokai Extended"),
                expected,
                "{language}"
            );
        }
    }

    #[test]
    fn multiline_comment_state_continues_but_the_annotation_does_not() {
        let source = "/* ordinary\n * TODO first\n * ordinary next line\n * fixmes second\n */ let TODO = \"FIXME\";\n";
        assert_eq!(
            changes("Rust", source, "Monokai Extended"),
            "TODO firstfixmes second"
        );
    }

    #[test]
    fn markers_use_word_boundaries_and_do_not_affect_other_comments() {
        let source = "// TODOISH MYTODO FIXMESUFFIX éTODO\n/* TODO one */ let x = \"TODO\"; /* ordinary */ // todos two\n";
        assert_eq!(
            changes("Rust", source, "Monokai Extended"),
            "TODO one */todos two"
        );
    }

    #[test]
    fn embedded_language_comments_and_unicode_are_supported() {
        let source = "```rust\nlet text = \"TODO fake\"; // TODO 界🙂\n```\nTODO prose\n";
        assert_eq!(changes("Markdown", source, "Monokai Extended"), "TODO 界🙂");
    }

    #[test]
    fn line_endings_and_unterminated_last_lines_are_preserved() {
        for source in ["// TODO one\r\n// FIXME two", "// TODO one\n// FIXME two"] {
            assert_eq!(
                changes("Rust", source, "Monokai Extended"),
                "TODO oneFIXME two"
            );
        }
    }

    #[test]
    fn carriage_return_ends_an_annotation() {
        assert_eq!(
            changes("Rust", "// TODO one\rnext", "Monokai Extended"),
            "TODO one"
        );
    }

    #[test]
    fn strings_and_plain_text_do_not_gain_comment_annotations() {
        for (language, source) in [
            ("Rust", "let TODO = r#\"/* TODO */\"#;\n"),
            ("Python", "text = '''TODO\nFIXME'''\n"),
            ("Plain Text", "// TODO fake\n# FIXME fake\n"),
        ] {
            assert_eq!(
                changes(language, source, "Monokai Extended"),
                "",
                "{language}"
            );
        }
    }

    #[test]
    fn light_and_palette_themes_keep_the_same_annotation_bounds() {
        for theme in ["GitHub", "ansi", "base16", "1337"] {
            assert_eq!(
                changes("Rust", "// TODO text\n", theme),
                "TODO text",
                "{theme}"
            );
        }
    }
}

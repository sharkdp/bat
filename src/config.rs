use crate::line_range::{HighlightedLineRanges, LineRanges};
#[cfg(feature = "paging")]
use crate::paging::PagingMode;
use crate::style::StyleComponents;
use crate::syntax_mapping::SyntaxMapping;
use crate::wrapping::WrappingMode;

#[derive(Debug, Clone)]
pub enum VisibleLines {
    /// Show all lines which are included in the line ranges
    Ranges(LineRanges),

    #[cfg(feature = "git")]
    /// Only show lines surrounding added/deleted/modified lines
    DiffContext(usize),
}

impl VisibleLines {
    pub fn diff_mode(&self) -> bool {
        match self {
            Self::Ranges(_) => false,
            #[cfg(feature = "git")]
            Self::DiffContext(_) => true,
        }
    }
}

impl Default for VisibleLines {
    fn default() -> Self {
        VisibleLines::Ranges(LineRanges::default())
    }
}

#[derive(Debug, Clone, Default)]
pub struct Config<'a> {
    /// The explicitly configured language, if any
    pub language: Option<&'a str>,

    /// Whether or not to show/replace non-printable characters like space, tab and newline.
    pub show_nonprintable: bool,

    /// The character width of the terminal
    pub term_width: usize,

    /// The width of tab characters.
    /// Currently, a value of 0 will cause tabs to be passed through without expanding them.
    pub tab_width: usize,

    /// Whether or not to simply loop through all input (`cat` mode)
    pub loop_through: bool,

    /// Whether or not the output should be colorized
    pub colored_output: bool,

    /// Whether or not the output terminal supports true color
    pub true_color: bool,

    /// Style elements (grid, line numbers, ...)
    pub style_components: StyleComponents,

    /// If and how text should be wrapped
    pub wrapping_mode: WrappingMode,

    /// Pager or STDOUT
    #[cfg(feature = "paging")]
    pub paging_mode: PagingMode,

    /// Specifies which lines should be printed
    pub visible_lines: VisibleLines,

    /// The syntax highlighting theme
    pub theme: String,

    /// File extension/name mappings
    pub syntax_mapping: SyntaxMapping<'a>,

    /// Command to start the pager
    pub pager: Option<&'a str>,

    /// Whether or not to use ANSI italics
    pub use_italic_text: bool,

    /// Ranges of lines which should be highlighted with a special background color
    pub highlighted_lines: HighlightedLineRanges,
}

#[test]
fn default_config_should_include_all_lines() {
    use crate::line_range::RangeCheckResult;

    assert_eq!(LineRanges::default().check(17), RangeCheckResult::InRange);
}

#[test]
fn default_config_should_highlight_no_lines() {
    use crate::line_range::RangeCheckResult;

    assert_ne!(
        Config::default().highlighted_lines.0.check(17),
        RangeCheckResult::InRange
    );
}

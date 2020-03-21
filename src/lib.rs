// `error_chain!` can recurse deeply
#![recursion_limit = "1024"]

extern crate ansi_term;
extern crate atty;
extern crate console;
extern crate content_inspector;
extern crate dirs as dirs_rs;
extern crate encoding;
extern crate git2;
extern crate shell_words;
extern crate syntect;
extern crate wild;

pub mod assets;
pub mod controller;
mod decorations;
mod diff;
pub mod errors;
pub mod inputfile;
mod less;
pub mod line_range;
mod output;
mod preprocessor;
mod printer;
pub mod style;
pub mod syntax_mapping;
mod terminal;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum PagingMode {
    Always,
    QuitIfOneScreen,
    Never,
}

impl Default for PagingMode {
    fn default() -> Self {
        PagingMode::Never
    }
}

use inputfile::InputFile;
use line_range::{HighlightedLineRanges, LineRanges};
use style::{OutputComponents, OutputWrap};
use syntax_mapping::SyntaxMapping;

#[derive(Debug, Clone, Default)]
pub struct Config<'a> {
    /// List of files to print
    pub files: Vec<InputFile<'a>>,

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
    pub output_components: OutputComponents,

    /// Text wrapping mode
    pub output_wrap: OutputWrap,

    /// Pager or STDOUT
    pub paging_mode: PagingMode,

    /// Specifies the lines that should be printed
    pub line_ranges: LineRanges,

    /// The syntax highlighting theme
    pub theme: String,

    /// File extension/name mappings
    pub syntax_mapping: SyntaxMapping,

    /// Command to start the pager
    pub pager: Option<&'a str>,

    /// Whether or not to use ANSI italics
    pub use_italic_text: bool,

    /// Ranges of lines which should be highlighted with a special background color
    pub highlighted_lines: HighlightedLineRanges,
}

#[test]
fn default_config_should_include_all_lines() {
    use line_range::RangeCheckResult;

    assert_eq!(
        Config::default().line_ranges.check(17),
        RangeCheckResult::InRange
    );
}

#[test]
fn default_config_should_highlight_no_lines() {
    use line_range::RangeCheckResult;

    assert_ne!(
        Config::default().highlighted_lines.0.check(17),
        RangeCheckResult::InRange
    );
}

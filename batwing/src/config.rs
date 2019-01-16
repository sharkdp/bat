use std::env;
use std::ffi::OsString;
use std::fs;
use std::path::PathBuf;

use shell_words;

use dirs::PROJECT_DIRS;
use util::transpose;
use inputfile::InputFile;
use line_range::LineRanges;
use style::{OutputComponents, OutputWrap};
use syntax_mapping::SyntaxMapping;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum PagingMode {
    Always,
    QuitIfOneScreen,
    Never,
}

#[derive(Clone)]
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

    /// Lines to highlight
    pub highlight_lines: Vec<usize>,
}

pub fn config_file() -> PathBuf {
    env::var("BAT_CONFIG_PATH")
        .ok()
        .map(PathBuf::from)
        .filter(|config_path| config_path.is_file())
        .unwrap_or(PROJECT_DIRS.config_dir().join("config"))
}

pub fn get_args_from_config_file() -> Result<Vec<OsString>, shell_words::ParseError> {
    Ok(transpose(
        fs::read_to_string(config_file())
            .ok()
            .map(|content| get_args_from_str(&content)),
    )?
    .unwrap_or(vec![]))
}

pub fn get_args_from_env_var() -> Option<Result<Vec<OsString>, shell_words::ParseError>> {
    env::var("BAT_OPTS").ok().map(|s| get_args_from_str(&s))
}

fn get_args_from_str<'a>(content: &'a str) -> Result<Vec<OsString>, shell_words::ParseError> {
    let args_per_line = content
        .split('\n')
        .map(|line| line.trim())
        .filter(|line| !line.is_empty())
        .filter(|line| !line.starts_with("#"))
        .map(|line| shell_words::split(line))
        .collect::<Result<Vec<_>, _>>()?;

    Ok(args_per_line
        .iter()
        .flatten()
        .map(|line| line.into())
        .collect())
}

#[test]
fn empty() {
    let args = get_args_from_str("").unwrap();
    assert!(args.is_empty());
}

#[test]
fn single() {
    assert_eq!(vec!["--plain"], get_args_from_str("--plain").unwrap());
}

#[test]
fn multiple() {
    assert_eq!(
        vec!["--plain", "--language=cpp"],
        get_args_from_str("--plain --language=cpp").unwrap()
    );
}

#[test]
fn quotes() {
    assert_eq!(
        vec!["--theme", "Sublime Snazzy"],
        get_args_from_str("--theme \"Sublime Snazzy\"").unwrap()
    );
}

#[test]
fn multi_line() {
    let config = "
    -p
    --style numbers,changes

    --color=always
    ";
    assert_eq!(
        vec!["-p", "--style", "numbers,changes", "--color=always"],
        get_args_from_str(config).unwrap()
    );
}

#[test]
fn comments() {
    let config = "
    # plain style
    -p

    # show line numbers and Git modifications
    --style numbers,changes

    # Always show ANSI colors
    --color=always
    ";
    assert_eq!(
        vec!["-p", "--style", "numbers,changes", "--color=always"],
        get_args_from_str(config).unwrap()
    );
}

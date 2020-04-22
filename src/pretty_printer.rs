use std::ffi::OsStr;
use std::io::Read;

use crate::{
    assets::HighlightingAssets, config::Config, controller::Controller, errors::Result,
    input::Input, HighlightedLineRanges, LineRanges, StyleComponents, SyntaxMapping, WrappingMode,
};

#[cfg(feature = "paging")]
use crate::config::PagingMode;

pub struct PrettyPrinter<'a> {
    inputs: Vec<Input<'a>>,
    config: Config<'a>,
    assets: HighlightingAssets,
}

impl<'a> PrettyPrinter<'a> {
    pub fn new() -> Self {
        let mut config = Config::default();

        config.colored_output = true;
        config.true_color = true;

        PrettyPrinter {
            inputs: vec![],
            config,
            assets: HighlightingAssets::from_binary(),
        }
    }

    /// Add a file which should be pretty-printed
    pub fn input_file(&mut self, path: &OsStr) -> &mut Self {
        self.inputs.push(Input::ordinary_file(path));
        self
    }

    /// Add multiple files which should be pretty-printed
    pub fn input_files<I, P>(&mut self, paths: I) -> &mut Self
    where
        I: IntoIterator<Item = P>,
        P: AsRef<OsStr>,
    {
        for path in paths {
            self.inputs.push(Input::ordinary_file(path.as_ref()));
        }
        self
    }

    /// Add STDIN as an input
    pub fn input_stdin(&mut self) -> &mut Self {
        self.inputs.push(Input::stdin());
        self
    }

    /// Use a string as an input
    pub fn input_from_bytes(&mut self, content: &'a [u8]) -> &mut Self {
        self.input_from_reader(content)
    }

    /// Add a custom reader as an input
    pub fn input_from_reader<R: Read + 'a>(&mut self, reader: R) -> &mut Self {
        self.inputs.push(Input::from_reader(Box::new(reader)));
        self
    }

    /// Specify the syntax file which should be used (default: auto-detect)
    pub fn language(&mut self, language: &'a str) -> &mut Self {
        self.config.language = Some(language);
        self
    }

    /// The character width of the terminal (default: unlimited)
    pub fn term_width(&mut self, width: usize) -> &mut Self {
        self.config.term_width = width;
        self
    }

    /// The width of tab characters (default: None - do not turn tabs to spaces)
    pub fn tab_width(&mut self, tab_width: Option<usize>) -> &mut Self {
        self.config.tab_width = tab_width.unwrap_or(0);
        self
    }

    /// Whether or not the output should be colorized (default: true)
    pub fn colored_output(&mut self, yes: bool) -> &mut Self {
        self.config.colored_output = yes;
        self
    }

    /// Whether or not to output 24bit colors (default: true)
    pub fn true_color(&mut self, yes: bool) -> &mut Self {
        self.config.true_color = yes;
        self
    }

    /// Configure style elements like grid or  line numbers (default: "full" style)
    pub fn style_components(&mut self, components: StyleComponents) -> &mut Self {
        self.config.style_components = components;
        self
    }

    /// Text wrapping mode (default: do not wrap)
    pub fn wrapping_mode(&mut self, mode: WrappingMode) -> &mut Self {
        self.config.wrapping_mode = mode;
        self
    }

    /// Whether or not to use ANSI italics (default: off)
    pub fn use_italics(&mut self, yes: bool) -> &mut Self {
        self.config.use_italic_text = yes;
        self
    }

    /// If and how to use a pager (default: no paging)
    #[cfg(feature = "paging")]
    pub fn paging_mode(&mut self, mode: PagingMode) -> &mut Self {
        self.config.paging_mode = mode;
        self
    }

    /// Specify the command to start the pager (default: use "less")
    #[cfg(feature = "paging")]
    pub fn pager(&mut self, cmd: &'a str) -> &mut Self {
        self.config.pager = Some(cmd);
        self
    }

    /// Specify the lines that should be printed (default: all)
    pub fn line_ranges(&mut self, ranges: LineRanges) -> &mut Self {
        self.config.line_ranges = ranges;
        self
    }

    /// Specify which lines should be highlighted (default: none)
    pub fn highlighted_lines(&mut self, ranges: HighlightedLineRanges) -> &mut Self {
        self.config.highlighted_lines = ranges;
        self
    }

    /// Specify the highlighting theme
    pub fn theme(&mut self, theme: impl AsRef<str>) -> &mut Self {
        self.config.theme = theme.as_ref().to_owned();
        self
    }

    /// Specify custom file extension / file name to syntax mappings
    pub fn syntax_mapping(&mut self, mapping: SyntaxMapping<'a>) -> &mut Self {
        self.config.syntax_mapping = mapping;
        self
    }

    /// Pretty-print all specified inputs. This method will drain all stored inputs.
    /// If you want to call 'run' multiple times, you have to call the appropriate
    /// input_* methods again.
    pub fn run(&mut self) -> Result<bool> {
        let mut inputs: Vec<Input> = vec![];
        std::mem::swap(&mut inputs, &mut self.inputs);

        let controller = Controller::new(&self.config, &self.assets);
        controller.run(inputs)
    }
}

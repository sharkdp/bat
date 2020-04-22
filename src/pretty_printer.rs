use std::ffi::OsStr;
use std::io::Read;

use console::Term;

use crate::{
    assets::HighlightingAssets,
    config::Config,
    controller::Controller,
    errors::Result,
    input::Input,
    line_range::{HighlightedLineRanges, LineRanges},
    style::{StyleComponent, StyleComponents},
    LineRange, SyntaxMapping, WrappingMode,
};

#[cfg(feature = "paging")]
use crate::config::PagingMode;

#[derive(Default)]
struct ActiveStyleComponents {
    header: bool,
    vcs_modification_markers: bool,
    grid: bool,
    line_numbers: bool,
    snip: bool,
}

pub struct PrettyPrinter<'a> {
    inputs: Vec<Input<'a>>,
    config: Config<'a>,
    assets: HighlightingAssets,

    highlighted_lines: Vec<LineRange>,
    term_width: Option<usize>,
    active_style_components: ActiveStyleComponents,
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

            highlighted_lines: vec![],
            term_width: None,
            active_style_components: ActiveStyleComponents::default(),
        }
    }

    /// Add a file which should be pretty-printed
    pub fn input_file(&mut self, path: impl AsRef<OsStr>) -> &mut Self {
        self.inputs.push(Input::ordinary_file(path.as_ref()));
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

    /// The character width of the terminal (default: autodetect)
    pub fn term_width(&mut self, width: usize) -> &mut Self {
        self.term_width = Some(width);
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

    /// Whether to show a header with the file name
    pub fn header(&mut self, yes: bool) -> &mut Self {
        self.active_style_components.header = yes;
        self
    }

    /// Whether to show line numbers
    pub fn line_numbers(&mut self, yes: bool) -> &mut Self {
        self.active_style_components.line_numbers = yes;
        self
    }

    /// Whether to paint a grid, separating line numbers, git changes and the code
    pub fn grid(&mut self, yes: bool) -> &mut Self {
        self.active_style_components.grid = yes;
        self
    }

    /// Whether to show modification markers for VCS changes
    pub fn vcs_modification_markers(&mut self, yes: bool) -> &mut Self {
        self.active_style_components.vcs_modification_markers = yes;
        self
    }

    /// Whether to show "snip" markers between visible line ranges (default: no)
    pub fn snip(&mut self, yes: bool) -> &mut Self {
        self.active_style_components.snip = yes;
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

    /// Specify a range of lines that should be highlighted (default: none).
    /// This can be called multiple times to highlight more than one range
    /// of lines.
    pub fn highlight(&mut self, range: LineRange) -> &mut Self {
        self.highlighted_lines.push(range);
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

    /// Pretty-print all specified inputs. This method will "use" all stored inputs.
    /// If you want to call 'print' multiple times, you have to call the appropriate
    /// input_* methods again.
    pub fn print(&mut self) -> Result<bool> {
        self.config.highlighted_lines =
            HighlightedLineRanges(LineRanges::from(self.highlighted_lines.clone()));
        self.config.term_width = self
            .term_width
            .unwrap_or_else(|| Term::stdout().size().1 as usize);

        let mut style_components = vec![];
        if self.active_style_components.grid {
            style_components.push(StyleComponent::Grid);
        }
        if self.active_style_components.header {
            style_components.push(StyleComponent::Header);
        }
        if self.active_style_components.line_numbers {
            style_components.push(StyleComponent::LineNumbers);
        }
        if self.active_style_components.snip {
            style_components.push(StyleComponent::Snip);
        }
        if self.active_style_components.vcs_modification_markers {
            style_components.push(StyleComponent::Changes);
        }
        self.config.style_components = StyleComponents::new(&style_components);

        let mut inputs: Vec<Input> = vec![];
        std::mem::swap(&mut inputs, &mut self.inputs);

        let controller = Controller::new(&self.config, &self.assets);
        controller.run(inputs)
    }
}

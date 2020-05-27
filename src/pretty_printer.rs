use std::ffi::OsStr;
use std::io::Read;

use console::Term;
use syntect::parsing::SyntaxReference;

use crate::{
    assets::HighlightingAssets,
    config::{Config, VisibleLines},
    controller::Controller,
    error::Result,
    input,
    line_range::{HighlightedLineRanges, LineRange, LineRanges},
    style::{StyleComponent, StyleComponents},
    SyntaxMapping, WrappingMode,
};

#[cfg(feature = "paging")]
use crate::paging::PagingMode;

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

    /// Add an input which should be pretty-printed
    pub fn input(&mut self, input: Input<'a>) -> &mut Self {
        self.inputs.push(input);
        self
    }

    /// Adds multiple inputs which should be pretty-printed
    pub fn inputs(&mut self, inputs: impl IntoIterator<Item = Input<'a>>) -> &mut Self {
        for input in inputs {
            self.inputs.push(input);
        }
        self
    }

    /// Add a file which should be pretty-printed
    pub fn input_file(&mut self, path: impl AsRef<OsStr>) -> &mut Self {
        self.input(Input::from_file(path).kind("File"))
    }

    /// Add multiple files which should be pretty-printed
    pub fn input_files<I, P>(&mut self, paths: I) -> &mut Self
    where
        I: IntoIterator<Item = P>,
        P: AsRef<OsStr>,
    {
        self.inputs(paths.into_iter().map(Input::from_file))
    }

    /// Add STDIN as an input
    pub fn input_stdin(&mut self) -> &mut Self {
        self.inputs.push(Input::from_stdin());
        self
    }

    /// Add STDIN as an input (with customized name)
    #[deprecated]
    pub fn input_stdin_with_name(&mut self, name: impl AsRef<OsStr>) -> &mut Self {
        self.inputs
            .push(Input::from_stdin().name(name).kind("File"));
        self
    }

    /// Add a byte string as an input
    pub fn input_from_bytes(&mut self, content: &'a [u8]) -> &mut Self {
        self.input_from_reader(content)
    }

    /// Add a byte string as an input (with customized name)
    #[deprecated]
    #[allow(deprecated)]
    pub fn input_from_bytes_with_name(
        &mut self,
        content: &'a [u8],
        name: impl AsRef<OsStr>,
    ) -> &mut Self {
        self.input_from_reader_with_name(content, name)
    }

    /// Add a custom reader as an input
    pub fn input_from_reader<R: Read + 'a>(&mut self, reader: R) -> &mut Self {
        self.inputs.push(Input::from_reader(reader));
        self
    }

    /// Add a custom reader as an input (with customized name)
    #[deprecated]
    pub fn input_from_reader_with_name<R: Read + 'a>(
        &mut self,
        reader: R,
        name: impl AsRef<OsStr>,
    ) -> &mut Self {
        self.inputs
            .push(Input::from_reader(reader).name(name).kind("File"));
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

    /// Whether to show modification markers for VCS changes. This has no effect if
    /// the `git` feature is not activated.
    #[cfg_attr(
        not(feature = "git"),
        deprecated(
            note = "Using vcs_modification_markers without the 'git' feature has no effect. \
                    The function will be removed (for non-'git' use cases) in the future."
        )
    )]
    #[allow(unused_variables)]
    pub fn vcs_modification_markers(&mut self, yes: bool) -> &mut Self {
        #[cfg(feature = "git")]
        {
            self.active_style_components.vcs_modification_markers = yes;
        }
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
        self.config.visible_lines = VisibleLines::Ranges(ranges);
        self
    }

    /// Specify a line that should be highlighted (default: none).
    /// This can be called multiple times to highlight more than one
    /// line. See also: highlight_range.
    pub fn highlight(&mut self, line: usize) -> &mut Self {
        self.highlighted_lines.push(LineRange::new(line, line));
        self
    }

    /// Specify a range of lines that should be highlighted (default: none).
    /// This can be called multiple times to highlight more than one range
    /// of lines.
    pub fn highlight_range(&mut self, from: usize, to: usize) -> &mut Self {
        self.highlighted_lines.push(LineRange::new(from, to));
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

    pub fn themes(&self) -> impl Iterator<Item = &str> {
        self.assets.themes()
    }

    pub fn syntaxes(&self) -> impl Iterator<Item = &SyntaxReference> {
        self.assets.syntaxes().iter()
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
            #[cfg(feature = "git")]
            style_components.push(StyleComponent::Changes);
        }
        self.config.style_components = StyleComponents::new(&style_components);

        // Collect the inputs to print
        let mut inputs: Vec<Input> = vec![];
        std::mem::swap(&mut inputs, &mut self.inputs);

        // Run the controller
        let controller = Controller::new(&self.config, &self.assets);
        controller.run(inputs.into_iter().map(|i| i.into()).collect())
    }
}

/// An input source for the pretty printer.
pub struct Input<'a> {
    input: input::Input<'a>,
}

impl<'a> Input<'a> {
    /// A new input from a reader.
    pub fn from_reader<R: Read + 'a>(reader: R) -> Self {
        input::Input::from_reader(Box::new(reader)).into()
    }

    /// A new input from a file.
    pub fn from_file(path: impl AsRef<OsStr>) -> Self {
        input::Input::ordinary_file(path.as_ref()).into()
    }

    /// A new input from bytes.
    pub fn from_bytes(bytes: &'a [u8]) -> Self {
        Input::from_reader(bytes).into()
    }

    /// A new input from STDIN.
    pub fn from_stdin() -> Self {
        input::Input::stdin().into()
    }

    /// The filename of the input.
    /// This affects syntax detection and changes the default header title.
    pub fn name(mut self, name: impl AsRef<OsStr>) -> Self {
        self.input = self.input.with_name(Some(name.as_ref()));
        self
    }

    /// The description for the type of input (e.g. "File")
    pub fn kind(mut self, kind: impl Into<String>) -> Self {
        let kind = kind.into();
        self.input
            .description_mut()
            .set_kind(if kind.is_empty() { None } else { Some(kind) });
        self
    }

    /// The title for the input (e.g. "http://example.com/example.txt")
    /// This defaults to the file name.
    pub fn title(mut self, title: impl Into<String>) -> Self {
        self.input.description_mut().set_title(Some(title.into()));
        self
    }
}

impl<'a> Into<Input<'a>> for input::Input<'a> {
    fn into(self) -> Input<'a> {
        Input { input: self }
    }
}

impl<'a> Into<input::Input<'a>> for Input<'a> {
    fn into(self) -> input::Input<'a> {
        self.input
    }
}

use std::io::Read;
use std::path::Path;

use console::Term;

use crate::{
    assets::HighlightingAssets,
    config::{Config, VisibleLines},
    controller::Controller,
    error::Result,
    input,
    line_range::{HighlightedLineRanges, LineRange, LineRanges},
    output::OutputHandle,
    style::StyleComponent,
    StripAnsiMode, SyntaxMapping, WrappingMode,
};

#[cfg(feature = "paging")]
use crate::paging::PagingMode;

#[derive(Default)]
struct ActiveStyleComponents {
    header_filename: bool,
    #[cfg(feature = "git")]
    vcs_modification_markers: bool,
    grid: bool,
    rule: bool,
    line_numbers: bool,
    snip: bool,
}

#[non_exhaustive]
pub struct Syntax {
    pub name: String,
    pub file_extensions: Vec<String>,
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
        let config = Config {
            colored_output: true,
            true_color: true,
            ..Default::default()
        };

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
    pub fn input_file(&mut self, path: impl AsRef<Path>) -> &mut Self {
        self.input(Input::from_file(path).kind("File"))
    }

    /// Add multiple files which should be pretty-printed
    pub fn input_files<I, P>(&mut self, paths: I) -> &mut Self
    where
        I: IntoIterator<Item = P>,
        P: AsRef<Path>,
    {
        self.inputs(paths.into_iter().map(Input::from_file))
    }

    /// Add STDIN as an input
    pub fn input_stdin(&mut self) -> &mut Self {
        self.inputs.push(Input::from_stdin());
        self
    }

    /// Add a byte string as an input
    pub fn input_from_bytes(&mut self, content: &'a [u8]) -> &mut Self {
        self.input_from_reader(content)
    }

    /// Add a custom reader as an input
    pub fn input_from_reader<R: Read + 'a>(&mut self, reader: R) -> &mut Self {
        self.inputs.push(Input::from_reader(reader));
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
        self.active_style_components.header_filename = yes;
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

    /// Whether to paint a horizontal rule to delimit files
    pub fn rule(&mut self, yes: bool) -> &mut Self {
        self.active_style_components.rule = yes;
        self
    }

    /// Whether to show modification markers for VCS changes. This has no effect if
    /// the `git` feature is not activated.
    #[cfg(feature = "git")]
    pub fn vcs_modification_markers(&mut self, yes: bool) -> &mut Self {
        self.active_style_components.vcs_modification_markers = yes;
        self
    }

    /// Whether to print binary content or nonprintable characters (default: no)
    pub fn show_nonprintable(&mut self, yes: bool) -> &mut Self {
        self.config.show_nonprintable = yes;
        self
    }

    /// Whether to show "snip" markers between visible line ranges (default: no)
    pub fn snip(&mut self, yes: bool) -> &mut Self {
        self.active_style_components.snip = yes;
        self
    }

    /// Whether to remove ANSI escape sequences from the input (default: never)
    ///
    /// If `Auto` is used, escape sequences will only be removed when the input
    /// is not plain text.
    pub fn strip_ansi(&mut self, mode: StripAnsiMode) -> &mut Self {
        self.config.strip_ansi = mode;
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

    /// Specify the maximum number of consecutive empty lines to print.
    pub fn squeeze_empty_lines(&mut self, maximum: Option<usize>) -> &mut Self {
        self.config.squeeze_lines = maximum;
        self
    }

    /// Specify the highlighting theme.
    /// You can use [`crate::theme::theme`] to pick a theme based on user preferences
    /// and the terminal's background color.
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

    pub fn syntaxes(&self) -> impl Iterator<Item = Syntax> + '_ {
        // We always use assets from the binary, which are guaranteed to always
        // be valid, so get_syntaxes() can never fail here
        self.assets
            .get_syntaxes()
            .unwrap()
            .iter()
            .filter(|s| !s.hidden)
            .map(|s| Syntax {
                name: s.name.clone(),
                file_extensions: s.file_extensions.clone(),
            })
    }

    /// Pretty-print all specified inputs. This method will "use" all stored inputs.
    /// If you want to call 'print' multiple times, you have to call the appropriate
    /// input_* methods again.
    pub fn print(&mut self) -> Result<bool> {
        self.print_with_writer(None::<&mut dyn std::fmt::Write>)
    }

    /// Pretty-print all specified inputs to a specified writer.
    pub fn print_with_writer<W: std::fmt::Write>(&mut self, writer: Option<W>) -> Result<bool> {
        let highlight_lines = std::mem::take(&mut self.highlighted_lines);
        self.config.highlighted_lines = HighlightedLineRanges(LineRanges::from(highlight_lines));
        self.config.term_width = self
            .term_width
            .unwrap_or_else(|| Term::stdout().size().1 as usize);

        self.config.style_components.clear();
        if self.active_style_components.grid {
            self.config.style_components.insert(StyleComponent::Grid);
        }
        if self.active_style_components.rule {
            self.config.style_components.insert(StyleComponent::Rule);
        }
        if self.active_style_components.header_filename {
            self.config
                .style_components
                .insert(StyleComponent::HeaderFilename);
        }
        if self.active_style_components.line_numbers {
            self.config
                .style_components
                .insert(StyleComponent::LineNumbers);
        }
        if self.active_style_components.snip {
            self.config.style_components.insert(StyleComponent::Snip);
        }
        #[cfg(feature = "git")]
        if self.active_style_components.vcs_modification_markers {
            self.config.style_components.insert(StyleComponent::Changes);
        }

        // Collect the inputs to print
        let inputs = std::mem::take(&mut self.inputs);

        // Run the controller
        let controller = Controller::new(&self.config, &self.assets);

        // If writer is provided, pass it to the controller, otherwise pass None
        if let Some(mut w) = writer {
            controller.run(
                inputs.into_iter().map(|i| i.into()).collect(),
                Some(OutputHandle::FmtWrite(&mut w)),
            )
        } else {
            controller.run(inputs.into_iter().map(|i| i.into()).collect(), None)
        }
    }
}

impl Default for PrettyPrinter<'_> {
    fn default() -> Self {
        Self::new()
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
    pub fn from_file(path: impl AsRef<Path>) -> Self {
        input::Input::ordinary_file(path).into()
    }

    /// A new input from bytes.
    pub fn from_bytes(bytes: &'a [u8]) -> Self {
        Input::from_reader(bytes)
    }

    /// A new input from STDIN.
    pub fn from_stdin() -> Self {
        input::Input::stdin().into()
    }

    /// The filename of the input.
    /// This affects syntax detection and changes the default header title.
    pub fn name(mut self, name: impl AsRef<Path>) -> Self {
        self.input = self.input.with_name(Some(name));
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

    /// The title for the input (e.g. "Descriptive title")
    /// This defaults to the file name.
    pub fn title(mut self, title: impl Into<String>) -> Self {
        self.input.description_mut().set_title(Some(title.into()));
        self
    }
}

impl<'a> From<input::Input<'a>> for Input<'a> {
    fn from(input: input::Input<'a>) -> Self {
        Self { input }
    }
}

impl<'a> From<Input<'a>> for input::Input<'a> {
    fn from(Input { input }: Input<'a>) -> Self {
        input
    }
}

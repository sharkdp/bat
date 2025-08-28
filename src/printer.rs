use std::vec::Vec;

use nu_ansi_term::Color::{Fixed, Green, Red, Yellow};
use nu_ansi_term::Style;

use bytesize::ByteSize;

use syntect::easy::HighlightLines;
use syntect::highlighting::Color;
use syntect::highlighting::FontStyle;
use syntect::highlighting::Theme;
use syntect::parsing::SyntaxSet;

use content_inspector::ContentType;

use encoding_rs::{UTF_16BE, UTF_16LE};

use unicode_segmentation::UnicodeSegmentation;
use unicode_width::UnicodeWidthChar;

use crate::assets::{HighlightingAssets, SyntaxReferenceInSet};
use crate::config::Config;
#[cfg(feature = "git")]
use crate::decorations::LineChangesDecoration;
use crate::decorations::{Decoration, GridBorderDecoration, LineNumberDecoration};
#[cfg(feature = "git")]
use crate::diff::LineChanges;
use crate::error::*;
use crate::input::OpenedInput;
use crate::line_range::{MaxBufferedLineNumber, RangeCheckResult};
use crate::output::OutputHandle;
use crate::preprocessor::strip_ansi;
use crate::preprocessor::{expand_tabs, replace_nonprintable};
use crate::style::StyleComponent;
use crate::terminal::{as_terminal_escaped, to_ansi_color};
use crate::vscreen::{AnsiStyle, EscapeSequence, EscapeSequenceIterator};
use crate::wrapping::WrappingMode;
use crate::BinaryBehavior;
use crate::StripAnsiMode;

const ANSI_UNDERLINE_ENABLE: EscapeSequence = EscapeSequence::CSI {
    raw_sequence: "\x1B[4m",
    parameters: "4",
    intermediates: "",
    final_byte: "m",
};

const ANSI_UNDERLINE_DISABLE: EscapeSequence = EscapeSequence::CSI {
    raw_sequence: "\x1B[24m",
    parameters: "24",
    intermediates: "",
    final_byte: "m",
};

const EMPTY_SYNTECT_STYLE: syntect::highlighting::Style = syntect::highlighting::Style {
    foreground: Color {
        r: 127,
        g: 127,
        b: 127,
        a: 255,
    },
    background: Color {
        r: 127,
        g: 127,
        b: 127,
        a: 255,
    },
    font_style: FontStyle::empty(),
};

pub(crate) trait Printer {
    fn print_header(
        &mut self,
        handle: &mut OutputHandle,
        input: &OpenedInput,
        add_header_padding: bool,
    ) -> Result<()>;
    fn print_footer(&mut self, handle: &mut OutputHandle, input: &OpenedInput) -> Result<()>;

    fn print_snip(&mut self, handle: &mut OutputHandle) -> Result<()>;

    fn print_line(
        &mut self,
        out_of_range: bool,
        handle: &mut OutputHandle,
        line_number: usize,
        line_buffer: &[u8],
        max_buffered_line_number: MaxBufferedLineNumber,
    ) -> Result<()>;
}

pub struct SimplePrinter<'a> {
    config: &'a Config<'a>,
    consecutive_empty_lines: usize,
}

impl<'a> SimplePrinter<'a> {
    pub fn new(config: &'a Config) -> Self {
        SimplePrinter {
            config,
            consecutive_empty_lines: 0,
        }
    }
}

impl Printer for SimplePrinter<'_> {
    fn print_header(
        &mut self,
        _handle: &mut OutputHandle,
        _input: &OpenedInput,
        _add_header_padding: bool,
    ) -> Result<()> {
        Ok(())
    }

    fn print_footer(&mut self, _handle: &mut OutputHandle, _input: &OpenedInput) -> Result<()> {
        Ok(())
    }

    fn print_snip(&mut self, _handle: &mut OutputHandle) -> Result<()> {
        Ok(())
    }

    fn print_line(
        &mut self,
        out_of_range: bool,
        handle: &mut OutputHandle,
        _line_number: usize,
        line_buffer: &[u8],
        _max_buffered_line_number: MaxBufferedLineNumber,
    ) -> Result<()> {
        // Skip squeezed lines.
        if let Some(squeeze_limit) = self.config.squeeze_lines {
            if String::from_utf8_lossy(line_buffer)
                .trim_end_matches(['\r', '\n'])
                .is_empty()
            {
                self.consecutive_empty_lines += 1;
                if self.consecutive_empty_lines > squeeze_limit {
                    return Ok(());
                }
            } else {
                self.consecutive_empty_lines = 0;
            }
        }

        if !out_of_range {
            if self.config.show_nonprintable {
                let line = replace_nonprintable(
                    line_buffer,
                    self.config.tab_width,
                    self.config.nonprintable_notation,
                );
                write!(handle, "{line}")?;
            } else {
                match handle {
                    OutputHandle::IoWrite(handle) => handle.write_all(line_buffer)?,
                    OutputHandle::FmtWrite(handle) => {
                        write!(
                            handle,
                            "{}",
                            std::str::from_utf8(line_buffer).map_err(|_| Error::Msg(
                                "encountered invalid utf8 while printing to non-io buffer"
                                    .to_string()
                            ))?
                        )?;
                    }
                }
            };
        }
        Ok(())
    }
}

struct HighlighterFromSet<'a> {
    highlighter: HighlightLines<'a>,
    syntax_set: &'a SyntaxSet,
}

impl<'a> HighlighterFromSet<'a> {
    fn new(syntax_in_set: SyntaxReferenceInSet<'a>, theme: &'a Theme) -> Self {
        Self {
            highlighter: HighlightLines::new(syntax_in_set.syntax, theme),
            syntax_set: syntax_in_set.syntax_set,
        }
    }
}

pub(crate) struct InteractivePrinter<'a> {
    colors: Colors,
    config: &'a Config<'a>,
    decorations: Vec<Box<dyn Decoration>>,
    panel_width: usize,
    ansi_style: AnsiStyle,
    content_type: Option<ContentType>,
    #[cfg(feature = "git")]
    pub line_changes: &'a Option<LineChanges>,
    highlighter_from_set: Option<HighlighterFromSet<'a>>,
    background_color_highlight: Option<Color>,
    consecutive_empty_lines: usize,
    strip_ansi: bool,
}

impl<'a> InteractivePrinter<'a> {
    pub(crate) fn new(
        config: &'a Config,
        assets: &'a HighlightingAssets,
        input: &mut OpenedInput,
        #[cfg(feature = "git")] line_changes: &'a Option<LineChanges>,
    ) -> Result<Self> {
        let theme = assets.get_theme(&config.theme);

        let background_color_highlight = theme.settings.line_highlight;

        let colors = if config.colored_output {
            Colors::colored(theme, config.true_color)
        } else {
            Colors::plain()
        };

        // Create decorations.
        let mut decorations: Vec<Box<dyn Decoration>> = Vec::new();

        if config.style_components.numbers() {
            decorations.push(Box::new(LineNumberDecoration::new(&colors)));
        }

        #[cfg(feature = "git")]
        {
            if config.style_components.changes() {
                decorations.push(Box::new(LineChangesDecoration::new(&colors)));
            }
        }

        let mut panel_width: usize =
            decorations.len() + decorations.iter().fold(0, |a, x| a + x.width());

        // The grid border decoration isn't added until after the panel_width calculation, since the
        // print_horizontal_line, print_header, and print_footer functions all assume the panel
        // width is without the grid border.
        if config.style_components.grid() && !decorations.is_empty() {
            decorations.push(Box::new(GridBorderDecoration::new(&colors)));
        }

        // Disable the panel if the terminal is too small (i.e. can't fit 5 characters with the
        // panel showing).
        if config.term_width
            < (decorations.len() + decorations.iter().fold(0, |a, x| a + x.width())) + 5
        {
            decorations.clear();
            panel_width = 0;
        }

        // Get the highlighter for the output.
        let is_printing_binary = input
            .reader
            .content_type
            .is_some_and(|c| c.is_binary() && !config.show_nonprintable);

        let needs_to_match_syntax = (!is_printing_binary
            || matches!(config.binary, BinaryBehavior::AsText))
            && (config.colored_output || config.strip_ansi == StripAnsiMode::Auto);

        let (is_plain_text, highlighter_from_set) = if needs_to_match_syntax {
            // Determine the type of syntax for highlighting
            const PLAIN_TEXT_SYNTAX: &str = "Plain Text";
            match assets.get_syntax(config.language, input, &config.syntax_mapping) {
                Ok(syntax_in_set) => (
                    syntax_in_set.syntax.name == PLAIN_TEXT_SYNTAX,
                    Some(HighlighterFromSet::new(syntax_in_set, theme)),
                ),

                Err(Error::UndetectedSyntax(_)) => (
                    true,
                    Some(
                        assets
                            .find_syntax_by_name(PLAIN_TEXT_SYNTAX)?
                            .map(|s| HighlighterFromSet::new(s, theme))
                            .expect("A plain text syntax is available"),
                    ),
                ),

                Err(e) => return Err(e),
            }
        } else {
            (false, None)
        };

        // Determine when to strip ANSI sequences
        let strip_ansi = match config.strip_ansi {
            _ if config.show_nonprintable => false,
            StripAnsiMode::Always => true,
            StripAnsiMode::Auto if is_plain_text => false, // Plain text may already contain escape sequences.
            StripAnsiMode::Auto => true,
            _ => false,
        };

        Ok(InteractivePrinter {
            panel_width,
            colors,
            config,
            decorations,
            content_type: input.reader.content_type,
            ansi_style: AnsiStyle::new(),
            #[cfg(feature = "git")]
            line_changes,
            highlighter_from_set,
            background_color_highlight,
            consecutive_empty_lines: 0,
            strip_ansi,
        })
    }

    fn print_horizontal_line_term(
        &mut self,
        handle: &mut OutputHandle,
        style: Style,
    ) -> Result<()> {
        writeln!(
            handle,
            "{}",
            style.paint("─".repeat(self.config.term_width))
        )?;
        Ok(())
    }

    fn print_horizontal_line(&mut self, handle: &mut OutputHandle, grid_char: char) -> Result<()> {
        if self.panel_width == 0 {
            self.print_horizontal_line_term(handle, self.colors.grid)?;
        } else {
            let hline = "─".repeat(self.config.term_width - (self.panel_width + 1));
            let hline = format!("{}{grid_char}{hline}", "─".repeat(self.panel_width));
            writeln!(handle, "{}", self.colors.grid.paint(hline))?;
        }

        Ok(())
    }

    fn create_fake_panel(&self, text: &str) -> String {
        if self.panel_width == 0 {
            return "".to_string();
        }

        let text_truncated: String = text.chars().take(self.panel_width - 1).collect();
        let text_filled: String = format!(
            "{text_truncated}{}",
            " ".repeat(self.panel_width - 1 - text_truncated.len())
        );
        if self.config.style_components.grid() {
            format!("{text_filled} │ ")
        } else {
            text_filled
        }
    }

    fn get_header_component_indent_length(&self) -> usize {
        if self.config.style_components.grid() && self.panel_width > 0 {
            self.panel_width + 2
        } else {
            self.panel_width
        }
    }

    fn print_header_component_indent(&mut self, handle: &mut OutputHandle) -> Result<()> {
        if self.config.style_components.grid() {
            write!(
                handle,
                "{}{}",
                " ".repeat(self.panel_width),
                self.colors
                    .grid
                    .paint(if self.panel_width > 0 { "│ " } else { "" }),
            )
        } else {
            write!(handle, "{}", " ".repeat(self.panel_width))
        }
    }

    fn print_header_component_with_indent(
        &mut self,
        handle: &mut OutputHandle,
        content: &str,
    ) -> Result<()> {
        self.print_header_component_indent(handle)?;
        writeln!(handle, "{content}")
    }

    fn print_header_multiline_component(
        &mut self,
        handle: &mut OutputHandle,
        content: &str,
    ) -> Result<()> {
        let content_width = self.config.term_width - self.get_header_component_indent_length();
        if content.chars().count() <= content_width {
            return self.print_header_component_with_indent(handle, content);
        }

        let mut content_graphemes: Vec<&str> = content.graphemes(true).collect();
        while content_graphemes.len() > content_width {
            let (content_line, remaining) = content_graphemes.split_at(content_width);
            self.print_header_component_with_indent(handle, content_line.join("").as_str())?;
            content_graphemes = remaining.to_vec();
        }
        self.print_header_component_with_indent(handle, content_graphemes.join("").as_str())
    }

    fn highlight_regions_for_line<'b>(
        &mut self,
        line: &'b str,
    ) -> Result<Vec<(syntect::highlighting::Style, &'b str)>> {
        let highlighter_from_set = match self.highlighter_from_set {
            Some(ref mut highlighter_from_set) => highlighter_from_set,
            _ => return Ok(vec![(EMPTY_SYNTECT_STYLE, line)]),
        };

        // skip syntax highlighting on long lines
        let too_long = line.len() > 1024 * 16;

        let for_highlighting: &str = if too_long { "\n" } else { line };

        let mut highlighted_line = highlighter_from_set
            .highlighter
            .highlight_line(for_highlighting, highlighter_from_set.syntax_set)?;

        if too_long {
            highlighted_line[0].1 = line;
        }

        Ok(highlighted_line)
    }

    fn preprocess(&self, text: &str, cursor: &mut usize) -> String {
        if self.config.tab_width > 0 {
            return expand_tabs(text, self.config.tab_width, cursor);
        }

        *cursor += text.len();
        text.to_string()
    }
}

impl Printer for InteractivePrinter<'_> {
    fn print_header(
        &mut self,
        handle: &mut OutputHandle,
        input: &OpenedInput,
        add_header_padding: bool,
    ) -> Result<()> {
        if add_header_padding && self.config.style_components.rule() {
            self.print_horizontal_line_term(handle, self.colors.rule)?;
        }

        if !self.config.style_components.header() {
            if Some(ContentType::BINARY) == self.content_type
                && !self.config.show_nonprintable
                && !matches!(self.config.binary, BinaryBehavior::AsText)
            {
                writeln!(
                    handle,
                    "{}: Binary content from {} will not be printed to the terminal \
                     (but will be present if the output of 'bat' is piped). You can use 'bat -A' \
                     to show the binary file contents.",
                    Yellow.paint("[bat warning]"),
                    input.description.summary(),
                )?;
            } else if self.config.style_components.grid() {
                self.print_horizontal_line(handle, '┬')?;
            }
            return Ok(());
        }

        let mode = match self.content_type {
            Some(ContentType::BINARY) => "   <BINARY>",
            Some(ContentType::UTF_16LE) => "   <UTF-16LE>",
            Some(ContentType::UTF_16BE) => "   <UTF-16BE>",
            None => "   <EMPTY>",
            _ => "",
        };

        let description = &input.description;
        let metadata = &input.metadata;

        // We use this iterator to have a deterministic order for
        // header components. HashSet has arbitrary order, but Vec is ordered.
        let header_components: Vec<StyleComponent> = [
            (
                StyleComponent::HeaderFilename,
                self.config.style_components.header_filename(),
            ),
            (
                StyleComponent::HeaderFilesize,
                self.config.style_components.header_filesize(),
            ),
        ]
        .iter()
        .filter(|(_, is_enabled)| *is_enabled)
        .map(|(component, _)| *component)
        .collect();

        // Print the cornering grid before the first header component
        if self.config.style_components.grid() {
            self.print_horizontal_line(handle, '┬')?;
        } else {
            // Only pad space between files, if we haven't already drawn a horizontal rule
            if add_header_padding && !self.config.style_components.rule() {
                writeln!(handle)?;
            }
        }

        header_components
            .iter()
            .try_for_each(|component| match component {
                StyleComponent::HeaderFilename => {
                    let header_filename = format!(
                        "{}{}{mode}",
                        description
                            .kind()
                            .map(|kind| format!("{kind}: "))
                            .unwrap_or_else(|| "".into()),
                        self.colors.header_value.paint(description.title()),
                    );
                    self.print_header_multiline_component(handle, &header_filename)
                }
                StyleComponent::HeaderFilesize => {
                    let bsize = metadata
                        .size
                        .map(|s| format!("{}", ByteSize(s)))
                        .unwrap_or_else(|| "-".into());
                    let header_filesize =
                        format!("Size: {}", self.colors.header_value.paint(bsize));
                    self.print_header_multiline_component(handle, &header_filesize)
                }
                _ => Ok(()),
            })?;

        if self.config.style_components.grid() {
            if self.content_type.is_some_and(|c| c.is_text())
                || self.config.show_nonprintable
                || matches!(self.config.binary, BinaryBehavior::AsText)
            {
                self.print_horizontal_line(handle, '┼')?;
            } else {
                self.print_horizontal_line(handle, '┴')?;
            }
        }

        Ok(())
    }

    fn print_footer(&mut self, handle: &mut OutputHandle, _input: &OpenedInput) -> Result<()> {
        if self.config.style_components.grid()
            && (self.content_type.is_some_and(|c| c.is_text())
                || self.config.show_nonprintable
                || matches!(self.config.binary, BinaryBehavior::AsText))
        {
            self.print_horizontal_line(handle, '┴')
        } else {
            Ok(())
        }
    }

    fn print_snip(&mut self, handle: &mut OutputHandle) -> Result<()> {
        let panel = self.create_fake_panel(" ...");
        let panel_count = panel.chars().count();

        let title = "8<";
        let title_count = title.chars().count();

        let snip_left = "─ ".repeat((self.config.term_width - panel_count - (title_count / 2)) / 4);
        let snip_left_count = snip_left.chars().count(); // Can't use .len() with Unicode.

        let snip_right =
            " ─".repeat((self.config.term_width - panel_count - snip_left_count - title_count) / 2);

        writeln!(
            handle,
            "{}",
            self.colors
                .grid
                .paint(format!("{panel}{snip_left}{title}{snip_right}"))
        )?;

        Ok(())
    }

    fn print_line(
        &mut self,
        out_of_range: bool,
        handle: &mut OutputHandle,
        line_number: usize,
        line_buffer: &[u8],
        max_buffered_line_number: MaxBufferedLineNumber,
    ) -> Result<()> {
        let line = if self.config.show_nonprintable {
            replace_nonprintable(
                line_buffer,
                self.config.tab_width,
                self.config.nonprintable_notation,
            )
            .into()
        } else {
            let mut line = match self.content_type {
                Some(ContentType::BINARY) | None
                    if !matches!(self.config.binary, BinaryBehavior::AsText) =>
                {
                    return Ok(());
                }
                Some(ContentType::UTF_16LE) => UTF_16LE.decode_with_bom_removal(line_buffer).0,
                Some(ContentType::UTF_16BE) => UTF_16BE.decode_with_bom_removal(line_buffer).0,
                _ => {
                    let line = String::from_utf8_lossy(line_buffer);
                    if line_number == 1 {
                        match line.strip_prefix('\u{feff}') {
                            Some(stripped) => stripped.to_string().into(),
                            None => line,
                        }
                    } else {
                        line
                    }
                }
            };

            // If ANSI escape sequences are supposed to be stripped, do it before syntax highlighting.
            if self.strip_ansi {
                line = strip_ansi(&line).into()
            }

            line
        };

        let regions = self.highlight_regions_for_line(&line)?;
        if out_of_range {
            return Ok(());
        }

        // Skip squeezed lines.
        if let Some(squeeze_limit) = self.config.squeeze_lines {
            if line.trim_end_matches(['\r', '\n']).is_empty() {
                self.consecutive_empty_lines += 1;
                if self.consecutive_empty_lines > squeeze_limit {
                    return Ok(());
                }
            } else {
                self.consecutive_empty_lines = 0;
            }
        }

        let mut cursor: usize = 0;
        let mut cursor_max: usize = self.config.term_width;
        let mut cursor_total: usize = 0;
        let mut panel_wrap: Option<String> = None;

        // Line highlighting
        let highlight_this_line = self
            .config
            .highlighted_lines
            .0
            .check(line_number, max_buffered_line_number)
            == RangeCheckResult::InRange;

        if highlight_this_line && self.config.theme == "ansi" {
            self.ansi_style.update(ANSI_UNDERLINE_ENABLE);
        }

        let background_color = self
            .background_color_highlight
            .filter(|_| highlight_this_line);

        // Line decorations.
        if self.panel_width > 0 {
            let decorations = self
                .decorations
                .iter()
                .map(|d| d.generate(line_number, false, self));

            for deco in decorations {
                write!(handle, "{} ", deco.text)?;
                cursor_max -= deco.width + 1;
            }
        }

        // Line contents.
        if matches!(self.config.wrapping_mode, WrappingMode::NoWrapping(_)) {
            let true_color = self.config.true_color;
            let colored_output = self.config.colored_output;
            let italics = self.config.use_italic_text;

            for &(style, region) in &regions {
                let ansi_iterator = EscapeSequenceIterator::new(region);
                for chunk in ansi_iterator {
                    match chunk {
                        // Regular text.
                        EscapeSequence::Text(text) => {
                            let text = self.preprocess(text, &mut cursor_total);
                            let text_trimmed = text.trim_end_matches(['\r', '\n']);

                            write!(
                                handle,
                                "{}{}",
                                as_terminal_escaped(
                                    style,
                                    &format!("{}{text_trimmed}", self.ansi_style),
                                    true_color,
                                    colored_output,
                                    italics,
                                    background_color
                                ),
                                self.ansi_style.to_reset_sequence(),
                            )?;

                            // Pad the rest of the line.
                            if text.len() != text_trimmed.len() {
                                if let Some(background_color) = background_color {
                                    let ansi_style = Style {
                                        background: to_ansi_color(background_color, true_color),
                                        ..Default::default()
                                    };

                                    let width = if cursor_total <= cursor_max {
                                        cursor_max - cursor_total + 1
                                    } else {
                                        0
                                    };
                                    write!(handle, "{}", ansi_style.paint(" ".repeat(width)))?;
                                }
                                write!(handle, "{}", &text[text_trimmed.len()..])?;
                            }
                        }

                        // ANSI escape passthrough.
                        _ => {
                            write!(handle, "{}", chunk.raw())?;
                            self.ansi_style.update(chunk);
                        }
                    }
                }
            }

            if !self.config.style_components.plain() && line.bytes().next_back() != Some(b'\n') {
                writeln!(handle)?;
            }
        } else {
            for &(style, region) in &regions {
                let ansi_iterator = EscapeSequenceIterator::new(region);
                for chunk in ansi_iterator {
                    match chunk {
                        // Regular text.
                        EscapeSequence::Text(text) => {
                            let text = self
                                .preprocess(text.trim_end_matches(['\r', '\n']), &mut cursor_total);

                            let mut max_width = cursor_max - cursor;

                            // line buffer (avoid calling write! for every character)
                            let mut line_buf = String::with_capacity(max_width * 4);

                            // Displayed width of line_buf
                            let mut current_width = 0;

                            for c in text.chars() {
                                // calculate the displayed width for next character
                                let cw = c.width().unwrap_or(0);
                                current_width += cw;

                                // if next character cannot be printed on this line,
                                // flush the buffer.
                                if current_width > max_width {
                                    // Generate wrap padding if not already generated.
                                    if panel_wrap.is_none() {
                                        panel_wrap = if self.panel_width > 0 {
                                            Some(format!(
                                                "{} ",
                                                self.decorations
                                                    .iter()
                                                    .map(|d| d
                                                        .generate(line_number, true, self)
                                                        .text)
                                                    .collect::<Vec<String>>()
                                                    .join(" ")
                                            ))
                                        } else {
                                            Some("".to_string())
                                        }
                                    }

                                    // It wraps.
                                    write!(
                                        handle,
                                        "{}{}\n{}",
                                        as_terminal_escaped(
                                            style,
                                            &format!("{}{line_buf}", self.ansi_style),
                                            self.config.true_color,
                                            self.config.colored_output,
                                            self.config.use_italic_text,
                                            background_color
                                        ),
                                        self.ansi_style.to_reset_sequence(),
                                        panel_wrap.clone().unwrap()
                                    )?;

                                    cursor = 0;
                                    max_width = cursor_max;

                                    line_buf.clear();
                                    current_width = cw;
                                }

                                line_buf.push(c);
                            }

                            // flush the buffer
                            cursor += current_width;
                            write!(
                                handle,
                                "{}",
                                as_terminal_escaped(
                                    style,
                                    &format!("{}{line_buf}", self.ansi_style),
                                    self.config.true_color,
                                    self.config.colored_output,
                                    self.config.use_italic_text,
                                    background_color
                                )
                            )?;
                        }

                        // ANSI escape passthrough.
                        _ => {
                            write!(handle, "{}", chunk.raw())?;
                            self.ansi_style.update(chunk);
                        }
                    }
                }
            }

            if let Some(background_color) = background_color {
                let ansi_style = Style {
                    background: to_ansi_color(background_color, self.config.true_color),
                    ..Default::default()
                };

                write!(
                    handle,
                    "{}",
                    ansi_style.paint(" ".repeat(cursor_max - cursor))
                )?;
            }
            writeln!(handle)?;
        }

        if highlight_this_line && self.config.theme == "ansi" {
            write!(handle, "{}", ANSI_UNDERLINE_DISABLE.raw())?;
            self.ansi_style.update(ANSI_UNDERLINE_DISABLE);
        }

        Ok(())
    }
}

const DEFAULT_GUTTER_COLOR: u8 = 238;

#[derive(Debug, Default)]
pub struct Colors {
    pub grid: Style,
    pub rule: Style,
    pub header_value: Style,
    pub git_added: Style,
    pub git_removed: Style,
    pub git_modified: Style,
    pub line_number: Style,
}

impl Colors {
    fn plain() -> Self {
        Colors::default()
    }

    fn colored(theme: &Theme, true_color: bool) -> Self {
        let gutter_style = Style {
            foreground: match theme.settings.gutter_foreground {
                // If the theme provides a gutter foreground color, use it.
                // Note: It might be the special value #00000001, in which case
                // to_ansi_color returns None and we use an empty Style
                // (resulting in the terminal's default foreground color).
                Some(c) => to_ansi_color(c, true_color),
                // Otherwise, use a specific fallback color.
                None => Some(Fixed(DEFAULT_GUTTER_COLOR)),
            },
            ..Style::default()
        };

        Colors {
            grid: gutter_style,
            rule: gutter_style,
            header_value: Style::new().bold(),
            git_added: Green.normal(),
            git_removed: Red.normal(),
            git_modified: Yellow.normal(),
            line_number: gutter_style,
        }
    }
}

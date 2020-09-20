use std::io::Write;
use std::vec::Vec;

use ansi_term::Colour::{Fixed, Green, Red, Yellow};
use ansi_term::Style;

use console::AnsiCodeIterator;

use syntect::easy::HighlightLines;
use syntect::highlighting::Color;
use syntect::highlighting::Theme;
use syntect::parsing::SyntaxSet;

use content_inspector::ContentType;

use encoding::all::{UTF_16BE, UTF_16LE};
use encoding::{DecoderTrap, Encoding};

use unicode_width::UnicodeWidthChar;

use crate::assets::HighlightingAssets;
use crate::config::Config;
#[cfg(feature = "git")]
use crate::decorations::LineChangesDecoration;
use crate::decorations::{Decoration, GridBorderDecoration, LineNumberDecoration};
#[cfg(feature = "git")]
use crate::diff::LineChanges;
use crate::error::*;
use crate::input::OpenedInput;
use crate::line_range::RangeCheckResult;
use crate::preprocessor::{expand_tabs, replace_nonprintable};
use crate::terminal::{as_terminal_escaped, to_ansi_color};
use crate::wrapping::WrappingMode;

pub(crate) trait Printer {
    fn print_header(
        &mut self,
        handle: &mut dyn Write,
        input: &OpenedInput,
        add_header_padding: bool,
    ) -> Result<()>;
    fn print_footer(&mut self, handle: &mut dyn Write, input: &OpenedInput) -> Result<()>;

    fn print_snip(&mut self, handle: &mut dyn Write) -> Result<()>;

    fn print_line(
        &mut self,
        out_of_range: bool,
        handle: &mut dyn Write,
        line_number: usize,
        line_buffer: &[u8],
    ) -> Result<()>;
}

pub struct SimplePrinter<'a> {
    config: &'a Config<'a>,
}

impl<'a> SimplePrinter<'a> {
    pub fn new(config: &'a Config) -> Self {
        SimplePrinter { config }
    }
}

impl<'a> Printer for SimplePrinter<'a> {
    fn print_header(
        &mut self,
        _handle: &mut dyn Write,
        _input: &OpenedInput,
        _add_header_padding: bool,
    ) -> Result<()> {
        Ok(())
    }

    fn print_footer(&mut self, _handle: &mut dyn Write, _input: &OpenedInput) -> Result<()> {
        Ok(())
    }

    fn print_snip(&mut self, _handle: &mut dyn Write) -> Result<()> {
        Ok(())
    }

    fn print_line(
        &mut self,
        out_of_range: bool,
        handle: &mut dyn Write,
        _line_number: usize,
        line_buffer: &[u8],
    ) -> Result<()> {
        if !out_of_range {
            if self.config.show_nonprintable {
                let line = replace_nonprintable(line_buffer, self.config.tab_width);
                write!(handle, "{}", line)?;
                if line_buffer.last() == Some(&b'\n') {
                    writeln!(handle)?;
                }
            } else {
                handle.write_all(line_buffer)?
            };
        }
        Ok(())
    }
}

pub(crate) struct InteractivePrinter<'a> {
    colors: Colors,
    config: &'a Config<'a>,
    decorations: Vec<Box<dyn Decoration>>,
    panel_width: usize,
    ansi_prefix_sgr: String,
    content_type: Option<ContentType>,
    #[cfg(feature = "git")]
    pub line_changes: &'a Option<LineChanges>,
    highlighter: Option<HighlightLines<'a>>,
    syntax_set: &'a SyntaxSet,
    background_color_highlight: Option<Color>,
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

        let highlighter = if input
            .reader
            .content_type
            .map_or(false, |c| c.is_binary() && !config.show_nonprintable)
        {
            None
        } else {
            // Determine the type of syntax for highlighting
            let syntax = match assets.get_syntax(config.language, input, &config.syntax_mapping) {
                Ok(syntax) => syntax,
                Err(Error(ErrorKind::UndetectedSyntax(_), _)) => {
                    assets.syntax_set.find_syntax_plain_text()
                }
                Err(e) => return Err(e),
            };

            Some(HighlightLines::new(syntax, theme))
        };

        Ok(InteractivePrinter {
            panel_width,
            colors,
            config,
            decorations,
            content_type: input.reader.content_type,
            ansi_prefix_sgr: String::new(),
            #[cfg(feature = "git")]
            line_changes,
            highlighter,
            syntax_set: &assets.syntax_set,
            background_color_highlight,
        })
    }

    fn print_horizontal_line(&mut self, handle: &mut dyn Write, grid_char: char) -> Result<()> {
        if self.panel_width == 0 {
            writeln!(
                handle,
                "{}",
                self.colors.grid.paint("─".repeat(self.config.term_width))
            )?;
        } else {
            let hline = "─".repeat(self.config.term_width - (self.panel_width + 1));
            let hline = format!("{}{}{}", "─".repeat(self.panel_width), grid_char, hline);
            writeln!(handle, "{}", self.colors.grid.paint(hline))?;
        }

        Ok(())
    }

    fn create_fake_panel(&self, text: &str) -> String {
        if self.panel_width == 0 {
            "".to_string()
        } else {
            let text_truncated: String = text.chars().take(self.panel_width - 1).collect();
            let text_filled: String = format!(
                "{}{}",
                text_truncated,
                " ".repeat(self.panel_width - 1 - text_truncated.len())
            );
            if self.config.style_components.grid() {
                format!("{} │ ", text_filled)
            } else {
                text_filled
            }
        }
    }

    fn preprocess(&self, text: &str, cursor: &mut usize) -> String {
        if self.config.tab_width > 0 {
            expand_tabs(text, self.config.tab_width, cursor)
        } else {
            *cursor += text.len();
            text.to_string()
        }
    }
}

impl<'a> Printer for InteractivePrinter<'a> {
    fn print_header(
        &mut self,
        handle: &mut dyn Write,
        input: &OpenedInput,
        add_header_padding: bool,
    ) -> Result<()> {
        if !self.config.style_components.header() {
            if Some(ContentType::BINARY) == self.content_type && !self.config.show_nonprintable {
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

        if self.config.style_components.grid() {
            self.print_horizontal_line(handle, '┬')?;

            write!(
                handle,
                "{}{}",
                " ".repeat(self.panel_width),
                self.colors
                    .grid
                    .paint(if self.panel_width > 0 { "│ " } else { "" }),
            )?;
        } else {
            if add_header_padding {
                writeln!(handle)?;
            }
            write!(handle, "{}", " ".repeat(self.panel_width))?;
        }

        let mode = match self.content_type {
            Some(ContentType::BINARY) => "   <BINARY>",
            Some(ContentType::UTF_16LE) => "   <UTF-16LE>",
            Some(ContentType::UTF_16BE) => "   <UTF-16BE>",
            None => "   <EMPTY>",
            _ => "",
        };

        let description = &input.description;

        writeln!(
            handle,
            "{}{}{}",
            description
                .kind()
                .map(|kind| format!("{}: ", kind))
                .unwrap_or("".into()),
            self.colors.filename.paint(description.title()),
            mode
        )?;

        if self.config.style_components.grid() {
            if self.content_type.map_or(false, |c| c.is_text()) || self.config.show_nonprintable {
                self.print_horizontal_line(handle, '┼')?;
            } else {
                self.print_horizontal_line(handle, '┴')?;
            }
        }

        Ok(())
    }

    fn print_footer(&mut self, handle: &mut dyn Write, _input: &OpenedInput) -> Result<()> {
        if self.config.style_components.grid()
            && (self.content_type.map_or(false, |c| c.is_text()) || self.config.show_nonprintable)
        {
            self.print_horizontal_line(handle, '┴')
        } else {
            Ok(())
        }
    }

    fn print_snip(&mut self, handle: &mut dyn Write) -> Result<()> {
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
                .paint(format!("{}{}{}{}", panel, snip_left, title, snip_right))
        )?;

        Ok(())
    }

    fn print_line(
        &mut self,
        out_of_range: bool,
        handle: &mut dyn Write,
        line_number: usize,
        line_buffer: &[u8],
    ) -> Result<()> {
        let line = if self.config.show_nonprintable {
            replace_nonprintable(&line_buffer, self.config.tab_width)
        } else {
            match self.content_type {
                Some(ContentType::BINARY) | None => {
                    return Ok(());
                }
                Some(ContentType::UTF_16LE) => UTF_16LE
                    .decode(&line_buffer, DecoderTrap::Replace)
                    .map_err(|_| "Invalid UTF-16LE")?,
                Some(ContentType::UTF_16BE) => UTF_16BE
                    .decode(&line_buffer, DecoderTrap::Replace)
                    .map_err(|_| "Invalid UTF-16BE")?,
                _ => String::from_utf8_lossy(&line_buffer).to_string(),
            }
        };

        let regions = {
            let highlighter = match self.highlighter {
                Some(ref mut highlighter) => highlighter,
                _ => {
                    return Ok(());
                }
            };
            highlighter.highlight(line.as_ref(), self.syntax_set)
        };

        if out_of_range {
            return Ok(());
        }

        let mut cursor: usize = 0;
        let mut cursor_max: usize = self.config.term_width;
        let mut cursor_total: usize = 0;
        let mut panel_wrap: Option<String> = None;

        // Line highlighting
        let highlight_this_line =
            self.config.highlighted_lines.0.check(line_number) == RangeCheckResult::InRange;

        let background_color = self
            .background_color_highlight
            .filter(|_| highlight_this_line);

        // Line decorations.
        if self.panel_width > 0 {
            let decorations = self
                .decorations
                .iter()
                .map(|ref d| d.generate(line_number, false, self))
                .collect::<Vec<_>>();

            for deco in decorations {
                write!(handle, "{} ", deco.text)?;
                cursor_max -= deco.width + 1;
            }
        }

        // Line contents.
        if self.config.wrapping_mode == WrappingMode::NoWrapping {
            let true_color = self.config.true_color;
            let colored_output = self.config.colored_output;
            let italics = self.config.use_italic_text;

            for &(style, region) in regions.iter() {
                let text = &*self.preprocess(region, &mut cursor_total);
                let text_trimmed = text.trim_end_matches(|c| c == '\r' || c == '\n');
                write!(
                    handle,
                    "{}",
                    as_terminal_escaped(
                        style,
                        text_trimmed,
                        true_color,
                        colored_output,
                        italics,
                        background_color
                    )
                )?;

                if text.len() != text_trimmed.len() {
                    if let Some(background_color) = background_color {
                        let mut ansi_style = Style::default();
                        ansi_style.background = Some(to_ansi_color(background_color, true_color));
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

            if line.bytes().next_back() != Some(b'\n') {
                writeln!(handle)?;
            }
        } else {
            for &(style, region) in regions.iter() {
                let ansi_iterator = AnsiCodeIterator::new(region);
                let mut ansi_prefix: String = String::new();
                for chunk in ansi_iterator {
                    match chunk {
                        // ANSI escape passthrough.
                        (text, true) => {
                            let is_ansi_csi = text.starts_with("\x1B[");

                            if is_ansi_csi && text.ends_with('m') {
                                // It's an ANSI SGR sequence.
                                // We should be mostly safe to just append these together.
                                ansi_prefix.push_str(text);
                                if text == "\x1B[0m" {
                                    self.ansi_prefix_sgr = "\x1B[0m".to_owned();
                                } else {
                                    self.ansi_prefix_sgr.push_str(text);
                                }
                            } else if is_ansi_csi {
                                // It's a regular CSI sequence.
                                // We should be mostly safe to just append these together.
                                ansi_prefix.push_str(text);
                            } else {
                                // It's probably a VT100 code.
                                // Passing it through is the safest bet.
                                write!(handle, "{}", text)?;
                            }
                        }

                        // Regular text.
                        (text, false) => {
                            let text = self.preprocess(
                                text.trim_end_matches(|c| c == '\r' || c == '\n'),
                                &mut cursor_total,
                            );

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
                                                    .map(|ref d| d
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
                                        "{}\n{}",
                                        as_terminal_escaped(
                                            style,
                                            &*format!(
                                                "{}{}{}",
                                                self.ansi_prefix_sgr, ansi_prefix, line_buf
                                            ),
                                            self.config.true_color,
                                            self.config.colored_output,
                                            self.config.use_italic_text,
                                            background_color
                                        ),
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
                                    &*format!(
                                        "{}{}{}",
                                        self.ansi_prefix_sgr, ansi_prefix, line_buf
                                    ),
                                    self.config.true_color,
                                    self.config.colored_output,
                                    self.config.use_italic_text,
                                    background_color
                                )
                            )?;

                            // Clear the ANSI prefix buffer.
                            ansi_prefix.clear();
                        }
                    }
                }
            }

            if let Some(background_color) = background_color {
                let mut ansi_style = Style::default();
                ansi_style.background =
                    Some(to_ansi_color(background_color, self.config.true_color));

                write!(
                    handle,
                    "{}",
                    ansi_style.paint(" ".repeat(cursor_max - cursor))
                )?;
            }
            writeln!(handle)?;
        }

        Ok(())
    }
}

const DEFAULT_GUTTER_COLOR: u8 = 238;

#[derive(Debug, Default)]
pub struct Colors {
    pub grid: Style,
    pub filename: Style,
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
        let gutter_color = theme
            .settings
            .gutter_foreground
            .map(|c| to_ansi_color(c, true_color))
            .unwrap_or(Fixed(DEFAULT_GUTTER_COLOR));

        Colors {
            grid: gutter_color.normal(),
            filename: Style::new().bold(),
            git_added: Green.normal(),
            git_removed: Red.normal(),
            git_modified: Yellow.normal(),
            line_number: gutter_color.normal(),
        }
    }
}

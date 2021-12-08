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

use crate::assets::{HighlightingAssets, SyntaxReferenceInSet};
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
use crate::vscreen::AnsiStyle;
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
            } else {
                handle.write_all(line_buffer)?
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

        let highlighter_from_set = if input
            .reader
            .content_type
            .map_or(false, |c| c.is_binary() && !config.show_nonprintable)
        {
            None
        } else {
            // Determine the type of syntax for highlighting
            let syntax_in_set =
                match assets.get_syntax(config.language, input, &config.syntax_mapping) {
                    Ok(syntax_in_set) => syntax_in_set,
                    Err(Error::UndetectedSyntax(_)) => assets
                        .find_syntax_by_name("Plain Text")?
                        .expect("A plain text syntax is available"),
                    Err(e) => return Err(e),
                };

            Some(HighlighterFromSet::new(syntax_in_set, theme))
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
        })
    }

    fn print_horizontal_line_term(&mut self, handle: &mut dyn Write, style: Style) -> Result<()> {
        writeln!(
            handle,
            "{}",
            style.paint("─".repeat(self.config.term_width))
        )?;
        Ok(())
    }

    fn print_horizontal_line(&mut self, handle: &mut dyn Write, grid_char: char) -> Result<()> {
        if self.panel_width == 0 {
            self.print_horizontal_line_term(handle, self.colors.grid)?;
        } else {
            let hline = "─".repeat(self.config.term_width - (self.panel_width + 1));
            let hline = format!("{}{}{}", "─".repeat(self.panel_width), grid_char, hline);
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

    fn preprocess(&self, text: &str, cursor: &mut usize) -> String {
        if self.config.tab_width > 0 {
            return expand_tabs(text, self.config.tab_width, cursor);
        }

        *cursor += text.len();
        text.to_string()
    }
}

impl<'a> Printer for InteractivePrinter<'a> {
    fn print_header(
        &mut self,
        handle: &mut dyn Write,
        input: &OpenedInput,
        add_header_padding: bool,
    ) -> Result<()> {
        if add_header_padding && self.config.style_components.rule() {
            self.print_horizontal_line_term(handle, self.colors.rule)?;
        }

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
            // Only pad space between files, if we haven't already drawn a horizontal rule
            if add_header_padding && !self.config.style_components.rule() {
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
                .unwrap_or_else(|| "".into()),
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
            replace_nonprintable(line_buffer, self.config.tab_width)
        } else {
            match self.content_type {
                Some(ContentType::BINARY) | None => {
                    return Ok(());
                }
                Some(ContentType::UTF_16LE) => UTF_16LE
                    .decode(line_buffer, DecoderTrap::Replace)
                    .map_err(|_| "Invalid UTF-16LE")?,
                Some(ContentType::UTF_16BE) => UTF_16BE
                    .decode(line_buffer, DecoderTrap::Replace)
                    .map_err(|_| "Invalid UTF-16BE")?,
                _ => String::from_utf8_lossy(line_buffer).to_string(),
            }
        };

        let regions = {
            let highlighter_from_set = match self.highlighter_from_set {
                Some(ref mut highlighter_from_set) => highlighter_from_set,
                _ => {
                    return Ok(());
                }
            };
            highlighter_from_set
                .highlighter
                .highlight(&line, highlighter_from_set.syntax_set)
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
                let ansi_iterator = AnsiCodeIterator::new(region);
                for chunk in ansi_iterator {
                    match chunk {
                        // ANSI escape passthrough.
                        (ansi, true) => {
                            self.ansi_style.update(ansi);
                            write!(handle, "{}", ansi)?;
                        }

                        // Regular text.
                        (text, false) => {
                            let text = &*self.preprocess(text, &mut cursor_total);
                            let text_trimmed = text.trim_end_matches(|c| c == '\r' || c == '\n');

                            write!(
                                handle,
                                "{}",
                                as_terminal_escaped(
                                    style,
                                    &format!("{}{}", self.ansi_style, text_trimmed),
                                    true_color,
                                    colored_output,
                                    italics,
                                    background_color
                                )
                            )?;

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
                    }
                }
            }

            if !self.config.style_components.plain() && line.bytes().next_back() != Some(b'\n') {
                writeln!(handle)?;
            }
        } else {
            for &(style, region) in &regions {
                let ansi_iterator = AnsiCodeIterator::new(region);
                for chunk in ansi_iterator {
                    match chunk {
                        // ANSI escape passthrough.
                        (ansi, true) => {
                            self.ansi_style.update(ansi);
                            write!(handle, "{}", ansi)?;
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
                                        "{}\n{}",
                                        as_terminal_escaped(
                                            style,
                                            &*format!("{}{}", self.ansi_style, line_buf),
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
                                    &*format!("{}{}", self.ansi_style, line_buf),
                                    self.config.true_color,
                                    self.config.colored_output,
                                    self.config.use_italic_text,
                                    background_color
                                )
                            )?;
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

        Ok(())
    }
}

const DEFAULT_GUTTER_COLOR: u8 = 238;

#[derive(Debug, Default)]
pub struct Colors {
    pub grid: Style,
    pub rule: Style,
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
            filename: Style::new().bold(),
            git_added: Green.normal(),
            git_removed: Red.normal(),
            git_modified: Yellow.normal(),
            line_number: gutter_style,
        }
    }
}

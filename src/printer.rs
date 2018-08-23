use std::boxed::Box;
use std::io::Write;
use std::vec::Vec;

use ansi_term::Colour::{Fixed, Green, Red, Yellow};
use ansi_term::Style;

use console::AnsiCodeIterator;

use syntect::easy::HighlightLines;
use syntect::highlighting::Theme;

use app::Config;
use assets::HighlightingAssets;
use decorations::{Decoration, GridBorderDecoration, LineChangesDecoration, LineNumberDecoration};
use diff::get_git_diff;
use diff::LineChanges;
use errors::*;
use style::OutputWrap;
use terminal::{as_terminal_escaped, to_ansi_color};

pub trait Printer {
    fn print_header(&mut self, handle: &mut Write, filename: Option<&str>) -> Result<()>;
    fn print_footer(&mut self, handle: &mut Write) -> Result<()>;
    fn print_line(&mut self, handle: &mut Write, line_number: usize, line: &str) -> Result<()>;
}

pub struct InteractivePrinter<'a> {
    colors: Colors,
    config: &'a Config<'a>,
    decorations: Vec<Box<Decoration>>,
    panel_width: usize,
    ansi_prefix_sgr: String,
    pub line_changes: Option<LineChanges>,
    highlighter: HighlightLines<'a>,
}

impl<'a> InteractivePrinter<'a> {
    pub fn new(config: &'a Config, assets: &'a HighlightingAssets, filename: Option<&str>) -> Self {
        let theme = assets.get_theme(&config.theme);

        let colors = if config.colored_output {
            Colors::colored(theme, config.true_color)
        } else {
            Colors::plain()
        };

        // Create decorations.
        let mut decorations: Vec<Box<Decoration>> = Vec::new();

        if config.output_components.numbers() {
            decorations.push(Box::new(LineNumberDecoration::new(&colors)));
        }

        if config.output_components.changes() {
            decorations.push(Box::new(LineChangesDecoration::new(&colors)));
        }

        let mut panel_width: usize =
            decorations.len() + decorations.iter().fold(0, |a, x| a + x.width());

        // The grid border decoration isn't added until after the panel_width calculation, since the
        // print_horizontal_line, print_header, and print_footer functions all assume the panel
        // width is without the grid border.
        if config.output_components.grid() && !decorations.is_empty() {
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

        // Get the Git modifications
        let line_changes = filename.and_then(|file| get_git_diff(file));

        // Determine the type of syntax for highlighting
        let syntax = assets.get_syntax(config.language, filename);
        let highlighter = HighlightLines::new(syntax, theme);

        InteractivePrinter {
            panel_width,
            colors,
            config,
            decorations,
            ansi_prefix_sgr: String::new(),
            line_changes,
            highlighter,
        }
    }

    fn print_horizontal_line(&mut self, handle: &mut Write, grid_char: char) -> Result<()> {
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
}

impl<'a> Printer for InteractivePrinter<'a> {
    fn print_header(&mut self, handle: &mut Write, filename: Option<&str>) -> Result<()> {
        if !self.config.output_components.header() {
            return Ok(());
        }

        if self.config.output_components.grid() {
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
            write!(handle, "{}", " ".repeat(self.panel_width))?;
        }

        writeln!(
            handle,
            "{}{}",
            filename.map_or("", |_| "File: "),
            self.colors.filename.paint(filename.unwrap_or("STDIN"))
        )?;

        if self.config.output_components.grid() {
            self.print_horizontal_line(handle, '┼')?;
        }

        Ok(())
    }

    fn print_footer(&mut self, handle: &mut Write) -> Result<()> {
        if self.config.output_components.grid() {
            self.print_horizontal_line(handle, '┴')
        } else {
            Ok(())
        }
    }

    fn print_line(&mut self, handle: &mut Write, line_number: usize, line: &str) -> Result<()> {
        let regions = self.highlighter.highlight(line.as_ref());

        let mut cursor: usize = 0;
        let mut cursor_max: usize = self.config.term_width;
        let mut panel_wrap: Option<String> = None;

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
        if self.config.output_wrap == OutputWrap::None {
            let true_color = self.config.true_color;
            let colored_output = self.config.colored_output;

            write!(
                handle,
                "{}",
                regions
                    .iter()
                    .map(|&(style, text)| as_terminal_escaped(
                        style,
                        text,
                        true_color,
                        colored_output,
                    )).collect::<Vec<_>>()
                    .join("")
            )?;
        } else {
            for &(style, region) in regions.iter() {
                let mut ansi_iterator = AnsiCodeIterator::new(region);
                let mut ansi_prefix: String = String::new();
                for chunk in ansi_iterator {
                    match chunk {
                        // ANSI escape passthrough.
                        (text, true) => {
                            if text.chars().last().map_or(false, |c| c == 'm') {
                                ansi_prefix.push_str(text);
                                if text == "\x1B[0m" {
                                    self.ansi_prefix_sgr = "\x1B[0m".to_owned();
                                } else {
                                    self.ansi_prefix_sgr.push_str(text);
                                }
                            } else {
                                ansi_prefix.push_str(text);
                            }
                        }

                        // Regular text.
                        (text, false) => {
                            let text = text.trim_right_matches(|c| c == '\r' || c == '\n');
                            let mut chars = text.chars();
                            let mut remaining = text.chars().count();

                            while remaining > 0 {
                                let available = cursor_max - cursor;

                                // It fits.
                                if remaining <= available {
                                    let text = chars.by_ref().take(remaining).collect::<String>();
                                    cursor += remaining;

                                    write!(
                                        handle,
                                        "{}",
                                        as_terminal_escaped(
                                            style,
                                            &*format!(
                                                "{}{}{}",
                                                self.ansi_prefix_sgr, ansi_prefix, text
                                            ),
                                            self.config.true_color,
                                            self.config.colored_output,
                                        )
                                    )?;
                                    break;
                                }

                                // Generate wrap padding if not already generated.
                                if panel_wrap.is_none() {
                                    panel_wrap = if self.panel_width > 0 {
                                        Some(format!(
                                            "{} ",
                                            self.decorations
                                                .iter()
                                                .map(|ref d| d
                                                    .generate(line_number, true, self)
                                                    .text).collect::<Vec<String>>()
                                                .join(" ")
                                        ))
                                    } else {
                                        Some("".to_string())
                                    }
                                }

                                // It wraps.
                                let text = chars.by_ref().take(available).collect::<String>();
                                cursor = 0;
                                remaining -= available;

                                write!(
                                    handle,
                                    "{}\n{}",
                                    as_terminal_escaped(
                                        style,
                                        &*format!(
                                            "{}{}{}",
                                            self.ansi_prefix_sgr, ansi_prefix, text
                                        ),
                                        self.config.true_color,
                                        self.config.colored_output,
                                    ),
                                    panel_wrap.clone().unwrap()
                                )?;
                            }

                            // Clear the ANSI prefix buffer.
                            ansi_prefix.clear();
                        }
                    }
                }
            }

            write!(handle, "\n")?;
        }

        Ok(())
    }
}

const DEFAULT_GUTTER_COLOR: u8 = 238;

#[derive(Default)]
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

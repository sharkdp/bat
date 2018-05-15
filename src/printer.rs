use app::Config;
use diff::LineChanges;
use errors::*;
use std::io::Write;
use std::vec::Vec;
use std::boxed::Box;
use syntect::highlighting;
use terminal::as_terminal_escaped;
use style::OutputWrap;
use decorations::{Decoration, GridBorderDecoration, LineChangesDecoration, LineNumberDecoration};
use Colors;

pub struct Printer<'a> {
    handle: &'a mut Write,
    colors: Colors,
    config: &'a Config<'a>,
    decorations: Vec<Box<Decoration>>,
    panel_width: usize,
    pub line_changes: Option<LineChanges>,
}

impl<'a> Printer<'a> {
    pub fn new(handle: &'a mut Write, config: &'a Config) -> Self {
        let colors = if config.colored_output {
            Colors::colored()
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
        if config.output_components.grid() && decorations.len() > 0 {
            decorations.push(Box::new(GridBorderDecoration::new(&colors)));
        }

        // Disable the panel if the terminal is too small (i.e. can't fit 5 characters with the
        // panel showing).
        if config.term_width < (decorations.len() + decorations.iter().fold(0, |a, x| a + x.width())) + 5 {
            decorations.clear();
            panel_width = 0;
        }

        // Create printer.
        Printer {
            panel_width,
            handle,
            colors,
            config,
            decorations,
            line_changes: None,
        }
    }

    pub fn print_header(&mut self, filename: Option<&str>) -> Result<()> {
        if !self.config.output_components.header() {
            return Ok(());
        }

        if self.config.output_components.grid() {
            self.print_horizontal_line('┬')?;

            write!(
                self.handle,
                "{}{} ",
                " ".repeat(self.panel_width),
                self.colors
                    .grid
                    .paint(if self.panel_width > 0 { "│" } else { "" }),
            )?;
        }

        writeln!(
            self.handle,
            "{}{}",
            filename.map_or("", |_| "File: "),
            self.colors.filename.paint(filename.unwrap_or("STDIN"))
        )?;

        if self.config.output_components.grid() {
            self.print_horizontal_line('┼')?;
        }

        Ok(())
    }

    pub fn print_footer(&mut self) -> Result<()> {
        if self.config.output_components.grid() {
            self.print_horizontal_line('┴')
        } else {
            Ok(())
        }
    }

    pub fn print_line(
        &mut self,
        line_number: usize,
        regions: &[(highlighting::Style, &str)],
    ) -> Result<()> {
        let mut cursor: usize = 0;
        let mut cursor_max: usize = self.config.term_width;
        let mut panel_wrap: Option<String> = None;

        // Line decorations.
        if self.panel_width > 0 {
            let decorations = self.decorations
                .iter()
                .map(|ref d| d.generate(line_number, false, self))
                .collect::<Vec<_>>();

            for deco in decorations {
                write!(self.handle, "{} ", deco.text)?;
                cursor_max -= deco.width + 1;
            }
        }

        // Line contents.
        if self.config.output_wrap == OutputWrap::None {
            let true_color = self.config.true_color;
            let colored_output = self.config.colored_output;

            write!(
                self.handle,
                "{}",
                regions
                    .iter()
                    .map(|&(style, text)| as_terminal_escaped(
                        style,
                        text,
                        true_color,
                        colored_output,
                    ))
                    .collect::<Vec<_>>()
                    .join("")
            )?;
        } else {
            for &(style, text) in regions.iter() {
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
                            self.handle,
                            "{}",
                            as_terminal_escaped(
                                style,
                                &*text,
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
                                    .map(|ref d| d.generate(line_number, true, self).text)
                                    .collect::<Vec<String>>()
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
                        self.handle,
                        "{}\n{}",
                        as_terminal_escaped(
                            style,
                            &*text,
                            self.config.true_color,
                            self.config.colored_output,
                        ),
                        panel_wrap.clone().unwrap()
                    )?;
                }
            }

            write!(self.handle, "\n")?;
        }

        Ok(())
    }

    fn print_horizontal_line(&mut self, grid_char: char) -> Result<()> {
        if self.panel_width == 0 {
            writeln!(
                self.handle,
                "{}",
                self.colors.grid.paint("─".repeat(self.config.term_width))
            )?;
        } else {
            let hline = "─".repeat(self.config.term_width - (self.panel_width + 1));
            let hline = format!("{}{}{}", "─".repeat(self.panel_width), grid_char, hline);
            writeln!(self.handle, "{}", self.colors.grid.paint(hline))?;
        }

        Ok(())
    }
}

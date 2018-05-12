use ansi_term::Style;
use app::Config;
use diff::{LineChange, LineChanges};
use errors::*;
use std::io::Write;
use syntect::highlighting;
use terminal::as_terminal_escaped;
use style::OutputWrap;
use Colors;

const LINE_NUMBER_WIDTH: usize = 4;

struct PrintSegment {
    size: usize,
    text: String,
}

pub struct Printer<'a> {
    handle: &'a mut Write,
    colors: Colors,
    config: &'a Config<'a>,
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

        // Create the instance.
        let mut instance = Printer {
            handle,
            colors,
            config,
            panel_width: 0,
            line_changes: None,
        };

        // Generate the panel (gutter) width.
        let decorations = instance.gen_decorations(0);
        instance.panel_width = decorations.len() + decorations.iter().fold(0, |a, x| a + x.size);

        // Return the instance.
        return instance;
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

        // Line decoration.
        let decorations = self.gen_decorations(line_number);
        let gutter_width = decorations.len() + decorations.iter().fold(0, |a, x| a + x.size);

        if gutter_width > 0 {
            cursor_max -= gutter_width;
            write!(
                self.handle,
                "{} ",
                decorations
                    .iter()
                    .map(|seg| seg.text.to_owned())
                    .collect::<Vec<String>>()
                    .join(" ")
            )?;
        }

        // Grid border.
        let border = if gutter_width > 0 && self.config.output_components.grid() {
            self.gen_border()
        } else {
            PrintSegment {
                size: 0,
                text: "".to_owned(),
            }
        };

        cursor_max -= border.size;
        write!(self.handle, "{}", border.text)?;

        // Line contents.
        if self.config.output_wrap == OutputWrap::None {
            let true_color = self.config.true_color;
            let colored_output = self.config.colored_output;

            write!(self.handle, "{}",
                   regions.iter()
                       .map(|&(style, text)| as_terminal_escaped(style, text, true_color, colored_output))
                       .collect::<Vec<_>>()
                       .join(" ")
            )?;
        } else {
            for &(style, text) in regions.iter() {
                let mut chars = text.chars().filter(|c| *c != '\n');
                let mut remaining = text.chars().filter(|c| *c != '\n').count();

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

                    // It wraps.
                    if self.config.output_wrap == OutputWrap::Character {
                        let text = chars.by_ref().take(available).collect::<String>();
                        cursor = 0;
                        remaining -= available;

                        write!(
                            self.handle,
                            "{}\n{}{}",
                            as_terminal_escaped(
                                style,
                                &*text,
                                self.config.true_color,
                                self.config.colored_output,
                            ),
                            " ".repeat(gutter_width),
                            border.text.to_owned()
                        )?;

                        continue;
                    }
                }
            }

            write!(self.handle, "\n")?;
        }

        // Finished.
        Ok(())
    }

    /// Generates all the line decorations.
    fn gen_decorations(&self, line_number: usize) -> Vec<PrintSegment> {
        let mut decorations = Vec::new();

        if self.config.output_components.numbers() {
            decorations.push(self.gen_deco_line_number(line_number));
        }

        if self.config.output_components.changes() {
            decorations.push(self.gen_deco_line_changes(line_number));
        }

        return decorations;
    }

    /// Generates the decoration for displaying the line number.
    fn gen_deco_line_number(&self, line_number: usize) -> PrintSegment {
        let plain: String = format!("{:width$}", line_number, width = LINE_NUMBER_WIDTH);
        let color = self.colors.line_number.paint(plain.to_owned());

        return PrintSegment {
            text: color.to_string(),
            size: plain.len(),
        };
    }

    /// Generates the decoration for displaying the git changes.
    fn gen_deco_line_changes(&self, line_number: usize) -> PrintSegment {
        let color = if let Some(ref changes) = self.line_changes {
            match changes.get(&(line_number as u32)) {
                Some(&LineChange::Added) => self.colors.git_added.paint("+"),
                Some(&LineChange::RemovedAbove) => self.colors.git_removed.paint("‾"),
                Some(&LineChange::RemovedBelow) => self.colors.git_removed.paint("_"),
                Some(&LineChange::Modified) => self.colors.git_modified.paint("~"),
                _ => Style::default().paint(" "),
            }
        } else {
            Style::default().paint(" ")
        };

        return PrintSegment {
            text: color.to_string(),
            size: 1,
        };
    }

    /// Generates the vertical grid border.
    fn gen_border(&self) -> PrintSegment {
        return PrintSegment {
            text: self.colors.grid.paint("│ ").to_string(),
            size: 2,
        };
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

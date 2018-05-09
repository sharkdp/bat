use ansi_term::Style;
use errors::*;
use std::io::Write;
use syntect::highlighting;
use terminal::as_terminal_escaped;
use {Colors, LineChange, LineChanges, Options};

const PANEL_WIDTH: usize = 7;

pub struct Printer<'a> {
    handle: &'a mut Write,
    colors: Colors,
    options: &'a Options<'a>,
    pub line_changes: Option<LineChanges>,
}

impl<'a> Printer<'a> {
    pub fn new(handle: &'a mut Write, options: &'a Options) -> Self {
        let colors = if options.colored_output {
            Colors::colored()
        } else {
            Colors::plain()
        };

        Printer {
            handle,
            colors,
            options,
            line_changes: None,
        }
    }

    pub fn print_header(&mut self, filename: Option<&str>) -> Result<()> {
        if !self.options.output_components.header() {
            return Ok(());
        }

        if self.options.output_components.grid() {
            self.print_horizontal_line('┬')?;

            write!(
                self.handle,
                "{}{} ",
                " ".repeat(PANEL_WIDTH),
                self.colors.grid.paint("│"),
            )?;

            writeln!(
                self.handle,
                "{}{}",
                filename.map_or("", |_| "File: "),
                self.colors.filename.paint(filename.unwrap_or("STDIN"))
            )?;

            self.print_horizontal_line('┼')
        } else {
            writeln!(
                self.handle,
                "File {}",
                self.colors.filename.paint(filename.unwrap_or("STDIN"))
            ).map_err(Into::into)
        }
    }

    pub fn print_footer(&mut self) -> Result<()> {
        if self.options.output_components.grid() {
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
        let decorations = vec![
            self.print_line_number(line_number),
            self.print_git_marker(line_number),
            self.print_line_border(),
            Some(as_terminal_escaped(
                &regions,
                self.options.true_color,
                self.options.colored_output,
            )),
        ];

        let grid_requested = self.options.output_components.grid();
        write!(
            self.handle,
            "{}",
            decorations
                .into_iter()
                .filter_map(|dec| if grid_requested {
                    Some(dec.unwrap_or(" ".to_owned()))
                } else {
                    dec
                })
                .collect::<Vec<_>>()
                .join(" ")
        )?;

        Ok(())
    }

    fn print_line_number<'s>(&self, line_number: usize) -> Option<String> {
        if self.options.output_components.numbers() {
            Some(
                self.colors
                    .line_number
                    .paint(format!("{:4}", line_number))
                    .to_string(),
            )
        } else if self.options.output_components.grid() {
            Some("    ".to_owned())
        } else {
            None
        }
    }

    fn print_git_marker<'s>(&self, line_number: usize) -> Option<String> {
        if self.options.output_components.changes() {
            Some(
                if let Some(ref changes) = self.line_changes {
                    match changes.get(&(line_number as u32)) {
                        Some(&LineChange::Added) => self.colors.git_added.paint("+"),
                        Some(&LineChange::RemovedAbove) => self.colors.git_removed.paint("‾"),
                        Some(&LineChange::RemovedBelow) => self.colors.git_removed.paint("_"),
                        Some(&LineChange::Modified) => self.colors.git_modified.paint("~"),
                        _ => Style::default().paint(" "),
                    }
                } else {
                    Style::default().paint(" ")
                }.to_string(),
            )
        } else if self.options.output_components.grid() {
            Some(" ".to_owned())
        } else {
            None
        }
    }

    fn print_line_border<'s>(&self) -> Option<String> {
        if self.options.output_components.grid() {
            Some(self.colors.grid.paint("│").to_string())
        } else {
            None
        }
    }

    fn print_horizontal_line(&mut self, grid_char: char) -> Result<()> {
        let hline = "─".repeat(self.options.term_width - (PANEL_WIDTH + 1));
        let hline = format!("{}{}{}", "─".repeat(PANEL_WIDTH), grid_char, hline);

        writeln!(self.handle, "{}", self.colors.grid.paint(hline))?;

        Ok(())
    }
}

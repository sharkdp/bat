use ansi_term::Style;
use console::Term;
use errors::*;
use std::io::Write;
use syntect::highlighting;
use terminal::as_terminal_escaped;
use {Colors, LineChange, LineChanges, Options, OptionsStyle};

const PANEL_WIDTH: usize = 7;

pub struct Printer<'a> {
    handle: &'a mut Write,
    colors: Colors,
    term_width: usize,
    options: &'a Options<'a>,
    pub line_changes: Option<LineChanges>,
}

impl<'a> Printer<'a> {
    pub fn new(handle: &'a mut Write, options: &'a Options) -> Self {
        let (_, term_width) = Term::stdout().size();
        let term_width = term_width as usize;

        let colors = if options.colored_output {
            Colors::colored()
        } else {
            Colors::plain()
        };

        Printer {
            handle,
            colors,
            term_width,
            options,
            line_changes: None,
        }
    }

    pub fn print_header(&mut self, filename: Option<&str>) -> Result<()> {
        match self.options.style {
            OptionsStyle::Full => {}
            _ => return Ok(()),
        }

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
    }

    pub fn print_footer(&mut self) -> Result<()> {
        if let OptionsStyle::Full = self.options.style {
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

        write!(
            self.handle,
            "{}",
            decorations
                .into_iter()
                .filter_map(|dec| dec)
                .collect::<Vec<_>>()
                .join(" ")
        )?;

        Ok(())
    }

    fn print_line_number(&self, line_number: usize) -> Option<String> {
        if let OptionsStyle::Plain = self.options.style {
            return None;
        }

        Some(
            self.colors
                .line_number
                .paint(format!("{:4}", line_number))
                .to_string(),
        )
    }

    fn print_git_marker(&self, line_number: usize) -> Option<String> {
        match self.options.style {
            OptionsStyle::Full => {}
            _ => return None,
        }

        let marker = if let Some(ref changes) = self.line_changes {
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

        Some(marker.to_string())
    }

    fn print_line_border(&self) -> Option<String> {
        if let OptionsStyle::Plain = self.options.style {
            return None;
        }

        Some(self.colors.grid.paint("│").to_string())
    }

    fn print_horizontal_line(&mut self, grid_char: char) -> Result<()> {
        let hline = "─".repeat(self.term_width - (PANEL_WIDTH + 1));
        let hline = format!("{}{}{}", "─".repeat(PANEL_WIDTH), grid_char, hline);

        writeln!(self.handle, "{}", self.colors.grid.paint(hline))?;

        Ok(())
    }
}

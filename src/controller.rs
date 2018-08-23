use std::fs::File;
use std::io::{self, BufRead, BufReader, Write};

use app::Config;
use assets::HighlightingAssets;
use errors::*;
use line_range::LineRange;
use output::OutputType;
use printer::{InteractivePrinter, Printer};

pub struct Controller<'a> {
    config: &'a Config<'a>,
    assets: &'a HighlightingAssets,
}

impl<'b> Controller<'b> {
    pub fn new<'a>(config: &'a Config, assets: &'a HighlightingAssets) -> Controller<'a> {
        Controller { config, assets }
    }

    pub fn run(&self) -> Result<bool> {
        let mut output_type = OutputType::from_mode(self.config.paging_mode);
        let writer = output_type.handle()?;
        let mut no_errors: bool = true;

        for file in &self.config.files {
            let result = self.print_file(writer, *file);

            if let Err(error) = result {
                handle_error(&error);
                no_errors = false;
            }
        }

        Ok(no_errors)
    }

    fn print_file(&self, writer: &mut Write, filename: Option<&str>) -> Result<()> {
        let mut printer = InteractivePrinter::new(&self.config, &self.assets, filename);

        let stdin = io::stdin();
        {
            let reader: Box<BufRead> = match filename {
                None => Box::new(stdin.lock()),
                Some(filename) => Box::new(BufReader::new(File::open(filename)?)),
            };

            printer.print_header(writer, filename)?;
            self.print_file_ranges(&mut printer, writer, reader, &self.config.line_range)?;
            printer.print_footer(writer)?;
        }
        Ok(())
    }

    fn print_file_ranges<'a, P: Printer>(
        &self,
        printer: &mut P,
        writer: &mut Write,
        mut reader: Box<BufRead + 'a>,
        line_ranges: &Option<LineRange>,
    ) -> Result<()> {
        let mut buffer = Vec::new();

        let mut line_number: usize = 1;

        while reader.read_until(b'\n', &mut buffer)? > 0 {
            {
                let line = String::from_utf8_lossy(&buffer);

                match line_ranges {
                    &Some(ref range) => {
                        if line_number < range.lower {
                            // skip line
                        } else if line_number > range.upper {
                            // no more lines in range
                            break;
                        } else {
                            printer.print_line(writer, line_number, &line)?;
                        }
                    }
                    &None => {
                        printer.print_line(writer, line_number, &line)?;
                    }
                }

                line_number += 1;
            }
            buffer.clear();
        }
        Ok(())
    }
}

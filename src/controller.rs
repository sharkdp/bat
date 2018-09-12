use std::fs::File;
use std::io::{self, BufRead, BufReader, Write};

use app::{Config, InputFile};
use assets::HighlightingAssets;
use errors::*;
use line_range::LineRange;
use output::OutputType;
use printer::{InteractivePrinter, Printer, SimplePrinter};

const THEME_PREVIEW_FILE: &[u8] = include_bytes!("../assets/theme_preview.rs");

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

        for filename in &self.config.files {
            let result = if self.config.loop_through {
                let mut printer = SimplePrinter::new();
                self.print_file(&mut printer, writer, *filename)
            } else {
                let mut printer = InteractivePrinter::new(&self.config, &self.assets, *filename);
                self.print_file(&mut printer, writer, *filename)
            };

            if let Err(error) = result {
                handle_error(&error);
                no_errors = false;
            }
        }

        Ok(no_errors)
    }

    fn print_file<'a, P: Printer>(
        &self,
        printer: &mut P,
        writer: &mut Write,
        filename: InputFile<'a>,
    ) -> Result<()> {
        let stdin = io::stdin();
        {
            let reader: Box<BufRead> = match filename {
                InputFile::StdIn => Box::new(stdin.lock()),
                InputFile::Ordinary(filename) => {
                    let file = File::open(filename)?;

                    if file.metadata()?.is_dir() {
                        return Err(format!("'{}' is a directory.", filename).into());
                    }

                    Box::new(BufReader::new(file))
                }
                InputFile::ThemePreviewFile => Box::new(THEME_PREVIEW_FILE),
            };

            printer.print_header(writer, filename)?;
            self.print_file_ranges(printer, writer, reader, &self.config.line_range)?;
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
        let mut line_buffer = Vec::new();

        let mut line_number: usize = 1;

        while reader.read_until(b'\n', &mut line_buffer)? > 0 {
            {
                match line_ranges {
                    &Some(ref range) => {
                        if line_number < range.lower {
                            // Call the printer in case we need to call the syntax highlighter
                            // for this line. However, set `out_of_range` to `true`.
                            printer.print_line(true, writer, line_number, &line_buffer)?;
                        } else if line_number > range.upper {
                            // no more lines in range, exit early
                            break;
                        } else {
                            printer.print_line(false, writer, line_number, &line_buffer)?;
                        }
                    }
                    &None => {
                        printer.print_line(false, writer, line_number, &line_buffer)?;
                    }
                }

                line_number += 1;
            }
            line_buffer.clear();
        }
        Ok(())
    }
}

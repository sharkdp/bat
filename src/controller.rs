use std::io::{self, Write};
use std::path::Path;

use app::{Config, PagingMode};
use assets::HighlightingAssets;
use errors::*;
use inputfile::{InputFile, InputFileReader};
use line_range::{LineRanges, RangeCheckResult};
use output::OutputType;
use printer::{InteractivePrinter, Printer, SimplePrinter};

pub struct Controller<'a> {
    config: &'a Config<'a>,
    assets: &'a HighlightingAssets,
}

impl<'b> Controller<'b> {
    pub fn new<'a>(config: &'a Config, assets: &'a HighlightingAssets) -> Controller<'a> {
        Controller { config, assets }
    }

    pub fn run(&self) -> Result<bool> {
        // Do not launch the pager if NONE of the input files exist
        let mut paging_mode = self.config.paging_mode;
        if self.config.paging_mode != PagingMode::Never {
            let call_pager = self.config.files.iter().any(|file| {
                if let InputFile::Ordinary(path) = file {
                    return Path::new(path).exists();
                } else {
                    return true;
                }
            });
            if !call_pager {
                paging_mode = PagingMode::Never;
            }
        }

        let mut output_type = OutputType::from_mode(paging_mode, self.config.pager)?;
        let writer = output_type.handle()?;
        let mut no_errors: bool = true;

        let stdin = io::stdin();

        for input_file in &self.config.files {
            match input_file.get_reader(&stdin) {
                Err(error) => {
                    handle_error(&error);
                    no_errors = false;
                }
                Ok(mut reader) => {
                    let result = if self.config.loop_through {
                        let mut printer = SimplePrinter::new();
                        self.print_file(reader, &mut printer, writer, *input_file)
                    } else {
                        let mut printer = InteractivePrinter::new(
                            &self.config,
                            &self.assets,
                            *input_file,
                            &mut reader,
                        );
                        self.print_file(reader, &mut printer, writer, *input_file)
                    };

                    if let Err(error) = result {
                        handle_error(&error);
                        no_errors = false;
                    }
                }
            }
        }

        Ok(no_errors)
    }

    fn print_file<'a, P: Printer>(
        &self,
        reader: InputFileReader,
        printer: &mut P,
        writer: &mut Write,
        input_file: InputFile<'a>,
    ) -> Result<()> {
        printer.print_header(writer, input_file)?;
        self.print_file_ranges(printer, writer, reader, &self.config.line_ranges)?;
        printer.print_footer(writer)?;

        Ok(())
    }

    fn print_file_ranges<'a, P: Printer>(
        &self,
        printer: &mut P,
        writer: &mut Write,
        mut reader: InputFileReader,
        line_ranges: &LineRanges,
    ) -> Result<()> {
        let mut line_buffer = Vec::new();
        let mut line_number: usize = 1;

        while reader.read_line(&mut line_buffer)? {
            match line_ranges.check(line_number) {
                RangeCheckResult::OutsideRange => {
                    // Call the printer in case we need to call the syntax highlighter
                    // for this line. However, set `out_of_range` to `true`.
                    printer.print_line(true, writer, line_number, &line_buffer)?;
                }
                RangeCheckResult::InRange => {
                    printer.print_line(false, writer, line_number, &line_buffer)?;
                }
                RangeCheckResult::AfterLastRange => {
                    break;
                }
            }

            line_number += 1;
            line_buffer.clear();
        }
        Ok(())
    }
}

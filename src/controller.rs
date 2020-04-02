use std::io::{self, Write};
use std::path::Path;

use crate::assets::HighlightingAssets;
use crate::config::{Config, PagingMode};
use crate::errors::*;
use crate::inputfile::{InputFile, InputFileReader};
use crate::line_range::{LineRanges, RangeCheckResult};
use crate::output::OutputType;
use crate::printer::{InteractivePrinter, Printer, SimplePrinter};

pub struct Controller<'a> {
    config: &'a Config<'a>,
    assets: &'a HighlightingAssets,
}

impl<'b> Controller<'b> {
    pub fn new<'a>(config: &'a Config, assets: &'a HighlightingAssets) -> Controller<'a> {
        Controller { config, assets }
    }

    pub fn run(&self) -> Result<bool> {
        self.run_with_error_handler(default_error_handler)
    }

    pub fn run_with_error_handler(&self, handle_error: impl Fn(&Error)) -> Result<bool> {
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

        let no_errors = self.run_with_writer(writer, handle_error);

        Ok(no_errors)
    }

    pub fn run_with_writer(&self, mut writer: &mut dyn Write, handle_error: impl Fn(&Error)) -> bool {
        let mut no_errors: bool = true;
        let stdin = io::stdin();

        let filenames: Box<dyn Iterator<Item = _>> = match self.config.filenames {
            Some(ref filenames) => Box::new(filenames.into_iter().map(|name| Some(*name))),
            None => Box::new(std::iter::repeat(None)),
        };

        for (input_file, file_name) in self.config.files.iter().zip(filenames) {
            match input_file.get_reader(&stdin) {
                Err(error) => {
                    handle_error(&error);
                    no_errors = false;
                }
                Ok(mut reader) => {
                    let result = if self.config.loop_through {
                        let mut printer = SimplePrinter::new();
                        self.print_file(reader, &mut printer, &mut writer, *input_file, file_name)
                    } else {
                        let mut printer = InteractivePrinter::new(
                            &self.config,
                            &self.assets,
                            *input_file,
                            &mut reader,
                        );
                        self.print_file(reader, &mut printer, &mut writer, *input_file, file_name)
                    };

                    if let Err(error) = result {
                        handle_error(&error);
                        no_errors = false;
                    }
                }
            }
        }

        no_errors
    }

    fn print_file<'a, P: Printer>(
        &self,
        reader: InputFileReader,
        printer: &mut P,
        writer: &mut dyn Write,
        input_file: InputFile<'a>,
        file_name: Option<&str>,
    ) -> Result<()> {
        if !reader.first_line.is_empty() || self.config.style_components.header() {
            printer.print_header(writer, input_file, file_name)?;
        }

        if !reader.first_line.is_empty() {
            self.print_file_ranges(printer, writer, reader, &self.config.line_ranges)?;
        }
        printer.print_footer(writer)?;

        Ok(())
    }

    fn print_file_ranges<P: Printer>(
        &self,
        printer: &mut P,
        writer: &mut dyn Write,
        mut reader: InputFileReader,
        line_ranges: &LineRanges,
    ) -> Result<()> {
        let mut line_buffer = Vec::new();
        let mut line_number: usize = 1;

        let mut first_range: bool = true;
        let mut mid_range: bool = false;

        while reader.read_line(&mut line_buffer)? {
            match line_ranges.check(line_number) {
                RangeCheckResult::BeforeOrBetweenRanges => {
                    // Call the printer in case we need to call the syntax highlighter
                    // for this line. However, set `out_of_range` to `true`.
                    printer.print_line(true, writer, line_number, &line_buffer)?;
                    mid_range = false;
                }

                RangeCheckResult::InRange => {
                    if self.config.style_components.snip() {
                        if first_range {
                            first_range = false;
                            mid_range = true;
                        } else if !mid_range {
                            mid_range = true;
                            printer.print_snip(writer)?;
                        }
                    }

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

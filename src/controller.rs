use std::io::{self, Write};

use crate::assets::HighlightingAssets;
use crate::config::Config;
#[cfg(feature = "paging")]
use crate::config::PagingMode;
use crate::errors::*;
use crate::input::{Input, InputReader};
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

    pub fn run(&self, inputs: Vec<Input>) -> Result<bool> {
        self.run_with_error_handler(inputs, default_error_handler)
    }

    pub fn run_with_error_handler(
        &self,
        inputs: Vec<Input>,
        handle_error: impl Fn(&Error),
    ) -> Result<bool> {
        let mut output_type;

        #[cfg(feature = "paging")]
        {
            use std::path::Path;

            // Do not launch the pager if NONE of the input files exist
            let mut paging_mode = self.config.paging_mode;
            if self.config.paging_mode != PagingMode::Never {
                let call_pager = inputs.iter().any(|file| {
                    if let Input::Ordinary(ofile) = file {
                        return Path::new(ofile.provided_path()).exists();
                    } else {
                        return true;
                    }
                });
                if !call_pager {
                    paging_mode = PagingMode::Never;
                }
            }
            output_type = OutputType::from_mode(paging_mode, self.config.pager)?;
        }

        #[cfg(not(feature = "paging"))]
        {
            output_type = OutputType::stdout();
        }

        let writer = output_type.handle()?;
        let mut no_errors: bool = true;

        for input in inputs.into_iter() {
            match input.get_reader(io::stdin().lock()) {
                Err(error) => {
                    handle_error(&error);
                    no_errors = false;
                }
                Ok(mut reader) => {
                    let result = if self.config.loop_through {
                        let mut printer = SimplePrinter::new();
                        self.print_file(reader, &mut printer, writer, &input)
                    } else {
                        let mut printer = InteractivePrinter::new(
                            &self.config,
                            &self.assets,
                            &input,
                            &mut reader,
                        );
                        self.print_file(reader, &mut printer, writer, &input)
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
        reader: InputReader,
        printer: &mut P,
        writer: &mut dyn Write,
        input: &Input,
    ) -> Result<()> {
        if !reader.first_line.is_empty() || self.config.style_components.header() {
            printer.print_header(writer, input)?;
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
        mut reader: InputReader,
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

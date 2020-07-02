use std::io::{self, Write};

use crate::assets::HighlightingAssets;
use crate::config::{Config, VisibleLines};
#[cfg(feature = "git")]
use crate::diff::{get_git_diff, LineChanges};
use crate::error::*;
use crate::input::{Input, InputReader, OpenedInput};
#[cfg(feature = "git")]
use crate::line_range::LineRange;
use crate::line_range::{LineRanges, RangeCheckResult};
use crate::output::OutputType;
#[cfg(feature = "paging")]
use crate::paging::PagingMode;
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
        handle_error: impl Fn(&Error, &mut dyn Write),
    ) -> Result<bool> {
        let mut output_type;

        #[cfg(feature = "paging")]
        {
            use crate::input::InputKind;
            use std::path::Path;

            // Do not launch the pager if NONE of the input files exist
            let mut paging_mode = self.config.paging_mode;
            if self.config.paging_mode != PagingMode::Never {
                let call_pager = inputs.iter().any(|ref input| {
                    if let InputKind::OrdinaryFile(ref path) = input.kind {
                        Path::new(path).exists()
                    } else {
                        true
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

        let attached_to_pager = output_type.is_pager();
        let writer = output_type.handle()?;
        let mut no_errors: bool = true;

        let stderr = io::stderr();
        let print_error = |error: &Error, write: &mut dyn Write| {
            if attached_to_pager {
                handle_error(error, write);
            } else {
                handle_error(error, &mut stderr.lock());
            }
        };

        for (index, input) in inputs.into_iter().enumerate() {
            match input.open(io::stdin().lock()) {
                Err(error) => {
                    print_error(&error, writer);
                    no_errors = false;
                }
                Ok(mut opened_input) => {
                    #[cfg(feature = "git")]
                    let line_changes = if self.config.visible_lines.diff_mode()
                        || (!self.config.loop_through && self.config.style_components.changes())
                    {
                        match opened_input.kind {
                            crate::input::OpenedInputKind::OrdinaryFile(ref path) => {
                                let diff = get_git_diff(path);

                                // Skip files without Git modifications
                                if self.config.visible_lines.diff_mode()
                                    && diff
                                        .as_ref()
                                        .map(|changes| changes.is_empty())
                                        .unwrap_or(false)
                                {
                                    continue;
                                }

                                diff
                            }
                            _ if self.config.visible_lines.diff_mode() => {
                                // Skip non-file inputs in diff mode
                                continue;
                            }
                            _ => None,
                        }
                    } else {
                        None
                    };

                    let mut printer: Box<dyn Printer> = if self.config.loop_through {
                        Box::new(SimplePrinter::new(&self.config))
                    } else {
                        Box::new(InteractivePrinter::new(
                            &self.config,
                            &self.assets,
                            &mut opened_input,
                            #[cfg(feature = "git")]
                            &line_changes,
                        )?)
                    };

                    let result = self.print_file(
                        &mut *printer,
                        writer,
                        &mut opened_input,
                        index != 0,
                        #[cfg(feature = "git")]
                        &line_changes,
                    );

                    if let Err(error) = result {
                        print_error(&error, writer);
                        no_errors = false;
                    }
                }
            }
        }

        Ok(no_errors)
    }

    fn print_file<'a>(
        &self,
        printer: &mut dyn Printer,
        writer: &mut dyn Write,
        input: &mut OpenedInput,
        add_header_padding: bool,
        #[cfg(feature = "git")] line_changes: &Option<LineChanges>,
    ) -> Result<()> {
        if !input.reader.first_line.is_empty() || self.config.style_components.header() {
            printer.print_header(writer, input, add_header_padding)?;
        }

        if !input.reader.first_line.is_empty() {
            let line_ranges = match self.config.visible_lines {
                VisibleLines::Ranges(ref line_ranges) => line_ranges.clone(),
                #[cfg(feature = "git")]
                VisibleLines::DiffContext(context) => {
                    let mut line_ranges: Vec<LineRange> = vec![];

                    if let Some(line_changes) = line_changes {
                        for line in line_changes.keys() {
                            let line = *line as usize;
                            line_ranges.push(LineRange::new(line - context, line + context));
                        }
                    }

                    LineRanges::from(line_ranges)
                }
            };

            self.print_file_ranges(printer, writer, &mut input.reader, &line_ranges)?;
        }
        printer.print_footer(writer, input)?;

        Ok(())
    }

    fn print_file_ranges(
        &self,
        printer: &mut dyn Printer,
        writer: &mut dyn Write,
        reader: &mut InputReader,
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

use crate::assets::HighlightingAssets;
use crate::config::{Config, VisibleLines};
#[cfg(feature = "git")]
use crate::diff::{get_git_diff, LineChanges};
use crate::error::*;
use crate::input::{Input, InputReader, OpenedInput};
#[cfg(feature = "lessopen")]
use crate::lessopen::LessOpenPreprocessor;
#[cfg(feature = "git")]
use crate::line_range::LineRange;
use crate::line_range::{LineRanges, MaxBufferedLineNumber, RangeCheckResult};
use crate::output::{OutputHandle, OutputType};
#[cfg(feature = "paging")]
use crate::paging::PagingMode;
use crate::printer::{InteractivePrinter, Printer, SimplePrinter};
use std::collections::VecDeque;
use std::io::{self, BufRead, Write};
use std::mem;

use clircle::{Clircle, Identifier};

pub struct Controller<'a> {
    config: &'a Config<'a>,
    assets: &'a HighlightingAssets,
    #[cfg(feature = "lessopen")]
    preprocessor: Option<LessOpenPreprocessor>,
}

impl Controller<'_> {
    pub fn new<'a>(config: &'a Config, assets: &'a HighlightingAssets) -> Controller<'a> {
        Controller {
            config,
            assets,
            #[cfg(feature = "lessopen")]
            preprocessor: LessOpenPreprocessor::new().ok(),
        }
    }

    pub fn run(&self, inputs: Vec<Input>, output_handle: Option<OutputHandle<'_>>) -> Result<bool> {
        self.run_with_error_handler(inputs, output_handle, default_error_handler)
    }

    pub fn run_with_error_handler(
        &self,
        inputs: Vec<Input>,
        output_handle: Option<OutputHandle<'_>>,
        mut handle_error: impl FnMut(&Error, &mut dyn Write),
    ) -> Result<bool> {
        let mut output_type;

        #[cfg(feature = "paging")]
        {
            use crate::input::InputKind;
            use std::path::Path;

            // Do not launch the pager if NONE of the input files exist
            let mut paging_mode = self.config.paging_mode;
            if self.config.paging_mode != PagingMode::Never {
                let call_pager = inputs.iter().any(|input| {
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

            let wrapping_mode = self.config.wrapping_mode;

            output_type = OutputType::from_mode(paging_mode, wrapping_mode, self.config.pager)?;
        }

        #[cfg(not(feature = "paging"))]
        {
            output_type = OutputType::stdout();
        }

        let attached_to_pager = output_type.is_pager();
        let stdout_identifier = if cfg!(windows) || attached_to_pager {
            None
        } else {
            clircle::Identifier::stdout()
        };

        let mut writer = match output_handle {
            Some(OutputHandle::FmtWrite(w)) => OutputHandle::FmtWrite(w),
            Some(OutputHandle::IoWrite(w)) => OutputHandle::IoWrite(w),
            None => OutputHandle::IoWrite(output_type.handle()?),
        };
        let mut no_errors: bool = true;
        let stderr = io::stderr();

        for (index, input) in inputs.into_iter().enumerate() {
            let identifier = stdout_identifier.as_ref();
            let is_first = index == 0;
            let result = if input.is_stdin() {
                self.print_input(input, &mut writer, io::stdin().lock(), identifier, is_first)
            } else {
                // Use dummy stdin since stdin is actually not used (#1902)
                self.print_input(input, &mut writer, io::empty(), identifier, is_first)
            };
            if let Err(error) = result {
                match writer {
                    // It doesn't make much sense to send errors straight to stderr if the user
                    // provided their own buffer, so we just return it.
                    OutputHandle::FmtWrite(_) => return Err(error),
                    OutputHandle::IoWrite(ref mut writer) => {
                        if attached_to_pager {
                            handle_error(&error, writer);
                        } else {
                            handle_error(&error, &mut stderr.lock());
                        }
                    }
                }
                no_errors = false;
            }
        }

        Ok(no_errors)
    }

    fn print_input<R: BufRead>(
        &self,
        input: Input,
        writer: &mut OutputHandle,
        stdin: R,
        stdout_identifier: Option<&Identifier>,
        is_first: bool,
    ) -> Result<()> {
        let mut opened_input = {
            #[cfg(feature = "lessopen")]
            match self.preprocessor {
                Some(ref preprocessor) if self.config.use_lessopen => {
                    preprocessor.open(input, stdin, stdout_identifier)?
                }
                _ => input.open(stdin, stdout_identifier)?,
            }

            #[cfg(not(feature = "lessopen"))]
            input.open(stdin, stdout_identifier)?
        };
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
                        return Ok(());
                    }

                    diff
                }
                _ if self.config.visible_lines.diff_mode() => {
                    // Skip non-file inputs in diff mode
                    return Ok(());
                }
                _ => None,
            }
        } else {
            None
        };

        let mut printer: Box<dyn Printer> = if self.config.loop_through {
            Box::new(SimplePrinter::new(self.config))
        } else {
            Box::new(InteractivePrinter::new(
                self.config,
                self.assets,
                &mut opened_input,
                #[cfg(feature = "git")]
                &line_changes,
            )?)
        };

        self.print_file(
            &mut *printer,
            writer,
            &mut opened_input,
            !is_first,
            #[cfg(feature = "git")]
            &line_changes,
        )
    }

    fn print_file(
        &self,
        printer: &mut dyn Printer,
        writer: &mut OutputHandle,
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
                        for &line in line_changes.keys() {
                            let line = line as usize;
                            line_ranges
                                .push(LineRange::new(line.saturating_sub(context), line + context));
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
        writer: &mut OutputHandle,
        reader: &mut InputReader,
        line_ranges: &LineRanges,
    ) -> Result<()> {
        let mut current_line_buffer: Vec<u8> = Vec::new();
        let mut current_line_number: usize = 1;
        // Buffer needs to be 1 greater than the offset to have a look-ahead line for EOF
        let buffer_size: usize = line_ranges.largest_offset_from_end() + 1;
        // Buffers multiple line data and line number
        let mut buffered_lines: VecDeque<(Vec<u8>, usize)> = VecDeque::with_capacity(buffer_size);

        let mut reached_eof: bool = false;
        let mut first_range: bool = true;
        let mut mid_range: bool = false;

        let style_snip = self.config.style_components.snip();

        loop {
            if reached_eof && buffered_lines.is_empty() {
                // Done processing all lines
                break;
            }
            if !reached_eof {
                if reader.read_line(&mut current_line_buffer)? {
                    // Fill the buffer
                    buffered_lines
                        .push_back((mem::take(&mut current_line_buffer), current_line_number));
                    current_line_number += 1;
                } else {
                    // No more data to read
                    reached_eof = true;
                }
            }

            if buffered_lines.len() < buffer_size && !reached_eof {
                // The buffer needs to be completely filled first
                continue;
            }

            let Some((line, line_nr)) = buffered_lines.pop_front() else {
                break;
            };

            // Determine if the last line number in the buffer is the last line of the file or
            // just a line somewhere in the file
            let max_buffered_line_number = buffered_lines
                .back()
                .map(|(_, max_line_number)| {
                    if reached_eof {
                        MaxBufferedLineNumber::Final(*max_line_number)
                    } else {
                        MaxBufferedLineNumber::Tentative(*max_line_number)
                    }
                })
                .unwrap_or(MaxBufferedLineNumber::Final(line_nr));

            match line_ranges.check(line_nr, max_buffered_line_number) {
                RangeCheckResult::BeforeOrBetweenRanges => {
                    // Call the printer in case we need to call the syntax highlighter
                    // for this line. However, set `out_of_range` to `true`.
                    printer.print_line(true, writer, line_nr, &line, max_buffered_line_number)?;
                    mid_range = false;
                }

                RangeCheckResult::InRange => {
                    if style_snip {
                        if first_range {
                            first_range = false;
                            mid_range = true;
                        } else if !mid_range {
                            mid_range = true;
                            printer.print_snip(writer)?;
                        }
                    }

                    printer.print_line(false, writer, line_nr, &line, max_buffered_line_number)?;
                }
                RangeCheckResult::AfterLastRange => {
                    break;
                }
            }
        }
        Ok(())
    }
}

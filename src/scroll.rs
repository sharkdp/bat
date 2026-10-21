//! Defer pager startup until a source line has a known rendered position.
use std::cell::Cell;
use std::io::{self, Seek, Write};
use std::process::Command;

use crate::config::Config;
use crate::error::{Error, Result};
use crate::less::{retrieve_less_version, LessVersion};
use crate::output::{OutputHandle, OutputType};
use crate::pager::{get_pager, PagerKind};
use crate::paging::PagingMode;
use tempfile::SpooledTempFile;

#[derive(Clone, Copy)]
pub(crate) struct PagerStart {
    // None means the requested source line is beyond the printed output.
    pub line: Option<usize>,
    pub center: bool,
}

impl PagerStart {
    pub(crate) fn configure(self, command: &mut Command, kind: &PagerKind) {
        command.env(
            "BAT_SCROLL_POSITION",
            if self.line.is_none() {
                "end"
            } else if self.center {
                "center"
            } else {
                "top"
            },
        );
        match self.line {
            Some(line) => {
                command.env("BAT_SCROLL_LINE", line.to_string());
            }
            None => {
                command.env_remove("BAT_SCROLL_LINE");
            }
        }
        if *kind == PagerKind::Less {
            if self.center && self.line.is_some() {
                command.arg("-j.5");
            }
            command.arg(
                self.line
                    .map_or_else(|| "+G".to_owned(), |line| format!("+{line}g")),
            );
        }
    }
}

pub(crate) struct DeferredOutput<'a> {
    config: &'a Config<'a>,
    mode: PagingMode,
    reached: &'a Cell<bool>,
    prefix: Option<SpooledTempFile>,
    newlines: usize,
    output: Option<OutputType>,
}

impl<'a> DeferredOutput<'a> {
    pub(crate) fn new(
        config: &'a Config<'a>,
        mode: PagingMode,
        reached: &'a Cell<bool>,
    ) -> Result<Option<Self>> {
        let Some(pager) = get_pager(config.pager).map_err(|_| "Could not parse pager command.")?
        else {
            return Ok(None);
        };
        match pager.kind {
            PagerKind::Bat => return Err(Error::InvalidPagerValueBat),
            PagerKind::Builtin | PagerKind::More | PagerKind::Most => {
                return Err(
                    "Scrolling to a line requires standard less or a custom pager wrapper".into(),
                );
            }
            PagerKind::Less if retrieve_less_version(&pager.bin) == Some(LessVersion::BusyBox) => {
                return Err("Scrolling to a line is not supported by BusyBox less".into());
            }
            _ => {}
        }
        Ok(Some(Self {
            config,
            mode,
            reached,
            prefix: Some(tempfile::spooled_tempfile(64 * 1024)),
            newlines: 0,
            output: None,
        }))
    }

    fn start(&mut self, line: Option<usize>) -> Result<()> {
        if self.output.is_some() {
            return Ok(());
        }
        let output = OutputType::from_mode_at(
            self.mode,
            self.config.wrapping_mode,
            self.config.pager,
            Some(PagerStart {
                line,
                center: self.config.center_highlight || self.config.scroll_to_center,
            }),
        )?;
        self.output = Some(output);
        if let Some(mut prefix) = self.prefix.take() {
            prefix.rewind()?;
            match self
                .output
                .as_mut()
                .expect("pager was just created")
                .handle()?
            {
                OutputHandle::IoWrite(writer) => {
                    io::copy(&mut prefix, writer)?;
                }
                OutputHandle::FmtWrite(_) => {
                    return Err("The selected pager cannot accept a scroll position".into())
                }
            }
        }
        Ok(())
    }

    pub(crate) fn finish(&mut self) -> Result<()> {
        // If no highlight was printed, keep the initial view at the beginning.
        // +G avoids less's out-of-range line error for a request beyond EOF.
        self.start(self.config.center_highlight.then_some(1))?;
        self.flush()?;
        Ok(())
    }
}

impl Write for DeferredOutput<'_> {
    fn write(&mut self, bytes: &[u8]) -> io::Result<usize> {
        if bytes.is_empty() {
            return Ok(0);
        }
        if self.output.is_none() && self.reached.get() {
            self.start(Some(self.newlines.saturating_add(1)))
                .map_err(as_io_error)?;
        }
        match self.output.as_mut() {
            Some(output) => match output.handle().map_err(as_io_error)? {
                OutputHandle::IoWrite(writer) => writer.write(bytes),
                OutputHandle::FmtWrite(_) => {
                    Err(io::Error::other("Pager output must accept bytes"))
                }
            },
            None => {
                let count = self
                    .prefix
                    .as_mut()
                    .expect("prefix exists until pager starts")
                    .write(bytes)?;
                self.newlines = self
                    .newlines
                    .saturating_add(bytes[..count].iter().filter(|&&b| b == b'\n').count());
                Ok(count)
            }
        }
    }

    fn flush(&mut self) -> io::Result<()> {
        match self.output.as_mut() {
            Some(output) => output
                .handle()
                .map_err(as_io_error)?
                .flush()
                .map_err(as_io_error),
            None => self
                .prefix
                .as_mut()
                .expect("prefix exists until pager starts")
                .flush(),
        }
    }
}

fn as_io_error(error: Error) -> io::Error {
    match error {
        Error::Io(error) => error,
        other => io::Error::other(other),
    }
}

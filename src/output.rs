use std::fmt;
use std::io;
#[cfg(feature = "paging")]
use std::process::Child;
#[cfg(feature = "paging")]
use std::thread::{spawn, JoinHandle};

use crate::error::*;
#[cfg(feature = "paging")]
use crate::less::{retrieve_less_version, LessVersion};
#[cfg(feature = "paging")]
use crate::paging::PagingMode;
#[cfg(feature = "paging")]
use crate::wrapping::WrappingMode;

#[cfg(feature = "paging")]
pub struct BuiltinPager {
    pager: minus::Pager,
    handle: Option<JoinHandle<Result<()>>>,
}

#[cfg(feature = "paging")]
impl BuiltinPager {
    fn new() -> Self {
        let pager = minus::Pager::new();
        let handle = {
            let pager = pager.clone();
            Some(spawn(move || {
                minus::dynamic_paging(pager).map_err(Error::from)
            }))
        };
        Self { pager, handle }
    }
}

#[cfg(feature = "paging")]
impl std::fmt::Debug for BuiltinPager {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("BuiltinPager")
            //.field("pager", &self.pager) /// minus::Pager doesn't implement fmt::Debug
            .field("handle", &self.handle)
            .finish()
    }
}

#[cfg(feature = "paging")]
#[derive(Debug, PartialEq)]
enum SingleScreenAction {
    Quit,
    Nothing,
}

#[derive(Debug)]
pub enum OutputType {
    #[cfg(feature = "paging")]
    Pager(Child),
    #[cfg(feature = "paging")]
    BuiltinPager(BuiltinPager),
    Stdout(io::Stdout),
}

impl OutputType {
    #[cfg(feature = "paging")]
    pub fn from_mode(
        paging_mode: PagingMode,
        wrapping_mode: WrappingMode,
        pager: Option<&str>,
    ) -> Result<Self> {
        use self::PagingMode::*;
        Ok(match paging_mode {
            Always => OutputType::try_pager(SingleScreenAction::Nothing, wrapping_mode, pager)?,
            QuitIfOneScreen => {
                OutputType::try_pager(SingleScreenAction::Quit, wrapping_mode, pager)?
            }
            _ => OutputType::stdout(),
        })
    }

    /// Try to launch the pager. Fall back to stdout in case of errors.
    #[cfg(feature = "paging")]
    fn try_pager(
        single_screen_action: SingleScreenAction,
        wrapping_mode: WrappingMode,
        pager_from_config: Option<&str>,
    ) -> Result<Self> {
        use crate::pager::{self, PagerKind, PagerSource};
        use std::process::{Command, Stdio};

        let pager_opt =
            pager::get_pager(pager_from_config).map_err(|_| "Could not parse pager command.")?;

        let pager = match pager_opt {
            Some(pager) => pager,
            None => return Ok(OutputType::stdout()),
        };

        if pager.kind == PagerKind::Bat {
            return Err(Error::InvalidPagerValueBat);
        }

        if pager.kind == PagerKind::Builtin {
            return Ok(OutputType::BuiltinPager(BuiltinPager::new()));
        }

        let resolved_path = match grep_cli::resolve_binary(&pager.bin) {
            Ok(path) => path,
            Err(_) => {
                return Ok(OutputType::stdout());
            }
        };

        let mut p = Command::new(resolved_path);
        let args = pager.args;

        if pager.kind == PagerKind::Less {
            // less needs to be called with the '-R' option in order to properly interpret the
            // ANSI color sequences printed by bat. If someone has set PAGER="less -F", we
            // therefore need to overwrite the arguments and add '-R'.
            //
            // We only do this for PAGER (as it is not specific to 'bat'), not for BAT_PAGER
            // or bats '--pager' command line option.
            let replace_arguments_to_less = pager.source == PagerSource::EnvVarPager;

            if args.is_empty() || replace_arguments_to_less {
                p.arg("-R"); // Short version of --RAW-CONTROL-CHARS for maximum compatibility
                if single_screen_action == SingleScreenAction::Quit {
                    p.arg("-F"); // Short version of --quit-if-one-screen for compatibility
                }

                if wrapping_mode == WrappingMode::NoWrapping(true) {
                    p.arg("-S"); // Short version of --chop-long-lines for compatibility
                }

                // Ensures that 'less' quits together with 'bat'
                p.arg("-K"); // Short version of '--quit-on-intr'

                // Passing '--no-init' fixes a bug with '--quit-if-one-screen' in older
                // versions of 'less'. Unfortunately, it also breaks mouse-wheel support.
                //
                // See: http://www.greenwoodsoftware.com/less/news.530.html
                //
                // For newer versions (530 or 558 on Windows), we omit '--no-init' as it
                // is not needed anymore.
                match retrieve_less_version(&pager.bin) {
                    None => {
                        p.arg("--no-init");
                    }
                    Some(LessVersion::Less(version))
                        if (version < 530 || (cfg!(windows) && version < 558)) =>
                    {
                        p.arg("--no-init");
                    }
                    _ => {}
                }
            } else {
                p.args(args);
            }
            p.env("LESSCHARSET", "UTF-8");

            #[cfg(feature = "lessopen")]
            // Ensures that 'less' does not preprocess input again if '$LESSOPEN' is set.
            p.arg("--no-lessopen");
        } else {
            p.args(args);
        };

        Ok(p.stdin(Stdio::piped())
            .spawn()
            .map(OutputType::Pager)
            .unwrap_or_else(|_| OutputType::stdout()))
    }

    pub(crate) fn stdout() -> Self {
        OutputType::Stdout(io::stdout())
    }

    #[cfg(feature = "paging")]
    pub(crate) fn is_pager(&self) -> bool {
        matches!(self, OutputType::Pager(_) | OutputType::BuiltinPager(_))
    }

    #[cfg(not(feature = "paging"))]
    pub(crate) fn is_pager(&self) -> bool {
        false
    }

    pub fn handle<'a>(&'a mut self) -> Result<OutputHandle<'a>> {
        Ok(match *self {
            #[cfg(feature = "paging")]
            OutputType::Pager(ref mut command) => OutputHandle::IoWrite(
                command
                    .stdin
                    .as_mut()
                    .ok_or("Could not open stdin for pager")?,
            ),
            #[cfg(feature = "paging")]
            OutputType::BuiltinPager(ref mut pager) => OutputHandle::FmtWrite(&mut pager.pager),
            OutputType::Stdout(ref mut handle) => OutputHandle::IoWrite(handle),
        })
    }
}

#[cfg(feature = "paging")]
impl Drop for OutputType {
    fn drop(&mut self) {
        match *self {
            OutputType::Pager(ref mut command) => {
                let _ = command.wait();
            }
            OutputType::BuiltinPager(ref mut pager) => {
                if pager.handle.is_some() {
                    let _ = pager.handle.take().unwrap().join().unwrap();
                }
            }
            OutputType::Stdout(_) => (),
        }
    }
}

pub enum OutputHandle<'a> {
    IoWrite(&'a mut dyn io::Write),
    FmtWrite(&'a mut dyn fmt::Write),
}

impl OutputHandle<'_> {
    pub fn write_fmt(&mut self, args: fmt::Arguments<'_>) -> Result<()> {
        match self {
            Self::IoWrite(handle) => handle.write_fmt(args).map_err(Into::into),
            Self::FmtWrite(handle) => handle.write_fmt(args).map_err(Into::into),
        }
    }
}

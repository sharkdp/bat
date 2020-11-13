use std::io::{self, Write};
#[cfg(feature = "paging")]
use std::process::Child;

use crate::error::*;
#[cfg(feature = "paging")]
use crate::less::retrieve_less_version;
#[cfg(feature = "paging")]
use crate::paging::PagingMode;
#[cfg(feature = "paging")]
use crate::wrapping::WrappingMode;

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
        use std::env;
        use std::ffi::OsString;
        use std::path::PathBuf;
        use std::process::{Command, Stdio};

        let mut replace_arguments_to_less = false;

        let pager_from_env = match (env::var("BAT_PAGER"), env::var("PAGER")) {
            (Ok(bat_pager), _) => Some(bat_pager),
            (_, Ok(pager)) => {
                // less needs to be called with the '-R' option in order to properly interpret the
                // ANSI color sequences printed by bat. If someone has set PAGER="less -F", we
                // therefore need to overwrite the arguments and add '-R'.
                //
                // We only do this for PAGER (as it is not specific to 'bat'), not for BAT_PAGER
                // or bats '--pager' command line option.
                replace_arguments_to_less = true;
                Some(pager)
            }
            _ => None,
        };

        let pager_from_config = pager_from_config.map(|p| p.to_string());

        if pager_from_config.is_some() {
            replace_arguments_to_less = false;
        }

        let pager = pager_from_config
            .or(pager_from_env)
            .unwrap_or_else(|| String::from("less"));

        let pagerflags =
            shell_words::split(&pager).chain_err(|| "Could not parse pager command.")?;

        match pagerflags.split_first() {
            Some((pager_name, args)) => {
                let pager_path = PathBuf::from(pager_name);

                if pager_path.file_stem() == Some(&OsString::from("bat")) {
                    return Err(ErrorKind::InvalidPagerValueBat.into());
                }

                let is_less = pager_path.file_stem() == Some(&OsString::from("less"));

                let mut process = if is_less {
                    let mut p = Command::new(&pager_path);
                    if args.is_empty() || replace_arguments_to_less {
                        p.arg("--RAW-CONTROL-CHARS");
                        if single_screen_action == SingleScreenAction::Quit {
                            p.arg("--quit-if-one-screen");
                        }

                        if wrapping_mode == WrappingMode::NoWrapping {
                            p.arg("--chop-long-lines");
                        }

                        // Passing '--no-init' fixes a bug with '--quit-if-one-screen' in older
                        // versions of 'less'. Unfortunately, it also breaks mouse-wheel support.
                        //
                        // See: http://www.greenwoodsoftware.com/less/news.530.html
                        //
                        // For newer versions (530 or 558 on Windows), we omit '--no-init' as it
                        // is not needed anymore.
                        match retrieve_less_version() {
                            None => {
                                p.arg("--no-init");
                            }
                            Some(version)
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
                    p
                } else {
                    let mut p = Command::new(&pager_path);
                    p.args(args);
                    p
                };

                Ok(process
                    .stdin(Stdio::piped())
                    .spawn()
                    .map(OutputType::Pager)
                    .unwrap_or_else(|_| OutputType::stdout()))
            }
            None => Ok(OutputType::stdout()),
        }
    }

    pub(crate) fn stdout() -> Self {
        OutputType::Stdout(io::stdout())
    }

    #[cfg(feature = "paging")]
    pub(crate) fn is_pager(&self) -> bool {
        if let OutputType::Pager(_) = self {
            true
        } else {
            false
        }
    }

    #[cfg(not(feature = "paging"))]
    pub(crate) fn is_pager(&self) -> bool {
        false
    }

    pub fn handle(&mut self) -> Result<&mut dyn Write> {
        Ok(match *self {
            #[cfg(feature = "paging")]
            OutputType::Pager(ref mut command) => command
                .stdin
                .as_mut()
                .chain_err(|| "Could not open stdin for pager")?,
            OutputType::Stdout(ref mut handle) => handle,
        })
    }
}

#[cfg(feature = "paging")]
impl Drop for OutputType {
    fn drop(&mut self) {
        if let OutputType::Pager(ref mut command) = *self {
            let _ = command.wait();
        }
    }
}

use app::PagingMode;
use errors::*;
use std::io::{self, Write};
use std::process::{Child, Command, Stdio};

pub enum OutputType {
    Pager(Child),
    Stdout(io::Stdout),
}

impl OutputType {
    pub fn from_mode(mode: PagingMode) -> Self {
        use self::PagingMode::*;
        match mode {
            Always => OutputType::try_pager(false),
            QuitIfOneScreen => OutputType::try_pager(true),
            _ => OutputType::stdout(),
        }
    }

    /// Try to launch the pager. Fall back to stdout in case of errors.
    fn try_pager(quit_if_one_screen: bool) -> Self {
        let mut args = vec!["--RAW-CONTROL-CHARS", "--no-init"];
        if quit_if_one_screen {
            args.push("--quit-if-one-screen");
        }
        Command::new("less")
            .args(&args)
            .env("LESSCHARSET", "UTF-8")
            .stdin(Stdio::piped())
            .spawn()
            .map(OutputType::Pager)
            .unwrap_or_else(|_| OutputType::stdout())
    }

    fn stdout() -> Self {
        OutputType::Stdout(io::stdout())
    }

    pub fn handle(&mut self) -> Result<&mut Write> {
        Ok(match *self {
            OutputType::Pager(ref mut command) => command
                .stdin
                .as_mut()
                .chain_err(|| "Could not open stdin for pager")?,
            OutputType::Stdout(ref mut handle) => handle,
        })
    }
}

impl Drop for OutputType {
    fn drop(&mut self) {
        if let OutputType::Pager(ref mut command) = *self {
            let _ = command.wait();
        }
    }
}

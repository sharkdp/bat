//! Lazily run one filter per CLI input without buffering its complete output.

use std::fs::File;
use std::io::{self, Read};
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};

use clircle::{Clircle, Identifier};

#[derive(Clone)]
pub struct ProcessCommand(Vec<String>);

impl ProcessCommand {
    pub fn parse(value: &str) -> Result<Self, String> {
        let words = shell_words::split(value).map_err(|e| e.to_string())?;
        if words.first().is_none_or(String::is_empty) {
            return Err("The process command cannot be empty".into());
        }
        Ok(Self(words))
    }

    pub fn reader(&self, path: Option<&Path>) -> ProcessReader {
        ProcessReader {
            command: self.clone(),
            path: path.map(Path::to_path_buf),
            child: None,
            finished: false,
        }
    }
}

pub struct ProcessReader {
    command: ProcessCommand,
    path: Option<PathBuf>,
    child: Option<Child>,
    finished: bool,
}

impl ProcessReader {
    fn start(&mut self) -> io::Result<()> {
        let stdout = if cfg!(windows) {
            None
        } else {
            Identifier::stdout()
        };
        let input = if let Some(path) = &self.path {
            let display = bat::sanitize_for_terminal(&path.to_string_lossy());
            let mut file = File::open(path).map_err(|error| {
                io::Error::new(
                    error.kind(),
                    format!("Input process source '{display}': {error}"),
                )
            })?;
            if file.metadata()?.is_dir() {
                return Err(io::Error::other(format!(
                    "Input process source '{display}' is a directory"
                )));
            }
            if let Some(stdout) = stdout {
                let identity = Identifier::try_from(file).map_err(io::Error::other)?;
                if stdout.surely_conflicts_with(&identity) {
                    return Err(io::Error::other(
                        "IO circle detected: the process input is also an output",
                    ));
                }
                file = identity
                    .into_inner()
                    .ok_or_else(|| io::Error::other("Lost process input file"))?;
            }
            Stdio::from(file)
        } else {
            if let Some(stdout) = stdout {
                let identity =
                    Identifier::try_from(clircle::Stdio::Stdin).map_err(io::Error::other)?;
                if stdout.surely_conflicts_with(&identity) {
                    return Err(io::Error::other(
                        "IO circle detected: process stdin is also an output",
                    ));
                }
            }
            Stdio::inherit()
        };
        self.child = Some(
            Command::new(&self.command.0[0])
                .args(&self.command.0[1..])
                .stdin(input)
                .stdout(Stdio::piped())
                .stderr(Stdio::inherit())
                .spawn()
                .map_err(|error| {
                    io::Error::new(
                        error.kind(),
                        format!("Could not start input process: {error}"),
                    )
                })?,
        );
        Ok(())
    }
}

impl Read for ProcessReader {
    fn read(&mut self, buffer: &mut [u8]) -> io::Result<usize> {
        if buffer.is_empty() || self.finished {
            return Ok(0);
        }
        if self.child.is_none() {
            self.start()?;
        }
        let child = self.child.as_mut().unwrap();
        let count = child.stdout.as_mut().unwrap().read(buffer)?;
        if count == 0 {
            let status = child.wait()?;
            self.finished = true;
            self.child = None;
            if !status.success() {
                return Err(io::Error::other(format!("Input process failed: {status}")));
            }
        }
        Ok(count)
    }
}

impl Drop for ProcessReader {
    fn drop(&mut self) {
        if let Some(mut child) = self.child.take() {
            // A line range or a closed pager may stop reading before EOF.
            // Do not leave a filter blocked on its output pipe in that case.
            if !matches!(child.try_wait(), Ok(Some(_))) {
                let _ = child.kill();
                let _ = child.wait();
            }
        }
    }
}

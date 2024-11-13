#![cfg(feature = "lessopen")]

use std::convert::TryFrom;
use std::env;
use std::fs::File;
use std::io::{BufRead, BufReader, Cursor, Read, Write};
use std::path::PathBuf;
use std::str;

use clircle::{Clircle, Identifier};
use os_str_bytes::RawOsString;
use run_script::{IoOptions, ScriptOptions};

use crate::error::Result;
use crate::{
    bat_warning,
    input::{Input, InputKind, InputReader, OpenedInput, OpenedInputKind},
};

/// Preprocess files and/or stdin using $LESSOPEN and $LESSCLOSE
pub(crate) struct LessOpenPreprocessor {
    lessopen: String,
    lessclose: Option<String>,
    command_options: ScriptOptions,
    kind: LessOpenKind,
    /// Whether or not data piped via stdin is to be preprocessed
    preprocess_stdin: bool,
}

enum LessOpenKind {
    Piped,
    PipedIgnoreExitCode,
    TempFile,
}

impl LessOpenPreprocessor {
    /// Create a new instance of LessOpenPreprocessor
    /// Will return Ok if and only if $LESSOPEN is set and contains exactly one %s
    pub(crate) fn new() -> Result<LessOpenPreprocessor> {
        let lessopen = env::var("LESSOPEN")?;

        // Ignore $LESSOPEN if it does not contains exactly one %s
        // Note that $LESSCLOSE has no such requirement
        if lessopen.match_indices("%s").count() != 1 {
            let error_msg = "LESSOPEN ignored: must contain exactly one %s";
            bat_warning!("{}", error_msg);
            return Err(error_msg.into());
        }

        // "||" means pipe directly to bat without making a temporary file
        // Also, if preprocessor output is empty and exit code is zero, use the empty output
        // Otherwise, if output is empty and exit code is nonzero, use original file contents
        let (kind, lessopen) = if lessopen.starts_with("||") {
            (LessOpenKind::Piped, lessopen.chars().skip(2).collect())
        // "|" means pipe, but ignore exit code, always using preprocessor output
        } else if lessopen.starts_with('|') {
            (
                LessOpenKind::PipedIgnoreExitCode,
                lessopen.chars().skip(1).collect(),
            )
        // If neither appear, write output to a temporary file and read from that
        } else {
            (LessOpenKind::TempFile, lessopen)
        };

        // "-" means that stdin is preprocessed along with files and may appear alongside "|" and "||"
        let (stdin, lessopen) = if lessopen.starts_with('-') {
            (true, lessopen.chars().skip(1).collect())
        } else {
            (false, lessopen)
        };

        let mut command_options = ScriptOptions::new();
        command_options.runner = env::var("SHELL").ok();
        command_options.input_redirection = IoOptions::Pipe;

        Ok(Self {
            lessopen: lessopen.replacen("%s", "$1", 1),
            lessclose: env::var("LESSCLOSE")
                .ok()
                .map(|str| str.replacen("%s", "$1", 1).replacen("%s", "$2", 1)),
            command_options,
            kind,
            preprocess_stdin: stdin,
        })
    }

    pub(crate) fn open<'a, R: BufRead + 'a>(
        &self,
        input: Input<'a>,
        mut stdin: R,
        stdout_identifier: Option<&Identifier>,
    ) -> Result<OpenedInput<'a>> {
        let (lessopen_stdout, path_str, kind) = match input.kind {
            InputKind::OrdinaryFile(ref path) => {
                let path_str = match path.to_str() {
                    Some(str) => str,
                    None => return input.open(stdin, stdout_identifier),
                };

                let (exit_code, lessopen_stdout, _) = match run_script::run(
                    &self.lessopen,
                    &vec![path_str.to_string()],
                    &self.command_options,
                ) {
                    Ok(output) => output,
                    Err(_) => return input.open(stdin, stdout_identifier),
                };

                if self.fall_back_to_original_file(&lessopen_stdout, exit_code) {
                    return input.open(stdin, stdout_identifier);
                }

                (
                    RawOsString::new(lessopen_stdout),
                    path_str.to_string(),
                    OpenedInputKind::OrdinaryFile(path.to_path_buf()),
                )
            }
            InputKind::StdIn => {
                if self.preprocess_stdin {
                    if let Some(stdout) = stdout_identifier {
                        let input_identifier = Identifier::try_from(clircle::Stdio::Stdin)
                            .map_err(|e| format!("Stdin: Error identifying file: {}", e))?;
                        if stdout.surely_conflicts_with(&input_identifier) {
                            return Err("IO circle detected. The input from stdin is also an output. Aborting to avoid infinite loop.".into());
                        }
                    }

                    // stdin isn't Clone, so copy it to a cloneable buffer
                    let mut stdin_buffer = Vec::new();
                    stdin.read_to_end(&mut stdin_buffer).unwrap();

                    let mut lessopen_handle = match run_script::spawn(
                        &self.lessopen,
                        &vec!["-".to_string()],
                        &self.command_options,
                    ) {
                        Ok(handle) => handle,
                        Err(_) => {
                            return input.open(stdin, stdout_identifier);
                        }
                    };

                    if lessopen_handle
                        .stdin
                        .as_mut()
                        .unwrap()
                        .write_all(&stdin_buffer.clone())
                        .is_err()
                    {
                        return input.open(stdin, stdout_identifier);
                    }

                    let lessopen_output = match lessopen_handle.wait_with_output() {
                        Ok(output) => output,
                        Err(_) => {
                            return input.open(Cursor::new(stdin_buffer), stdout_identifier);
                        }
                    };

                    if lessopen_output.stdout.is_empty()
                        && (!lessopen_output.status.success()
                            || matches!(self.kind, LessOpenKind::PipedIgnoreExitCode))
                    {
                        return input.open(Cursor::new(stdin_buffer), stdout_identifier);
                    }

                    (
                        RawOsString::assert_from_raw_vec(lessopen_output.stdout),
                        "-".to_string(),
                        OpenedInputKind::StdIn,
                    )
                } else {
                    return input.open(stdin, stdout_identifier);
                }
            }
            InputKind::CustomReader(_) => {
                return input.open(stdin, stdout_identifier);
            }
        };

        Ok(OpenedInput {
            kind,
            reader: InputReader::new(BufReader::new(
                if matches!(self.kind, LessOpenKind::TempFile) {
                    // Remove newline at end of temporary file path returned by $LESSOPEN
                    let stdout = match lessopen_stdout.strip_suffix("\n") {
                        Some(stripped) => stripped.to_owned(),
                        None => lessopen_stdout,
                    };

                    let stdout = stdout.into_os_string();

                    let file = match File::open(PathBuf::from(&stdout)) {
                        Ok(file) => file,
                        Err(_) => {
                            return input.open(stdin, stdout_identifier);
                        }
                    };

                    Preprocessed {
                        kind: PreprocessedKind::TempFile(file),
                        lessclose: self.lessclose.clone(),
                        command_args: vec![path_str, stdout.to_str().unwrap().to_string()],
                        command_options: self.command_options.clone(),
                    }
                } else {
                    Preprocessed {
                        kind: PreprocessedKind::Piped(Cursor::new(lessopen_stdout.into_raw_vec())),
                        lessclose: self.lessclose.clone(),
                        command_args: vec![path_str, "-".to_string()],
                        command_options: self.command_options.clone(),
                    }
                },
            )),
            metadata: input.metadata,
            description: input.description,
        })
    }

    fn fall_back_to_original_file(&self, lessopen_output: &str, exit_code: i32) -> bool {
        lessopen_output.is_empty()
            && (exit_code != 0 || matches!(self.kind, LessOpenKind::PipedIgnoreExitCode))
    }

    #[cfg(test)]
    /// For testing purposes only
    /// Create an instance of LessOpenPreprocessor with specified valued for $LESSOPEN and $LESSCLOSE
    fn mock_new(lessopen: Option<&str>, lessclose: Option<&str>) -> Result<LessOpenPreprocessor> {
        if let Some(command) = lessopen {
            env::set_var("LESSOPEN", command)
        } else {
            env::remove_var("LESSOPEN")
        }

        if let Some(command) = lessclose {
            env::set_var("LESSCLOSE", command)
        } else {
            env::remove_var("LESSCLOSE")
        }

        Self::new()
    }
}

enum PreprocessedKind {
    Piped(Cursor<Vec<u8>>),
    TempFile(File),
}

impl Read for PreprocessedKind {
    fn read(&mut self, buf: &mut [u8]) -> std::result::Result<usize, std::io::Error> {
        match self {
            PreprocessedKind::Piped(data) => data.read(buf),
            PreprocessedKind::TempFile(data) => data.read(buf),
        }
    }
}

pub struct Preprocessed {
    kind: PreprocessedKind,
    lessclose: Option<String>,
    command_args: Vec<String>,
    command_options: ScriptOptions,
}

impl Read for Preprocessed {
    fn read(&mut self, buf: &mut [u8]) -> std::result::Result<usize, std::io::Error> {
        self.kind.read(buf)
    }
}

impl Drop for Preprocessed {
    fn drop(&mut self) {
        if let Some(ref command) = self.lessclose {
            self.command_options.output_redirection = IoOptions::Inherit;

            run_script::run(command, &self.command_args, &self.command_options)
                .expect("failed to run $LESSCLOSE to clean up file");
        }
    }
}

#[cfg(test)]
mod tests {
    // All tests here are serial because they all involve reading and writing environment variables
    // Running them in parallel causes these tests and some others to randomly fail
    use serial_test::serial;

    use super::*;

    /// Reset environment variables after each test as a precaution
    fn reset_env_vars() {
        env::remove_var("LESSOPEN");
        env::remove_var("LESSCLOSE");
    }

    #[test]
    #[serial]
    fn test_just_lessopen() -> Result<()> {
        let preprocessor = LessOpenPreprocessor::mock_new(Some("|batpipe %s"), None)?;

        assert_eq!(preprocessor.lessopen, "batpipe $1");
        assert!(preprocessor.lessclose.is_none());

        reset_env_vars();

        Ok(())
    }

    #[test]
    #[serial]
    fn test_just_lessclose() -> Result<()> {
        let preprocessor = LessOpenPreprocessor::mock_new(None, Some("lessclose.sh %s %s"));

        assert!(preprocessor.is_err());

        reset_env_vars();

        Ok(())
    }

    #[test]
    #[serial]
    fn test_both_lessopen_and_lessclose() -> Result<()> {
        let preprocessor =
            LessOpenPreprocessor::mock_new(Some("lessopen.sh %s"), Some("lessclose.sh %s %s"))?;

        assert_eq!(preprocessor.lessopen, "lessopen.sh $1");
        assert_eq!(preprocessor.lessclose.unwrap(), "lessclose.sh $1 $2");

        reset_env_vars();

        Ok(())
    }

    #[test]
    #[serial]
    fn test_lessopen_prefixes() -> Result<()> {
        let preprocessor = LessOpenPreprocessor::mock_new(Some("batpipe %s"), None)?;

        assert_eq!(preprocessor.lessopen, "batpipe $1");
        assert!(matches!(preprocessor.kind, LessOpenKind::TempFile));
        assert!(!preprocessor.preprocess_stdin);

        let preprocessor = LessOpenPreprocessor::mock_new(Some("|batpipe %s"), None)?;

        assert_eq!(preprocessor.lessopen, "batpipe $1");
        assert!(matches!(
            preprocessor.kind,
            LessOpenKind::PipedIgnoreExitCode
        ));
        assert!(!preprocessor.preprocess_stdin);

        let preprocessor = LessOpenPreprocessor::mock_new(Some("||batpipe %s"), None)?;

        assert_eq!(preprocessor.lessopen, "batpipe $1");
        assert!(matches!(preprocessor.kind, LessOpenKind::Piped));
        assert!(!preprocessor.preprocess_stdin);

        let preprocessor = LessOpenPreprocessor::mock_new(Some("-batpipe %s"), None)?;

        assert_eq!(preprocessor.lessopen, "batpipe $1");
        assert!(matches!(preprocessor.kind, LessOpenKind::TempFile));
        assert!(preprocessor.preprocess_stdin);

        let preprocessor = LessOpenPreprocessor::mock_new(Some("|-batpipe %s"), None)?;

        assert_eq!(preprocessor.lessopen, "batpipe $1");
        assert!(matches!(
            preprocessor.kind,
            LessOpenKind::PipedIgnoreExitCode
        ));
        assert!(preprocessor.preprocess_stdin);

        let preprocessor = LessOpenPreprocessor::mock_new(Some("||-batpipe %s"), None)?;

        assert_eq!(preprocessor.lessopen, "batpipe $1");
        assert!(matches!(preprocessor.kind, LessOpenKind::Piped));
        assert!(preprocessor.preprocess_stdin);

        reset_env_vars();

        Ok(())
    }

    #[test]
    #[serial]
    fn replace_part_of_argument() -> Result<()> {
        let preprocessor =
            LessOpenPreprocessor::mock_new(Some("|echo File:%s"), Some("echo File:%s Temp:%s"))?;

        assert_eq!(preprocessor.lessopen, "echo File:$1");
        assert_eq!(preprocessor.lessclose.unwrap(), "echo File:$1 Temp:$2");

        reset_env_vars();

        Ok(())
    }
}

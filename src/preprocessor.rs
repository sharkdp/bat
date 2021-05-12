use console::AnsiCodeIterator;

#[cfg(feature = "preprocessor")]
pub struct Preprocessor<'a>(pub(crate) Box<dyn crate::input::InputPreprocessor<'a> + 'a>);

/// Expand tabs like an ANSI-enabled expand(1).
pub(crate) fn expand_tabs(line: &str, width: usize, cursor: &mut usize) -> String {
    let mut buffer = String::with_capacity(line.len() * 2);

    for chunk in AnsiCodeIterator::new(line) {
        match chunk {
            (text, true) => buffer.push_str(text),
            (mut text, false) => {
                while let Some(index) = text.find('\t') {
                    // Add previous text.
                    if index > 0 {
                        *cursor += index;
                        buffer.push_str(&text[0..index]);
                    }

                    // Add tab.
                    let spaces = width - (*cursor % width);
                    *cursor += spaces;
                    buffer.push_str(&*" ".repeat(spaces));

                    // Next.
                    text = &text[index + 1..text.len()];
                }

                *cursor += text.len();
                buffer.push_str(text);
            }
        }
    }

    buffer
}

fn try_parse_utf8_char(input: &[u8]) -> Option<(char, usize)> {
    let str_from_utf8 = |seq| std::str::from_utf8(seq).ok();

    let decoded = input
        .get(0..1)
        .and_then(str_from_utf8)
        .map(|c| (c, 1))
        .or_else(|| input.get(0..2).and_then(str_from_utf8).map(|c| (c, 2)))
        .or_else(|| input.get(0..3).and_then(str_from_utf8).map(|c| (c, 3)))
        .or_else(|| input.get(0..4).and_then(str_from_utf8).map(|c| (c, 4)));

    decoded.map(|(seq, n)| (seq.chars().next().unwrap(), n))
}

pub(crate) fn replace_nonprintable(input: &[u8], tab_width: usize) -> String {
    let mut output = String::new();

    let tab_width = if tab_width == 0 { 4 } else { tab_width };

    let mut idx = 0;
    let len = input.len();
    while idx < len {
        if let Some((chr, skip_ahead)) = try_parse_utf8_char(&input[idx..]) {
            idx += skip_ahead;

            match chr {
                // space
                ' ' => output.push('·'),
                // tab
                '\t' => {
                    if tab_width == 1 {
                        output.push('↹');
                    } else {
                        output.push('├');
                        output.push_str(&"─".repeat(tab_width - 2));
                        output.push('┤');
                    }
                }
                // line feed
                '\x0A' => output.push_str("␊\x0A"),
                // carriage return
                '\x0D' => output.push('␍'),
                // null
                '\x00' => output.push('␀'),
                // bell
                '\x07' => output.push('␇'),
                // backspace
                '\x08' => output.push('␈'),
                // escape
                '\x1B' => output.push('␛'),
                // printable ASCII
                c if c.is_ascii_alphanumeric()
                    || c.is_ascii_punctuation()
                    || c.is_ascii_graphic() =>
                {
                    output.push(c)
                }
                // everything else
                c => output.push_str(&c.escape_unicode().collect::<String>()),
            }
        } else {
            output.push_str(&format!("\\x{:02X}", input[idx]));
            idx += 1;
        }
    }

    output
}

#[cfg(feature = "preprocessor-lessopen")]
pub(crate) mod __feature_preprocessor_lessopen {
    use crate::error::{Error, ErrorKind, Result};
    use crate::input::{Input, InputHandle, InputPreprocessor, OpenedInput, OpenedInputHandle};
    use os_str_bytes::OsStrBytes;
    use shell_words::split;
    use std::ffi::OsString;
    use std::io::{BufRead, BufReader};
    use std::path::{Path, PathBuf};
    use std::process::{Child, Command, Stdio};

    /// A preprocessor that follows the LESSOPEN and LESSCLOSE specification.
    pub struct LessPreprocessor {
        lessopen_template: Vec<String>,
        lessclose_template: Option<Vec<String>>,
        kind: LessPreprocessorKind,
    }

    enum LessPreprocessorKind {
        Piped,  // Data is read directly from stdout.
        Staged, // Data is read from a temporary file.
    }

    struct LessPreprocessorHandle {
        cleanup: Option<Vec<OsString>>,
        process: Option<Child>,
    }

    impl OpenedInputHandle for LessPreprocessorHandle {
        fn close(&mut self) -> Result<()> {
            let results = vec![self.handle_process(), self.handle_cleanup()];

            match results.into_iter().find(|p| p.is_err()) {
                Some(Err(err)) => Err(Error::from_kind(ErrorKind::Preprocessor(
                    "lessclose".to_owned(),
                    Box::new(err),
                ))),
                _ => Ok(()),
            }
        }
    }

    impl LessPreprocessorHandle {
        fn handle_process(&mut self) -> Result<()> {
            if let Some(child) = &mut self.process {
                // Kill the child process.
                if let Err(error) = child.kill() {
                    if error.kind() != std::io::ErrorKind::InvalidInput {
                        let _ = child.wait();
                        return Err(error.into());
                    }
                }

                // Wait for the child process.
                child.wait()?;
            }

            Ok(())
        }

        fn handle_cleanup(&mut self) -> Result<()> {
            if let Some(args) = &self.cleanup {
                // Run the LESSCLOSE cleanup command.
                Command::new(&args[0])
                    .args(args.iter().skip(1))
                    .spawn()?
                    .wait()?;
            }

            Ok(())
        }
    }

    impl<'a> InputPreprocessor<'a> for LessPreprocessor {
        fn open(&self, input: Input<'a>, handle: &InputHandle) -> Result<OpenedInput<'a>> {
            let input_path = match input.path() {
                Some(path) => path,
                None => return input.open(handle),
            };

            fn passthrough_with_warning(
                input: Result<OpenedInput>,
                error: Error,
            ) -> Result<OpenedInput> {
                match input {
                    Err(error) => Err(error),
                    Ok(mut opened) => {
                        opened
                            .warnings
                            .push(Error::from_kind(ErrorKind::Preprocessor(
                                "lessopen".to_owned(),
                                Box::new(error),
                            )));

                        Ok(opened)
                    }
                }
            }

            // Spawn the preprocessor executable.
            let command_args = Self::argsub(&self.lessopen_template, vec![input_path]);
            let mut command = Command::new(&command_args[0]);
            command
                .args(command_args.iter().skip(1))
                .stdout(Stdio::piped())
                .stderr(Stdio::null())
                .stdin(Stdio::null());

            let mut child: Child = match command.spawn() {
                Err(error) => return passthrough_with_warning(input.open(handle), error.into()),
                Ok(child) => child,
            };

            // Create a new reader, depending on what kind of preprocessor is being used.
            let child_stdout = child.stdout.take().expect("Piped preprocessor stdout");
            Ok(match self.kind {
                // Pipe preprocessor.
                LessPreprocessorKind::Piped => {
                    // Turn the preprocessor's standard output into a reader.
                    let pp_input =
                        Input::from_reader(Box::new(child_stdout)).preprocessed_from(&input);

                    // Open the preprocessor's reader.
                    let mut pp_opened = match pp_input.open(handle) {
                        Err(error) => return passthrough_with_warning(input.open(handle), error),
                        Ok(opened) => opened,
                    };

                    // If the preprocessor didn't output anything, use the original reader.
                    if pp_opened.reader.first_line.is_empty() {
                        let _ = child.wait();
                        return input.open(handle);
                    }

                    // Add a handle to cleanup the preprocessor data.
                    pp_opened.handles.push(Box::new(LessPreprocessorHandle {
                        process: Some(child),
                        cleanup: self
                            .lessclose_template
                            .as_ref()
                            .map(|args| Self::argsub(&args, vec![input_path, Path::new("-")])),
                    }));

                    pp_opened
                }

                // Temporary file preprocessor.
                LessPreprocessorKind::Staged => {
                    let mut reader = BufReader::new(child_stdout);
                    let mut buffer = vec![];

                    // Read the first line, which will contain the temporary file name.
                    let read = match reader.read_until(b'\n', &mut buffer) {
                        Ok(read) => read,
                        Err(error) => {
                            return passthrough_with_warning(input.open(handle), error.into())
                        }
                    };

                    // If the preprocessor didn't write anything, use the original reader.
                    if read == 0 {
                        let _ = child.wait();
                        return input.open(handle);
                    }

                    // Turn the first line into a PathBuf, and create a new reader from it.
                    let pp_file = PathBuf::from(OsStrBytes::from_raw_bytes(&buffer[0..read - 1])?);
                    let pp_input = Input::ordinary_file(pp_file).preprocessed_from(&input);

                    // Open the new reader.
                    let mut pp_opened = match pp_input.open(handle) {
                        Err(error) => return passthrough_with_warning(input.open(handle), error),
                        Ok(opened) => opened,
                    };

                    // Add a handle to cleanup the preprocessor data.
                    pp_opened.handles.push(Box::new(LessPreprocessorHandle {
                        process: None,
                        cleanup: self
                            .lessclose_template
                            .as_ref()
                            .map(|args| Self::argsub(&args, vec![input_path, Path::new("-")])),
                    }));

                    pp_opened
                }
            })
        }
    }

    impl LessPreprocessor {
        pub fn new(
            lessopen: String,
            lessclose: Option<String>,
        ) -> std::result::Result<Option<Self>, shell_words::ParseError> {
            let (lessopen, kind) = if lessopen.starts_with('|') {
                (
                    lessopen
                        .chars()
                        .skip(1)
                        .skip_while(|c| c.is_whitespace())
                        .collect::<String>(),
                    LessPreprocessorKind::Piped,
                )
            } else {
                (lessopen, LessPreprocessorKind::Staged)
            };

            let lessopen_template = match split(&lessopen) {
                Ok(args) => args,
                Err(error) => return Err(error),
            };

            let lessclose_template = match lessclose.map(|lessclose| split(&lessclose)) {
                Some(Err(error)) => return Err(error),
                Some(Ok(args)) => Some(args),
                None => None,
            };

            Ok(Some(LessPreprocessor {
                lessopen_template,
                lessclose_template,
                kind,
            }))
        }

        /// Substitutes occurrences of "%s".
        pub fn argsub(
            template: &Vec<String>,
            substitutions: Vec<impl AsRef<Path>>,
        ) -> Vec<OsString> {
            let mut i = 0;
            template
                .iter()
                .map(|arg| {
                    if arg == "%s" {
                        let replaced = substitutions
                            .get(i)
                            .map(|replacement| OsString::from(replacement.as_ref()))
                            .unwrap_or(OsString::from("%s"));

                        i += 1;
                        replaced
                    } else {
                        OsString::from(arg)
                    }
                })
                .collect()
        }
    }
}

#[cfg(feature = "preprocessor-lessopen")]
pub fn lessopen<'a>(lessopen: String, lessclose: Option<String>) -> Option<Preprocessor<'a>> {
    match __feature_preprocessor_lessopen::LessPreprocessor::new(lessopen, lessclose) {
        Err(_) => None, // Silently ignore errors like with the pager.
        Ok(None) => None,
        Ok(Some(preprocessor)) => Some(Preprocessor(Box::new(preprocessor))),
    }
}

#[test]
fn test_try_parse_utf8_char() {
    assert_eq!(try_parse_utf8_char(&[0x20]), Some((' ', 1)));
    assert_eq!(try_parse_utf8_char(&[0x20, 0x20]), Some((' ', 1)));
    assert_eq!(try_parse_utf8_char(&[0x20, 0xef]), Some((' ', 1)));

    assert_eq!(try_parse_utf8_char(&[0x00]), Some(('\x00', 1)));
    assert_eq!(try_parse_utf8_char(&[0x1b]), Some(('\x1b', 1)));

    assert_eq!(try_parse_utf8_char(&[0xc3, 0xa4]), Some(('ä', 2)));
    assert_eq!(try_parse_utf8_char(&[0xc3, 0xa4, 0xef]), Some(('ä', 2)));
    assert_eq!(try_parse_utf8_char(&[0xc3, 0xa4, 0x20]), Some(('ä', 2)));

    assert_eq!(try_parse_utf8_char(&[0xe2, 0x82, 0xac]), Some(('€', 3)));
    assert_eq!(
        try_parse_utf8_char(&[0xe2, 0x82, 0xac, 0xef]),
        Some(('€', 3))
    );
    assert_eq!(
        try_parse_utf8_char(&[0xe2, 0x82, 0xac, 0x20]),
        Some(('€', 3))
    );

    assert_eq!(try_parse_utf8_char(&[0xe2, 0x88, 0xb0]), Some(('∰', 3)));

    assert_eq!(
        try_parse_utf8_char(&[0xf0, 0x9f, 0x8c, 0x82]),
        Some(('🌂', 4))
    );
    assert_eq!(
        try_parse_utf8_char(&[0xf0, 0x9f, 0x8c, 0x82, 0xef]),
        Some(('🌂', 4))
    );
    assert_eq!(
        try_parse_utf8_char(&[0xf0, 0x9f, 0x8c, 0x82, 0x20]),
        Some(('🌂', 4))
    );

    assert_eq!(try_parse_utf8_char(&[]), None);
    assert_eq!(try_parse_utf8_char(&[0xef]), None);
    assert_eq!(try_parse_utf8_char(&[0xef, 0x20]), None);
    assert_eq!(try_parse_utf8_char(&[0xf0, 0xf0]), None);
}

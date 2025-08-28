use std::convert::TryFrom;
use std::fs;
use std::fs::File;
use std::io::{self, BufRead, BufReader, Read};
use std::path::{Path, PathBuf};

use clircle::{Clircle, Identifier};
use content_inspector::{self, ContentType};

use crate::error::*;

/// A description of an Input source.
/// This tells bat how to refer to the input.
#[derive(Clone)]
pub struct InputDescription {
    pub(crate) name: String,

    /// The input title.
    /// This replaces the name if provided.
    title: Option<String>,

    /// The input kind.
    kind: Option<String>,

    /// A summary description of the input.
    /// Defaults to "{kind} '{name}'"
    summary: Option<String>,
}

impl InputDescription {
    /// Creates a description for an input.
    pub fn new(name: impl Into<String>) -> Self {
        InputDescription {
            name: name.into(),
            title: None,
            kind: None,
            summary: None,
        }
    }

    pub fn set_kind(&mut self, kind: Option<String>) {
        self.kind = kind;
    }

    pub fn set_summary(&mut self, summary: Option<String>) {
        self.summary = summary;
    }

    pub fn set_title(&mut self, title: Option<String>) {
        self.title = title;
    }

    pub fn title(&self) -> &String {
        match &self.title {
            Some(title) => title,
            None => &self.name,
        }
    }

    pub fn kind(&self) -> Option<&String> {
        self.kind.as_ref()
    }

    pub fn summary(&self) -> String {
        self.summary.clone().unwrap_or_else(|| match &self.kind {
            None => self.name.clone(),
            Some(kind) => format!("{} '{}'", kind.to_lowercase(), self.name),
        })
    }
}

pub(crate) enum InputKind<'a> {
    OrdinaryFile(PathBuf),
    StdIn,
    CustomReader(Box<dyn Read + 'a>),
}

impl InputKind<'_> {
    pub fn description(&self) -> InputDescription {
        match self {
            InputKind::OrdinaryFile(ref path) => InputDescription::new(path.to_string_lossy()),
            InputKind::StdIn => InputDescription::new("STDIN"),
            InputKind::CustomReader(_) => InputDescription::new("READER"),
        }
    }
}

#[derive(Clone, Default)]
pub(crate) struct InputMetadata {
    pub(crate) user_provided_name: Option<PathBuf>,
    pub(crate) size: Option<u64>,
}

pub struct Input<'a> {
    pub(crate) kind: InputKind<'a>,
    pub(crate) metadata: InputMetadata,
    pub(crate) description: InputDescription,
}

pub(crate) enum OpenedInputKind {
    OrdinaryFile(PathBuf),
    StdIn,
    CustomReader,
}

pub(crate) struct OpenedInput<'a> {
    pub(crate) kind: OpenedInputKind,
    pub(crate) metadata: InputMetadata,
    pub(crate) reader: InputReader<'a>,
    pub(crate) description: InputDescription,
}

impl OpenedInput<'_> {
    /// Get the path of the file:
    /// If this was set by the metadata, that will take priority.
    /// If it wasn't, it will use the real file path (if available).
    pub(crate) fn path(&self) -> Option<&PathBuf> {
        self.metadata
            .user_provided_name
            .as_ref()
            .or(match self.kind {
                OpenedInputKind::OrdinaryFile(ref path) => Some(path),
                _ => None,
            })
    }
}

impl<'a> Input<'a> {
    pub fn ordinary_file(path: impl AsRef<Path>) -> Self {
        Self::_ordinary_file(path.as_ref())
    }

    fn _ordinary_file(path: &Path) -> Self {
        let kind = InputKind::OrdinaryFile(path.to_path_buf());
        let metadata = InputMetadata {
            size: fs::metadata(path).map(|m| m.len()).ok(),
            ..InputMetadata::default()
        };

        Input {
            description: kind.description(),
            metadata,
            kind,
        }
    }

    pub fn stdin() -> Self {
        let kind = InputKind::StdIn;
        Input {
            description: kind.description(),
            metadata: InputMetadata::default(),
            kind,
        }
    }

    pub fn from_reader(reader: Box<dyn Read + 'a>) -> Self {
        let kind = InputKind::CustomReader(reader);
        Input {
            description: kind.description(),
            metadata: InputMetadata::default(),
            kind,
        }
    }

    pub fn is_stdin(&self) -> bool {
        matches!(self.kind, InputKind::StdIn)
    }

    pub fn with_name(self, provided_name: Option<impl AsRef<Path>>) -> Self {
        self._with_name(provided_name.as_ref().map(|it| it.as_ref()))
    }

    fn _with_name(mut self, provided_name: Option<&Path>) -> Self {
        if let Some(name) = provided_name {
            self.description.name = name.to_string_lossy().to_string()
        }

        self.metadata.user_provided_name = provided_name.map(|n| n.to_owned());
        self
    }

    pub fn description(&self) -> &InputDescription {
        &self.description
    }

    pub fn description_mut(&mut self) -> &mut InputDescription {
        &mut self.description
    }

    pub(crate) fn open<R: BufRead + 'a>(
        self,
        stdin: R,
        stdout_identifier: Option<&Identifier>,
    ) -> Result<OpenedInput<'a>> {
        let description = self.description().clone();
        match self.kind {
            InputKind::StdIn => {
                if let Some(stdout) = stdout_identifier {
                    let input_identifier = Identifier::try_from(clircle::Stdio::Stdin)
                        .map_err(|e| format!("Stdin: Error identifying file: {e}"))?;
                    if stdout.surely_conflicts_with(&input_identifier) {
                        return Err("IO circle detected. The input from stdin is also an output. Aborting to avoid infinite loop.".into());
                    }
                }

                Ok(OpenedInput {
                    kind: OpenedInputKind::StdIn,
                    description,
                    metadata: self.metadata,
                    reader: InputReader::new(stdin),
                })
            }

            InputKind::OrdinaryFile(path) => Ok(OpenedInput {
                kind: OpenedInputKind::OrdinaryFile(path.clone()),
                description,
                metadata: self.metadata,
                reader: {
                    let mut file = File::open(&path)
                        .map_err(|e| format!("'{}': {e}", path.to_string_lossy()))?;
                    if file.metadata()?.is_dir() {
                        return Err(format!("'{}' is a directory.", path.to_string_lossy()).into());
                    }

                    if let Some(stdout) = stdout_identifier {
                        let input_identifier = Identifier::try_from(file).map_err(|e| {
                            format!("{}: Error identifying file: {e}", path.to_string_lossy())
                        })?;
                        if stdout.surely_conflicts_with(&input_identifier) {
                            return Err(format!(
                                "IO circle detected. The input from '{}' is also an output. Aborting to avoid infinite loop.",
                                path.to_string_lossy()
                            )
                            .into());
                        }
                        file = input_identifier.into_inner().expect("The file was lost in the clircle::Identifier, this should not have happened...");
                    }

                    InputReader::new(BufReader::new(file))
                },
            }),
            InputKind::CustomReader(reader) => Ok(OpenedInput {
                description,
                kind: OpenedInputKind::CustomReader,
                metadata: self.metadata,
                reader: InputReader::new(BufReader::new(reader)),
            }),
        }
    }
}

pub(crate) struct InputReader<'a> {
    inner: Box<dyn BufRead + 'a>,
    pub(crate) first_line: Vec<u8>,
    pub(crate) content_type: Option<ContentType>,
}

impl<'a> InputReader<'a> {
    pub(crate) fn new<R: BufRead + 'a>(mut reader: R) -> InputReader<'a> {
        let mut first_line = vec![];
        reader.read_until(b'\n', &mut first_line).ok();

        let content_type = if first_line.is_empty() {
            None
        } else {
            Some(content_inspector::inspect(&first_line[..]))
        };

        if content_type == Some(ContentType::UTF_16LE) {
            read_utf16_line(&mut reader, &mut first_line, 0x00, 0x0A).ok();
        } else if content_type == Some(ContentType::UTF_16BE) {
            read_utf16_line(&mut reader, &mut first_line, 0x0A, 0x00).ok();
        }

        InputReader {
            inner: Box::new(reader),
            first_line,
            content_type,
        }
    }

    pub(crate) fn read_line(&mut self, buf: &mut Vec<u8>) -> io::Result<bool> {
        if !self.first_line.is_empty() {
            buf.append(&mut self.first_line);
            return Ok(true);
        }

        if self.content_type == Some(ContentType::UTF_16LE) {
            return read_utf16_line(&mut self.inner, buf, 0x00, 0x0A);
        }
        if self.content_type == Some(ContentType::UTF_16BE) {
            return read_utf16_line(&mut self.inner, buf, 0x0A, 0x00);
        }

        let res = self.inner.read_until(b'\n', buf).map(|size| size > 0)?;
        Ok(res)
    }
}

fn read_utf16_line<R: BufRead>(
    reader: &mut R,
    buf: &mut Vec<u8>,
    read_until_char: u8,
    preceded_by_char: u8,
) -> io::Result<bool> {
    loop {
        let mut temp = Vec::new();
        let n = reader.read_until(read_until_char, &mut temp)?;
        if n == 0 {
            // EOF reached
            break;
        }
        buf.extend_from_slice(&temp);
        if buf.len() >= 2
            && buf[buf.len() - 2] == preceded_by_char
            && buf[buf.len() - 1] == read_until_char
        {
            // end of line found
            break;
        }
        // end of line not found, keep going
    }
    Ok(!buf.is_empty())
}

#[test]
fn basic() {
    let content = b"#!/bin/bash\necho hello";
    let mut reader = InputReader::new(&content[..]);

    assert_eq!(b"#!/bin/bash\n", &reader.first_line[..]);

    let mut buffer = vec![];

    let res = reader.read_line(&mut buffer);
    assert!(res.is_ok());
    assert!(res.unwrap());
    assert_eq!(b"#!/bin/bash\n", &buffer[..]);

    buffer.clear();

    let res = reader.read_line(&mut buffer);
    assert!(res.is_ok());
    assert!(res.unwrap());
    assert_eq!(b"echo hello", &buffer[..]);

    buffer.clear();

    let res = reader.read_line(&mut buffer);
    assert!(res.is_ok());
    assert!(!res.unwrap());
    assert!(buffer.is_empty());
}

#[test]
fn utf16le() {
    let content = b"\xFF\xFE\x73\x00\x0A\x00\x64\x00";
    let mut reader = InputReader::new(&content[..]);

    assert_eq!(b"\xFF\xFE\x73\x00\x0A\x00", &reader.first_line[..]);

    let mut buffer = vec![];

    let res = reader.read_line(&mut buffer);
    assert!(res.is_ok());
    assert!(res.unwrap());
    assert_eq!(b"\xFF\xFE\x73\x00\x0A\x00", &buffer[..]);

    buffer.clear();

    let res = reader.read_line(&mut buffer);
    assert!(res.is_ok());
    assert!(res.unwrap());
    assert_eq!(b"\x64\x00", &buffer[..]);

    buffer.clear();

    let res = reader.read_line(&mut buffer);
    assert!(res.is_ok());
    assert!(!res.unwrap());
    assert!(buffer.is_empty());
}

#[test]
fn utf16le_issue3367() {
    let content = b"\xFF\xFE\x0A\x4E\x00\x4E\x0A\x4F\x00\x52\x0A\x00\
        \x6F\x00\x20\x00\x62\x00\x61\x00\x72\x00\x0A\x00\
        \x68\x00\x65\x00\x6C\x00\x6C\x00\x6F\x00\x20\x00\x77\x00\x6F\x00\x72\x00\x6C\x00\x64\x00";
    let mut reader = InputReader::new(&content[..]);

    assert_eq!(
        b"\xFF\xFE\x0A\x4E\x00\x4E\x0A\x4F\x00\x52\x0A\x00",
        &reader.first_line[..]
    );

    let mut buffer = vec![];

    let res = reader.read_line(&mut buffer);
    assert!(res.is_ok());
    assert!(res.unwrap());
    assert_eq!(
        b"\xFF\xFE\x0A\x4E\x00\x4E\x0A\x4F\x00\x52\x0A\x00",
        &buffer[..]
    );

    buffer.clear();

    let res = reader.read_line(&mut buffer);
    assert!(res.is_ok());
    assert!(res.unwrap());
    assert_eq!(
        b"\x6F\x00\x20\x00\x62\x00\x61\x00\x72\x00\x0A\x00",
        &buffer[..]
    );

    buffer.clear();

    let res = reader.read_line(&mut buffer);
    assert!(res.is_ok());
    assert!(res.unwrap());
    assert_eq!(
        b"\x68\x00\x65\x00\x6C\x00\x6C\x00\x6F\x00\x20\x00\x77\x00\x6F\x00\x72\x00\x6C\x00\x64\x00",
        &buffer[..]
    );

    buffer.clear();

    let res = reader.read_line(&mut buffer);
    assert!(res.is_ok());
    assert!(!res.unwrap());
    assert!(buffer.is_empty());
}

use std::convert::TryFrom;
use std::fs;
use std::fs::File;
use std::io::{self, BufRead, BufReader, Read};
use std::path::{Path, PathBuf};

use clircle::{Clircle, Identifier};
use content_inspector::{self, ContentType};
use once_cell::unsync::Lazy;

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

impl<'a> InputKind<'a> {
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
        soft_limit: Option<usize>,
        hard_limit: Option<usize>,
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
                    reader: InputReader::new(stdin, soft_limit, hard_limit),
                })
            }

            InputKind::OrdinaryFile(path) => Ok(OpenedInput {
                kind: OpenedInputKind::OrdinaryFile(path.clone()),
                description,
                metadata: self.metadata,
                reader: {
                    let mut file = File::open(&path)
                        .map_err(|e| format!("'{}': {}", path.to_string_lossy(), e))?;
                    if file.metadata()?.is_dir() {
                        return Err(format!("'{}' is a directory.", path.to_string_lossy()).into());
                    }

                    if let Some(stdout) = stdout_identifier {
                        let input_identifier = Identifier::try_from(file).map_err(|e| {
                            format!("{}: Error identifying file: {}", path.to_string_lossy(), e)
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

                    InputReader::new(BufReader::new(file), soft_limit, hard_limit)
                },
            }),
            InputKind::CustomReader(reader) => Ok(OpenedInput {
                description,
                kind: OpenedInputKind::CustomReader,
                metadata: self.metadata,
                reader: InputReader::new(BufReader::new(reader), soft_limit, hard_limit),
            }),
        }
    }
}

pub(crate) struct InputReader<'a> {
    inner: LimitBuf<'a>,
    pub(crate) first_line: Vec<u8>,
    pub(crate) content_type: Option<ContentType>,
}

impl<'a> InputReader<'a> {
    pub(crate) fn new<R: Read + 'a>(
        reader: R,
        soft_limit: Option<usize>,
        hard_limit: Option<usize>,
    ) -> InputReader<'a> {
        let mut input_reader = InputReader {
            inner: LimitBuf::new(
                reader,
                4096,
                soft_limit.unwrap_or(usize::MAX),
                hard_limit.unwrap_or(usize::MAX),
            ),
            first_line: vec![],
            content_type: None,
        };

        input_reader.read_first_line().ok();

        let content_type = if input_reader.first_line.is_empty() {
            None
        } else {
            Some(content_inspector::inspect(&input_reader.first_line[..]))
        };

        if content_type == Some(ContentType::UTF_16LE) {
            input_reader
                .inner
                .read_until(0x00, &mut input_reader.first_line)
                .ok();
        }

        input_reader.content_type = content_type;
        input_reader
    }

    fn read_first_line(&mut self) -> std::result::Result<bool, ReaderError> {
        let mut first_line = vec![];
        let res = self.read_line(&mut first_line);
        self.first_line = first_line;

        res
    }

    pub(crate) fn read_line(
        &mut self,
        buf: &mut Vec<u8>,
    ) -> std::result::Result<bool, ReaderError> {
        if !self.first_line.is_empty() {
            buf.append(&mut self.first_line);
            return Ok(true);
        }

        let res = self.inner.read_until(b'\n', buf).map(|size| size > 0)?;

        if self.content_type == Some(ContentType::UTF_16LE) {
            let _ = self.inner.read_until(0x00, buf);
        }

        Ok(res)
    }
}

struct LimitBuf<'a> {
    reader: Box<dyn Read + 'a>,
    inner: Box<[u8]>,
    start: usize,
    len: usize,
    soft_limit: usize,
    hard_limit: usize,
}

impl<'a> LimitBuf<'a> {
    pub fn new<R: Read + 'a>(
        reader: R,
        buf_size: usize,
        soft_limit: usize,
        hard_limit: usize,
    ) -> Self {
        Self {
            reader: Box::new(reader),
            inner: vec![0u8; buf_size].into_boxed_slice(),
            start: 0,
            len: 0,
            soft_limit,
            hard_limit,
        }
    }

    pub fn read_until(
        &mut self,
        byte: u8,
        buf: &mut Vec<u8>,
    ) -> std::result::Result<usize, ReaderError> {
        let mut end_byte_reached = false;
        let mut total_bytes = 0;

        let mut soft_limit_hit = false;
        let capacity = self.inner.len();
        let mut drop_buf = Lazy::new(|| Vec::with_capacity(capacity));

        while !end_byte_reached {
            if self.len == 0 {
                let bytes = self.reader.read(&mut self.inner)?;
                self.len += bytes;
                self.start = 0;

                if bytes == 0 {
                    break;
                }
            }

            let bytes = (&self.inner[self.start..self.start + self.len])
                .read_until(byte, if soft_limit_hit { &mut drop_buf } else { buf })?;
            end_byte_reached = self.inner[self.start + bytes - 1] == byte;

            if soft_limit_hit {
                drop_buf.clear();
            }

            self.len -= bytes;
            self.start += bytes;
            total_bytes += bytes;

            if total_bytes > self.hard_limit {
                return Err(ReaderError::HardLimitHit);
            } else if total_bytes > self.soft_limit {
                soft_limit_hit = true;
            }
        }

        if soft_limit_hit {
            Err(ReaderError::SoftLimitHit)
        } else {
            Ok(total_bytes)
        }
    }
}

#[derive(Debug)]
pub(crate) enum ReaderError {
    SoftLimitHit,
    HardLimitHit,
    IoError(io::Error),
}

impl From<io::Error> for ReaderError {
    fn from(value: io::Error) -> Self {
        Self::IoError(value)
    }
}

#[test]
fn basic() {
    let content = b"#!/bin/bash\necho hello";
    let mut reader = InputReader::new(&content[..], None, None);

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
    let mut reader = InputReader::new(&content[..], None, None);

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

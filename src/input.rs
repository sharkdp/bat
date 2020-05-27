use std::ffi::{OsStr, OsString};
use std::fs::File;
use std::io::{self, BufRead, BufReader, Read};

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
        match self.title.as_ref() {
            Some(ref title) => title,
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
    OrdinaryFile(OsString),
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
    pub(crate) user_provided_name: Option<OsString>,
}

pub struct Input<'a> {
    pub(crate) kind: InputKind<'a>,
    pub(crate) metadata: InputMetadata,
    pub(crate) description: InputDescription,
}

pub(crate) enum OpenedInputKind {
    OrdinaryFile(OsString),
    StdIn,
    CustomReader,
}

pub(crate) struct OpenedInput<'a> {
    pub(crate) kind: OpenedInputKind,
    pub(crate) metadata: InputMetadata,
    pub(crate) reader: InputReader<'a>,
    pub(crate) description: InputDescription,
}

impl<'a> Input<'a> {
    pub fn ordinary_file(path: &OsStr) -> Self {
        let kind = InputKind::OrdinaryFile(path.to_os_string());
        Input {
            description: kind.description(),
            metadata: InputMetadata::default(),
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
        if let InputKind::StdIn = self.kind {
            true
        } else {
            false
        }
    }

    pub fn with_name(mut self, provided_name: Option<&OsStr>) -> Self {
        match provided_name {
            Some(name) => self.description.name = name.to_string_lossy().to_string(),
            None => {}
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

    pub(crate) fn open<R: BufRead + 'a>(self, stdin: R) -> Result<OpenedInput<'a>> {
        let description = self.description().clone();
        match self.kind {
            InputKind::StdIn => Ok(OpenedInput {
                kind: OpenedInputKind::StdIn,
                description,
                metadata: self.metadata,
                reader: InputReader::new(stdin),
            }),
            InputKind::OrdinaryFile(path) => Ok(OpenedInput {
                kind: OpenedInputKind::OrdinaryFile(path.clone()),
                description,
                metadata: self.metadata,
                reader: {
                    let file = File::open(&path)
                        .map_err(|e| format!("'{}': {}", path.to_string_lossy(), e))?;
                    if file.metadata()?.is_dir() {
                        return Err(format!("'{}' is a directory.", path.to_string_lossy()).into());
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
    fn new<R: BufRead + 'a>(mut reader: R) -> InputReader<'a> {
        let mut first_line = vec![];
        reader.read_until(b'\n', &mut first_line).ok();

        let content_type = if first_line.is_empty() {
            None
        } else {
            Some(content_inspector::inspect(&first_line[..]))
        };

        if content_type == Some(ContentType::UTF_16LE) {
            reader.read_until(0x00, &mut first_line).ok();
        }

        InputReader {
            inner: Box::new(reader),
            first_line,
            content_type,
        }
    }

    pub(crate) fn read_line(&mut self, buf: &mut Vec<u8>) -> io::Result<bool> {
        if self.first_line.is_empty() {
            let res = self.inner.read_until(b'\n', buf).map(|size| size > 0)?;

            if self.content_type == Some(ContentType::UTF_16LE) {
                self.inner.read_until(0x00, buf).ok();
            }

            Ok(res)
        } else {
            buf.append(&mut self.first_line);
            Ok(true)
        }
    }
}

#[test]
fn basic() {
    let content = b"#!/bin/bash\necho hello";
    let mut reader = InputReader::new(&content[..]);

    assert_eq!(b"#!/bin/bash\n", &reader.first_line[..]);

    let mut buffer = vec![];

    let res = reader.read_line(&mut buffer);
    assert!(res.is_ok());
    assert_eq!(true, res.unwrap());
    assert_eq!(b"#!/bin/bash\n", &buffer[..]);

    buffer.clear();

    let res = reader.read_line(&mut buffer);
    assert!(res.is_ok());
    assert_eq!(true, res.unwrap());
    assert_eq!(b"echo hello", &buffer[..]);

    buffer.clear();

    let res = reader.read_line(&mut buffer);
    assert!(res.is_ok());
    assert_eq!(false, res.unwrap());
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
    assert_eq!(true, res.unwrap());
    assert_eq!(b"\xFF\xFE\x73\x00\x0A\x00", &buffer[..]);

    buffer.clear();

    let res = reader.read_line(&mut buffer);
    assert!(res.is_ok());
    assert_eq!(true, res.unwrap());
    assert_eq!(b"\x64\x00", &buffer[..]);

    buffer.clear();

    let res = reader.read_line(&mut buffer);
    assert!(res.is_ok());
    assert_eq!(false, res.unwrap());
    assert!(buffer.is_empty());
}

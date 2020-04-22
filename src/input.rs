use std::ffi::{OsStr, OsString};
use std::fs::File;
use std::io::{self, BufRead, BufReader, Read};

use content_inspector::{self, ContentType};

use crate::errors::*;

const THEME_PREVIEW_FILE: &[u8] = include_bytes!("../assets/theme_preview.rs");

#[derive(Debug, Clone)]
pub struct InputDescription {
    pub full: String,
    pub prefix: String,
    pub name: String,
}

pub enum InputKind {
    OrdinaryFile(OsString),
    StdIn,
    ThemePreviewFile,
    CustomReader(Box<dyn BufRead>),
}

#[derive(Clone, Default)]
pub struct InputMetadata {
    pub user_provided_name: Option<OsString>,
}

pub struct Input {
    pub kind: InputKind,
    pub metadata: InputMetadata,
}

pub enum OpenedInputKind {
    OrdinaryFile(OsString),
    StdIn,
    ThemePreviewFile,
    CustomReader,
}

pub struct OpenedInput<'a> {
    pub kind: OpenedInputKind,
    pub metadata: InputMetadata,
    pub reader: InputReader<'a>,
}

impl Input {
    pub fn ordinary_file(path: &OsStr) -> Self {
        Input {
            kind: InputKind::OrdinaryFile(path.to_os_string()),
            metadata: InputMetadata::default(),
        }
    }

    pub fn stdin() -> Self {
        Input {
            kind: InputKind::StdIn,
            metadata: InputMetadata::default(),
        }
    }

    pub fn theme_preview_file() -> Self {
        Input {
            kind: InputKind::ThemePreviewFile,
            metadata: InputMetadata::default(),
        }
    }

    pub fn is_stdin(&self) -> bool {
        if let InputKind::StdIn = self.kind {
            true
        } else {
            false
        }
    }

    pub fn set_provided_name(&mut self, provided_name: Option<&OsStr>) {
        self.metadata.user_provided_name = provided_name.map(|n| n.to_owned());
    }

    pub fn open<'a, R: BufRead + 'a>(self, stdin: R) -> Result<OpenedInput<'a>> {
        match self.kind {
            InputKind::StdIn => Ok(OpenedInput {
                kind: OpenedInputKind::StdIn,
                metadata: self.metadata,
                reader: InputReader::new(stdin),
            }),
            InputKind::OrdinaryFile(path) => Ok(OpenedInput {
                kind: OpenedInputKind::OrdinaryFile(path.clone()),
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
            InputKind::ThemePreviewFile => Ok(OpenedInput {
                kind: OpenedInputKind::ThemePreviewFile,
                metadata: self.metadata,
                reader: InputReader::new(THEME_PREVIEW_FILE),
            }),
            InputKind::CustomReader(reader) => Ok(OpenedInput {
                kind: OpenedInputKind::CustomReader,
                metadata: self.metadata,
                reader: InputReader::new(BufReader::new(reader)),
            }),
        }
    }
}

impl<'a> OpenedInput<'a> {
    pub fn description(&self) -> InputDescription {
        if let Some(ref name) = self.metadata.user_provided_name {
            InputDescription {
                full: format!("file '{}'", name.to_string_lossy()),
                prefix: "File: ".to_owned(),
                name: name.to_string_lossy().into_owned(),
            }
        } else {
            match self.kind {
                OpenedInputKind::OrdinaryFile(ref path) => InputDescription {
                    full: format!("file '{}'", path.to_string_lossy()),
                    prefix: "File: ".to_owned(),
                    name: path.to_string_lossy().into_owned(),
                },
                OpenedInputKind::StdIn => InputDescription {
                    full: "STDIN".to_owned(),
                    prefix: "".to_owned(),
                    name: "STDIN".to_owned(),
                },
                OpenedInputKind::ThemePreviewFile => InputDescription {
                    full: "".to_owned(),
                    prefix: "".to_owned(),
                    name: "".to_owned(),
                },
                OpenedInputKind::CustomReader => InputDescription {
                    full: "reader".to_owned(),
                    prefix: "".to_owned(),
                    name: "READER".into(),
                },
            }
        }
    }
}

pub struct InputReader<'a> {
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

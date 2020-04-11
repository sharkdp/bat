use std::ffi::OsStr;
use std::fs::File;
use std::io::{self, BufRead, BufReader};

use content_inspector::{self, ContentType};

use crate::errors::*;

const THEME_PREVIEW_FILE: &[u8] = include_bytes!("../assets/theme_preview.rs");

pub struct InputFileReader<'a> {
    inner: Box<dyn BufRead + 'a>,
    pub(crate) first_line: Vec<u8>,
    pub(crate) content_type: Option<ContentType>,
}

impl<'a> InputFileReader<'a> {
    fn new<R: BufRead + 'a>(mut reader: R) -> InputFileReader<'a> {
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

        InputFileReader {
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

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct OrdinaryFile<'a> {
    path: &'a OsStr,
    user_provided_name: Option<&'a OsStr>,
}

impl<'a> OrdinaryFile<'a> {
    pub fn from_path(path: &'a OsStr) -> OrdinaryFile<'a> {
        OrdinaryFile {
            path,
            user_provided_name: None,
        }
    }

    pub fn from_path_with_name(
        path: &'a OsStr,
        user_provided_name: Option<&'a OsStr>,
    ) -> OrdinaryFile<'a> {
        OrdinaryFile {
            path,
            user_provided_name,
        }
    }

    pub fn filename(&self) -> &'a OsStr {
        self.user_provided_name.unwrap_or_else(|| self.path)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum InputFile<'a> {
    StdIn(Option<&'a OsStr>),
    Ordinary(OrdinaryFile<'a>),
    ThemePreviewFile,
}

impl<'a> InputFile<'a> {
    pub(crate) fn get_reader(&self, stdin: &'a io::Stdin) -> Result<InputFileReader> {
        match self {
            InputFile::StdIn(_) => Ok(InputFileReader::new(stdin.lock())),
            InputFile::Ordinary(ofile) => {
                let file = File::open(ofile.path)
                    .map_err(|e| format!("'{}': {}", ofile.path.to_string_lossy(), e))?;

                if file.metadata()?.is_dir() {
                    return Err(
                        format!("'{}' is a directory.", ofile.path.to_string_lossy()).into(),
                    );
                }

                Ok(InputFileReader::new(BufReader::new(file)))
            }
            InputFile::ThemePreviewFile => Ok(InputFileReader::new(THEME_PREVIEW_FILE)),
        }
    }
}

#[test]
fn basic() {
    let content = b"#!/bin/bash\necho hello";
    let mut reader = InputFileReader::new(&content[..]);

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
    let mut reader = InputFileReader::new(&content[..]);

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

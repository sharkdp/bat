use std::fs::File;
use std::io::{self, BufRead, BufReader};

use content_inspector::{self, ContentType};

use errors::*;

const THEME_PREVIEW_FILE: &[u8] = include_bytes!("../assets/theme_preview.rs");

pub struct InputFileReader<'a> {
    inner: Box<dyn BufRead + 'a>,
    pub first_line: Vec<u8>,
    pub content_type: ContentType,
}

impl<'a> InputFileReader<'a> {
    fn new<R: BufRead + 'a>(mut reader: R) -> InputFileReader<'a> {
        let mut first_line = vec![];
        reader.read_until(b'\n', &mut first_line).ok();

        let content_type = content_inspector::inspect(&first_line[..]);

        if content_type == ContentType::UTF_16LE {
            reader.read_until(0x00, &mut first_line).ok();
        }

        InputFileReader {
            inner: Box::new(reader),
            first_line,
            content_type,
        }
    }

    pub fn read_line(&mut self, buf: &mut Vec<u8>) -> io::Result<bool> {
        if self.first_line.is_empty() {
            let res = self.inner.read_until(b'\n', buf).map(|size| size > 0)?;

            if self.content_type == ContentType::UTF_16LE {
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
pub enum InputFile<'a> {
    StdIn,
    Ordinary(&'a str),
    ThemePreviewFile,
}

impl<'a> InputFile<'a> {
    pub fn get_reader(&self, stdin: &'a io::Stdin) -> Result<InputFileReader> {
        match self {
            InputFile::StdIn => Ok(InputFileReader::new(stdin.lock())),
            InputFile::Ordinary(filename) => {
                let file = File::open(filename).map_err(|e| format!("'{}': {}", filename, e))?;

                if file.metadata()?.is_dir() {
                    return Err(format!("'{}' is a directory.", filename).into());
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

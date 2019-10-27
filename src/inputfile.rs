use std::fs::File;
use std::io::{self, BufRead, BufReader, ErrorKind};

use content_inspector::{self, ContentType};

use crate::errors::*;

const THEME_PREVIEW_FILE: &[u8] = include_bytes!("../assets/theme_preview.rs");
const MAXIMUM_LINE_LENGTH: usize = 1024 * 1024;

/// This function is like the read until present in the standard library
/// but with a limit in the read buffer size
fn read_until<R: BufRead + ?Sized>(
    r: &mut R,
    buf: &mut Vec<u8>,
    pattern: u8,
) -> io::Result<ReadStatus> {
    let mut read = 0;
    let limit = MAXIMUM_LINE_LENGTH;
    loop {
        let (done, used) = {
            let available = match r.fill_buf() {
                Ok(n) => n,
                Err(ref e) if e.kind() == ErrorKind::Interrupted => continue,
                Err(e) => return Err(e),
            };
            match memchr::memchr(pattern, available) {
                Some(i) => {
                    buf.extend_from_slice(&available[..=i]);
                    (true, i + 1)
                }
                None => {
                    buf.extend_from_slice(available);
                    (false, available.len())
                }
            }
        };
        r.consume(used);
        read += used;
        if read == 0 {
            return Ok(ReadStatus::Eof);
        }
        if done || used == 0 {
            return Ok(ReadStatus::Ok);
        }
        if read > limit {
            return Ok(ReadStatus::Overflow);
        }
    }
}

pub struct InputFileReader<'a> {
    inner: Box<dyn BufRead + 'a>,
    pub current_line: Vec<u8>,
    pub content_type: Option<ContentType>,
    pub line_number: usize,
    last_status: ReadStatus,
    first_read: bool,
}

#[derive(Debug, PartialEq, Copy, Clone)]
enum ReadStatus {
    Ok,
    Eof,
    Overflow,
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum ReadPosition {
    FullLine,
    Start,
    Middle,
    End,
    Eof,
}

impl ReadPosition {
    pub fn is_begin_of_line(self) -> bool {
        match self {
            ReadPosition::FullLine | ReadPosition::Start => true,
            _ => false,
        }
    }

    pub fn is_end_of_line(self) -> bool {
        match self {
            ReadPosition::FullLine | ReadPosition::End => true,
            _ => false,
        }
    }
}

impl<'a> InputFileReader<'a> {
    fn new<R: BufRead + 'a>(mut reader: R) -> Result<InputFileReader<'a>> {
        let mut current_line = vec![];
        let last_status = read_until(&mut reader, &mut current_line, b'\n')?;

        let content_type = if current_line.is_empty() {
            None
        } else {
            Some(content_inspector::inspect(&current_line[..]))
        };

        if content_type == Some(ContentType::UTF_16LE) {
            read_until(&mut reader, &mut current_line, 0x00)?;
        }

        Ok(InputFileReader {
            inner: Box::new(reader),
            current_line,
            content_type,
            last_status,
            line_number: 0,
            first_read: true,
        })
    }

    pub fn read_line(&mut self) -> Result<ReadPosition> {
        if self.current_line.is_empty() {
            return Ok(ReadPosition::Eof);
        }

        if self.first_read {
            self.first_read = false;
            if self.last_status != ReadStatus::Overflow {
                self.line_number += 1;
            }
            let pos = match self.last_status {
                ReadStatus::Eof => ReadPosition::Eof,
                ReadStatus::Ok => ReadPosition::FullLine,
                ReadStatus::Overflow => ReadPosition::Start,
            };
            Ok(pos)
        } else {
            self.current_line.clear();
            let this_status = read_until(&mut self.inner, &mut self.current_line, b'\n')?;

            if this_status != ReadStatus::Overflow
                && self.content_type == Some(ContentType::UTF_16LE)
            {
                read_until(&mut self.inner, &mut self.current_line, 0x00)?;
            }

            let res = match (self.last_status, this_status) {
                (ReadStatus::Ok, ReadStatus::Ok) => ReadPosition::FullLine,
                (ReadStatus::Ok, ReadStatus::Overflow) => ReadPosition::Start,
                (ReadStatus::Overflow, ReadStatus::Ok) => ReadPosition::End,
                (ReadStatus::Overflow, ReadStatus::Overflow) => ReadPosition::Middle,
                (ReadStatus::Eof, _) => ReadPosition::Eof,
                (_, ReadStatus::Eof) => ReadPosition::Eof,
            };
            if res.is_begin_of_line() {
                self.line_number += 1;
            }
            self.last_status = this_status;
            Ok(res)
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
            InputFile::StdIn => InputFileReader::new(stdin.lock()),
            InputFile::Ordinary(filename) => {
                let file = File::open(filename).map_err(|e| format!("'{}': {}", filename, e))?;

                if file.metadata()?.is_dir() {
                    return Err(format!("'{}' is a directory.", filename).into());
                }

                InputFileReader::new(BufReader::new(file))
            }
            InputFile::ThemePreviewFile => InputFileReader::new(THEME_PREVIEW_FILE),
        }
    }
}

#[test]
fn basic() {
    let content = b"#!/bin/bash\necho hello";
    let mut reader = InputFileReader::new(&content[..]).unwrap();

    assert_eq!(b"#!/bin/bash\n", &reader.current_line[..]);

    let res = reader.read_line();
    assert!(res.is_ok());
    assert_eq!(ReadPosition::FullLine, res.unwrap());
    assert_eq!(b"#!/bin/bash\n", &reader.current_line[..]);

    let res = reader.read_line();
    assert!(res.is_ok());
    assert_eq!(ReadPosition::FullLine, res.unwrap());
    assert_eq!(b"echo hello", &reader.current_line[..]);

    let res = reader.read_line();
    assert!(res.is_ok());
    assert_eq!(ReadPosition::Eof, res.unwrap());
    assert!(reader.current_line.is_empty());
}

#[test]
fn utf16le() {
    let content = b"\xFF\xFE\x73\x00\x0A\x00\x64\x00";
    let mut reader = InputFileReader::new(&content[..]).unwrap();

    assert_eq!(b"\xFF\xFE\x73\x00\x0A\x00", &reader.current_line[..]);

    let res = reader.read_line();
    assert!(res.is_ok());
    assert_eq!(ReadPosition::FullLine, res.unwrap());
    assert_eq!(b"\xFF\xFE\x73\x00\x0A\x00", &reader.current_line[..]);

    let res = reader.read_line();
    assert!(res.is_ok());
    assert_eq!(ReadPosition::FullLine, res.unwrap());
    assert_eq!(b"\x64\x00", &reader.current_line[..]);

    let res = reader.read_line();
    assert!(res.is_ok());
    assert_eq!(ReadPosition::Eof, res.unwrap());
    assert!(reader.current_line.is_empty());
}

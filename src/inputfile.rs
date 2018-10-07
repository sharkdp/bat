use std::fs::File;
use std::io::{self, BufRead, BufReader};

use errors::*;

const THEME_PREVIEW_FILE: &[u8] = include_bytes!("../assets/theme_preview.rs");

pub struct InputFileReader<'a> {
    inner: Box<dyn BufRead + 'a>,
}

impl<'a> InputFileReader<'a> {
    fn new<R: BufRead + 'a>(reader: R) -> InputFileReader<'a> {
        InputFileReader {
            inner: Box::new(reader),
        }
    }

    pub fn read_line(&mut self, buf: &mut Vec<u8>) -> io::Result<bool> {
        self.inner.read_until(b'\n', buf).map(|size| size > 0)
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
                let file = File::open(filename)?;

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
    let content = b"hello\nworld";
    let mut reader = InputFileReader::new(&content[..]);

    let mut buffer = vec![];

    let res = reader.read_line(&mut buffer);
    assert!(res.is_ok());
    assert_eq!(true, res.unwrap());
    assert_eq!(b"hello\n", &buffer[..]);

    buffer.clear();

    let res = reader.read_line(&mut buffer);
    assert!(res.is_ok());
    assert_eq!(true, res.unwrap());
    assert_eq!(b"world", &buffer[..]);

    buffer.clear();

    let res = reader.read_line(&mut buffer);
    assert!(res.is_ok());
    assert_eq!(false, res.unwrap());
    assert!(buffer.is_empty());
}

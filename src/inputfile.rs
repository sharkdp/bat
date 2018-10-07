use std::fs::File;
use std::io::{self, BufRead, BufReader};

use errors::*;

const THEME_PREVIEW_FILE: &[u8] = include_bytes!("../assets/theme_preview.rs");

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum InputFile<'a> {
    StdIn,
    Ordinary(&'a str),
    ThemePreviewFile,
}

impl<'a> InputFile<'a> {
    pub fn get_reader(&self, stdin: &'a io::Stdin) -> Result<Box<dyn BufRead + 'a>> {
        match self {
            InputFile::StdIn => Ok(Box::new(stdin.lock())),
            InputFile::Ordinary(filename) => {
                let file = File::open(filename)?;

                if file.metadata()?.is_dir() {
                    return Err(format!("'{}' is a directory.", filename).into());
                }

                Ok(Box::new(BufReader::new(file)))
            }
            InputFile::ThemePreviewFile => Ok(Box::new(THEME_PREVIEW_FILE)),
        }
    }
}

use std::convert::TryFrom;
use std::fs::File;
use std::io::{self, stdin, BufRead, BufReader, Read, Stdin};
use std::path::{Path, PathBuf};
use std::pin::Pin;

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

    #[cfg(feature = "preprocessor")]
    preprocessed: bool,
}

impl InputDescription {
    /// Creates a description for an input.
    pub fn new(name: impl Into<String>) -> Self {
        InputDescription {
            name: name.into(),
            title: None,
            kind: None,
            summary: None,

            #[cfg(feature = "preprocessor")]
            preprocessed: false,
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
            Some(kind) => {
                #[cfg(feature = "preprocessor")]
                if self.preprocessed() {
                    return format!("{} '{}' <PP>", kind.to_lowercase(), self.name);
                }

                format!("{} '{}'", kind.to_lowercase(), self.name)
            }
        })
    }

    #[cfg(feature = "preprocessor")]
    pub(crate) fn preprocessed(&self) -> bool {
        self.preprocessed
    }

    #[cfg(feature = "preprocessor")]
    pub fn set_preprocessed(&mut self, preprocessed: bool) {
        self.preprocessed = preprocessed;
    }
}

#[cfg(feature = "preprocessor")]
pub(crate) trait InputPreprocessor<'a> {
    fn open(&self, input: Input<'a>, handle: &InputHandle) -> Result<OpenedInput<'a>>;
}

pub(crate) enum InputKind<'a> {
    OrdinaryFile(PathBuf),
    StdIn(Option<Box<dyn BufRead + 'a>>),
    CustomReader(Box<dyn Read + 'a>),
}

impl<'a> InputKind<'a> {
    pub fn description(&self) -> InputDescription {
        match self {
            InputKind::OrdinaryFile(ref path) => InputDescription::new(path.to_string_lossy()),
            InputKind::StdIn(_) => InputDescription::new("STDIN"),
            InputKind::CustomReader(_) => InputDescription::new("READER"),
        }
    }
}

#[derive(Clone, Default)]
pub(crate) struct InputMetadata {
    pub(crate) user_provided_name: Option<PathBuf>,
}

pub struct Input<'a> {
    pub(crate) kind: InputKind<'a>,
    pub(crate) metadata: InputMetadata,
    pub(crate) description: InputDescription,
}

pub(crate) struct InputHandle {
    pub(crate) stdout_identifier: Option<Identifier>,
}

pub(crate) trait OpenedInputHandle {
    fn close(&mut self) -> Result<()>;
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
    pub(crate) warnings: Vec<Error>,
    pub(crate) handles: Vec<Box<dyn OpenedInputHandle + 'a>>,
}

impl<'a> Drop for OpenedInput<'a> {
    fn drop(&mut self) {
        match self.close_handles() {
            Ok(()) => (),
            Err(errors) => {
                panic!("Error while closing input handle: {}", &errors[0])
            }
        }
    }
}

impl<'a> OpenedInput<'a> {
    fn close_handles(&mut self) -> std::result::Result<(), Vec<Error>> {
        let mut handles: Vec<Box<dyn OpenedInputHandle>> = Vec::new();
        std::mem::swap(&mut handles, &mut self.handles);

        let errors: Vec<Error> = handles
            .into_iter()
            .map(|mut handle| handle.close())
            .filter_map(Result::err)
            .collect();

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    pub fn close(mut self) -> std::result::Result<(), Vec<Error>> {
        self.close_handles()
    }
}

impl<'a> Input<'a> {
    pub fn ordinary_file(path: impl AsRef<Path>) -> Self {
        Self::_ordinary_file(path.as_ref())
    }

    fn _ordinary_file(path: &Path) -> Self {
        let kind = InputKind::OrdinaryFile(path.to_path_buf());
        Input {
            description: kind.description(),
            metadata: InputMetadata::default(),
            kind,
        }
    }

    pub fn stdin() -> Self {
        let kind = InputKind::StdIn(None);
        Input {
            description: kind.description(),
            metadata: InputMetadata::default(),
            kind,
        }
    }

    #[cfg(test)]
    pub(crate) fn stdin_with_contents(contents: impl BufRead + 'a) -> Self {
        let kind = InputKind::StdIn(Some(Box::new(contents)));
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
        matches!(self.kind, InputKind::StdIn(_))
    }

    pub fn with_name(self, provided_name: Option<impl AsRef<Path>>) -> Self {
        self._with_name(provided_name.as_ref().map(|it| it.as_ref()))
    }

    #[cfg(feature = "preprocessor")]
    pub(crate) fn preprocessed_from(mut self, input: &Input<'a>) -> Self {
        self.description.clone_from(&input.description);
        self.description.preprocessed = true;
        self.metadata.user_provided_name = input
            .metadata
            .user_provided_name
            .as_ref()
            .cloned()
            .or(input.path().map(ToOwned::to_owned));

        self
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

    pub fn path(&self) -> Option<&Path> {
        match &self.kind {
            InputKind::OrdinaryFile(path) => Some(path),
            _ => None,
        }
    }

    pub(crate) fn open(self, handle: &InputHandle) -> Result<OpenedInput<'a>> {
        let description = self.description().clone();
        match self.kind {
            InputKind::StdIn(stdin_contents) => {
                if let Some(stdout) = &handle.stdout_identifier {
                    let input_identifier = Identifier::try_from(clircle::Stdio::Stdin)
                        .map_err(|e| format!("Stdin: Error identifying file: {}", e))?;
                    if stdout.surely_conflicts_with(&input_identifier) {
                        return Err("IO circle detected. The input from stdin is also an output. Aborting to avoid infinite loop.".into());
                    }
                }

                if let Some(stdin_contents) = stdin_contents {
                    Ok(OpenedInput {
                        kind: OpenedInputKind::StdIn,
                        description,
                        metadata: self.metadata,
                        warnings: Vec::new(),
                        reader: InputReader::new(stdin_contents),
                        handles: Vec::new(),
                    })
                } else {
                    let handle = Box::new(OpenedStdinInputHandle::new());
                    Ok(OpenedInput {
                        kind: OpenedInputKind::StdIn,
                        description,
                        metadata: self.metadata,
                        warnings: Vec::new(),
                        reader: unsafe { handle.reader() },
                        handles: vec![handle],
                    })
                }
            }

            InputKind::OrdinaryFile(path) => Ok(OpenedInput {
                kind: OpenedInputKind::OrdinaryFile(path.clone()),
                description,
                metadata: self.metadata,
                warnings: Vec::new(),
                handles: Vec::new(),
                reader: {
                    let mut file = File::open(&path)
                        .map_err(|e| format!("'{}': {}", path.to_string_lossy(), e))?;
                    if file.metadata()?.is_dir() {
                        return Err(format!("'{}' is a directory.", path.to_string_lossy()).into());
                    }

                    if let Some(stdout) = &handle.stdout_identifier {
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
                        file = input_identifier.into_inner().expect("The file was lost in the clircle::Identifier, this should not have happended...");
                    }

                    InputReader::new(BufReader::new(file))
                },
            }),

            InputKind::CustomReader(reader) => Ok(OpenedInput {
                description,
                kind: OpenedInputKind::CustomReader,
                metadata: self.metadata,
                reader: InputReader::new(BufReader::new(reader)),
                warnings: Vec::new(),
                handles: Vec::new(),
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

/// A OpenedInputHandle that stores a locked Stdin reader.
///
/// SAFETY NOTES:
/// - The reader **MUST NOT** be accessed after the handle is closed.
/// - If the handle is never closed, memory will be leaked.
///
/// IMPLEMENTATION NOTES:
/// - The unsafeness is fine as long as the handle is added to the `OpenedInput.handles` vector.
///   That will ensure that this lives long enough to where stdin() no longer needs to be read from.
///
struct OpenedStdinInputHandle {
    stdin: Pin<Box<Stdin>>,
}

impl OpenedStdinInputHandle {
    fn new() -> Self {
        OpenedStdinInputHandle {
            stdin: Box::pin(stdin()),
        }
    }

    unsafe fn reader<'a>(&'a self) -> InputReader<'static> {
        InputReader::new(std::mem::transmute::<&'a Stdin, &'static Stdin>(&self.stdin).lock())
    }
}

impl OpenedInputHandle for OpenedStdinInputHandle {
    fn close(&mut self) -> Result<()> {
        Ok(()) // This is fine. The `stdin` field will be dropped when this handle gets dropped.
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

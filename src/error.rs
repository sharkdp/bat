use std::io::Write;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum Error {
    #[error(transparent)]
    Io(#[from] ::std::io::Error),
    #[error(transparent)]
    SyntectError(#[from] ::syntect::LoadingError),
    #[error(transparent)]
    ParseIntError(#[from] ::std::num::ParseIntError),
    #[error(transparent)]
    GlobParsingError(#[from] ::globset::Error),
    #[error(transparent)]
    SerdeYamlError(#[from] ::serde_yaml::Error),
    #[error("unable to detect syntax for {0}")]
    UndetectedSyntax(String),
    #[error("unknown syntax: '{0}'")]
    UnknownSyntax(String),
    #[error("Unknown style '{0}'")]
    UnknownStyle(String),
    #[error("Use of bat as a pager is disallowed in order to avoid infinite recursion problems")]
    InvalidPagerValueBat,
    #[error("{0}")]
    Msg(String),
}

impl From<&'static str> for Error {
    fn from(s: &'static str) -> Self {
        Error::Msg(s.to_owned())
    }
}

impl From<String> for Error {
    fn from(s: String) -> Self {
        Error::Msg(s)
    }
}

pub type Result<T> = std::result::Result<T, Error>;

pub fn default_error_handler(error: &Error, output: &mut dyn Write) {
    use ansi_term::Colour::Red;

    match error {
        Error::Io(ref io_error) if io_error.kind() == ::std::io::ErrorKind::BrokenPipe => {
            ::std::process::exit(0);
        }
        Error::SerdeYamlError(_) => {
            writeln!(
                output,
                "{}: Error while parsing metadata.yaml file: {}",
                Red.paint("[bat error]"),
                error
            )
            .ok();
        }
        _ => {
            writeln!(output, "{}: {}", Red.paint("[bat error]"), error).ok();
        }
    };
}

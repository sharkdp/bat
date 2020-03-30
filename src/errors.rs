use error_chain::error_chain;

error_chain! {
    foreign_links {
        Clap(clap::Error);
        Io(std::io::Error);
        SyntectError(syntect::LoadingError);
        ParseIntError(std::num::ParseIntError);
        GlobParsingError(globset::Error);
    }
}

pub fn default_error_handler(error: &Error) {
    match error {
        Error(ErrorKind::Io(ref io_error), _)
            if io_error.kind() == std::io::ErrorKind::BrokenPipe =>
        {
            std::process::exit(0);
        }
        _ => {
            use ansi_term::Colour::Red;
            eprintln!("{}: {}", Red.paint("[bat error]"), error);
        }
    };
}

// Mock out a type for clap::Error if we aren't pulling in a dependency on clap.
//
// This can be removed after migrating away from error_chain to some modern
// derive-based error library such as thiserror, in favor of:
//
//     #[derive(Error)]
//     pub enum Error {
//         #[cfg(feature = "application")]
//         Clap(clap::Error),
//         ...
//     }
//
#[cfg(not(feature = "application"))]
mod clap {
    use std::fmt::{self, Debug, Display};

    pub struct Error(());

    impl Display for Error {
        fn fmt(&self, _formatter: &mut fmt::Formatter) -> fmt::Result {
            unreachable!()
        }
    }

    impl Debug for Error {
        fn fmt(&self, _formatter: &mut fmt::Formatter) -> fmt::Result {
            unreachable!()
        }
    }

    impl std::error::Error for Error {}
}

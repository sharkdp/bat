use error_chain::error_chain;

error_chain! {
    foreign_links {
        Clap(::clap::Error) #[cfg(feature = "application")];
        Io(::std::io::Error);
        SyntectError(::syntect::LoadingError);
        ParseIntError(::std::num::ParseIntError);
        GlobParsingError(::globset::Error);
    }
}

pub fn default_error_handler(error: &Error) {
    match error {
        Error(ErrorKind::Io(ref io_error), _)
            if io_error.kind() == ::std::io::ErrorKind::BrokenPipe =>
        {
            ::std::process::exit(0);
        }
        _ => {
            use ansi_term::Colour::Red;
            eprintln!("{}: {}", Red.paint("[bat error]"), error);
        }
    };
}

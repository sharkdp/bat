use error_chain::error_chain;

error_chain! {
    foreign_links {
        Clap(::clap::Error) #[cfg(feature = "application")];
        Io(::std::io::Error);
        SyntectError(::syntect::LoadingError);
        ParseIntError(::std::num::ParseIntError);
        GlobParsingError(::globset::Error);
        SerdeYamlError(::serde_yaml::Error);
    }
}

pub fn default_error_handler(error: &Error) {
    use ansi_term::Colour::Red;

    match error {
        Error(ErrorKind::Io(ref io_error), _)
            if io_error.kind() == ::std::io::ErrorKind::BrokenPipe =>
        {
            ::std::process::exit(0);
        }
        Error(ErrorKind::SerdeYamlError(_), _) => {
            eprintln!(
                "{}: Error while parsing metadata.yaml file: {}",
                Red.paint("[bat error]"),
                error
            );
        }
        _ => {
            eprintln!("{}: {}", Red.paint("[bat error]"), error);
        }
    };
}

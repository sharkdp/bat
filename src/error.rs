use error_chain::error_chain;
use std::io::Write;

error_chain! {
    foreign_links {
        Clap(::clap::Error) #[cfg(feature = "application")];
        Io(::std::io::Error);
        SyntectError(::syntect::LoadingError);
        ParseIntError(::std::num::ParseIntError);
        GlobParsingError(::globset::Error);
        SerdeYamlError(::serde_yaml::Error);
    }

    errors {
        UndetectedSyntax(input: String) {
            description("unable to detect syntax"),
            display("unable to detect syntax for {}", input)
        }
        UnknownSyntax(name: String) {
            description("unknown syntax"),
            display("unknown syntax: '{}'", name)
        }
    }
}

pub fn default_error_handler(error: &Error, output: &mut dyn Write) {
    use ansi_term::Colour::Red;

    match error {
        Error(ErrorKind::Io(ref io_error), _)
            if io_error.kind() == ::std::io::ErrorKind::BrokenPipe =>
        {
            ::std::process::exit(0);
        }
        Error(ErrorKind::SerdeYamlError(_), _) => {
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

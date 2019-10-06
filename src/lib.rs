// `error_chain!` can recurse deeply
#![recursion_limit = "1024"]

#[macro_use]
extern crate error_chain;

#[macro_use]
extern crate lazy_static;

extern crate ansi_term;
extern crate atty;
extern crate console;
extern crate content_inspector;
extern crate dirs as dirs_rs;
extern crate encoding;
extern crate git2;
extern crate shell_words;
extern crate syntect;
extern crate wild;

pub mod assets;
pub mod config;
pub mod diff;
pub mod dirs;
pub mod inputfile;
pub mod line_range;
pub mod preprocessor;
pub mod style;
pub mod syntax_mapping;
pub mod terminal;
pub mod util;

pub mod errors {
    error_chain! {
        foreign_links {
            Clap(::clap::Error);
            Io(::std::io::Error);
            SyntectError(::syntect::LoadingError);
            ParseIntError(::std::num::ParseIntError);
        }
    }

    pub fn handle_error(error: &Error) {
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
}
// `error_chain!` can recurse deeply
#![recursion_limit = "1024"]

#[macro_use]
extern crate error_chain;

#[macro_use]
extern crate clap;

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

mod assets;
mod config;
mod diff;
mod dirs;
mod inputfile;
mod line_range;
mod preprocessor;
mod style;
mod syntax_mapping;
mod terminal;
mod util;

mod errors {
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
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
extern crate directories;
extern crate git2;
extern crate syntect;

mod app;
mod assets;
mod decorations;
mod diff;
mod features;
mod output;
mod printer;
mod style;
mod terminal;

use std::io;
use std::path::Path;
use std::process;

use app::App;
use assets::{clear_assets, config_dir, HighlightingAssets};
use features::{list_languages, print_files};

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
            &Error(ErrorKind::Io(ref io_error), _)
                if io_error.kind() == super::io::ErrorKind::BrokenPipe =>
            {
                super::process::exit(0);
            }
            _ => {
                use ansi_term::Colour::Red;
                eprintln!("{}: {}", Red.paint("[bat error]"), error);
            }
        };
    }
}

use errors::*;

/// Returns `Err(..)` upon fatal errors. Otherwise, returns `Some(true)` on full success and
/// `Some(false)` if any intermediate errors occurred (were printed).
fn run() -> Result<bool> {
    let app = App::new();

    match app.matches.subcommand() {
        ("cache", Some(cache_matches)) => {
            if cache_matches.is_present("init") {
                let source_dir = cache_matches.value_of("source").map(Path::new);
                let target_dir = cache_matches.value_of("target").map(Path::new);

                let assets = HighlightingAssets::from_files(source_dir)?;
                assets.save(target_dir)?;
            } else if cache_matches.is_present("clear") {
                clear_assets();
            } else if cache_matches.is_present("config-dir") {
                println!("{}", config_dir());
            }

            return Ok(true);
        }
        _ => {
            let config = app.config()?;
            let assets = HighlightingAssets::new();

            if app.matches.is_present("list-languages") {
                list_languages(&assets, config.term_width);
                return Ok(true);
            }

            if app.matches.is_present("list-themes") {
                let themes = &assets.theme_set.themes;
                for (theme, _) in themes.iter() {
                    println!("{}", theme);
                }
                return Ok(true);
            }

            print_files(&assets, &config)
        }
    }
}

fn main() {
    let result = run();

    match result {
        Err(error) => {
            handle_error(&error);
            process::exit(1);
        }
        Ok(false) => {
            process::exit(1);
        }
        Ok(true) => {
            process::exit(0);
        }
    }
}

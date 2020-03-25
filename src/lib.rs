// `error_chain!` can recurse deeply
#![recursion_limit = "1024"]

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

pub(crate) mod assets;
pub mod config;
pub(crate) mod controller;
mod decorations;
mod diff;
pub mod errors;
pub(crate) mod inputfile;
mod less;
pub(crate) mod line_range;
mod output;
mod preprocessor;
pub(crate) mod printer;
pub(crate) mod style;
pub(crate) mod syntax_mapping;
mod terminal;
pub(crate) mod wrap;

pub use assets::HighlightingAssets;
pub use controller::Controller;
pub use printer::{InteractivePrinter, Printer, SimplePrinter};

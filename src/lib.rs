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

pub mod assets;
pub mod config;
pub mod controller;
mod decorations;
mod diff;
pub mod errors;
pub mod inputfile;
mod less;
pub mod line_range;
mod output;
mod preprocessor;
mod printer;
pub mod style;
pub mod syntax_mapping;
mod terminal;

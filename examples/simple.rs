/// A simple program that prints its own source code using the bat library
use bat::{
    config::{Config, InputFile, OrdinaryFile},
    Controller, HighlightingAssets,
};
use std::ffi::OsStr;

fn main() {
    let path_to_this_file = OsStr::new(file!());

    let config = Config {
        files: vec![InputFile::Ordinary(OrdinaryFile::new(
            path_to_this_file,
            None,
        ))],
        colored_output: true,
        true_color: true,
        ..Default::default()
    };
    let assets = HighlightingAssets::from_binary();

    Controller::new(&config, &assets).run().expect("no errors");
}

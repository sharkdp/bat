use bat::{
    assets::HighlightingAssets,
    controller::Controller,
    inputfile::InputFile,
    style::{OutputComponent, OutputComponents},
    Config,
};
use std::collections::HashSet;

fn main() {
    let assets = HighlightingAssets::new();
    let mut config = Config {
        term_width: 100, // must be greater than 13 to enable style=numbers
        colored_output: true,
        true_color: true,
        output_components: OutputComponents(with_header()),
        ..Default::default()
    };
    let mut add_file = |file: &'static str| config.files.push(InputFile::Ordinary(file));

    add_file("Cargo.toml");
    // add_file("build.rs");

    let print = || Controller::new(&config, &assets).run();
    print().expect("no error");
}

fn with_header() -> HashSet<OutputComponent> {
    [OutputComponent::Header, OutputComponent::Grid]
        .iter()
        .cloned()
        .collect()
}

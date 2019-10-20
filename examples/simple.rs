use bat::{
    assets::HighlightingAssets,
    controller::Controller,
    inputfile::InputFile,
    style::{OutputComponent, OutputComponents},
    Config,
};
use console::Term;
use std::process;

fn main() {
    let files = std::env::args().skip(1).collect::<Vec<_>>();

    if files.is_empty() {
        eprintln!("No input files specified");
        process::exit(1);
    }

    let config = Config {
        term_width: Term::stdout().size().1 as usize,
        colored_output: true,
        true_color: true,
        output_components: OutputComponents::new(&[
            OutputComponent::Header,
            OutputComponent::Grid,
            OutputComponent::Numbers,
        ]),
        files: files.iter().map(|file| InputFile::Ordinary(file)).collect(),
        theme: "1337".into(),
        ..Default::default()
    };
    let assets = HighlightingAssets::new();

    Controller::new(&config, &assets).run().expect("no errors");
}

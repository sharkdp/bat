/// An example of running as a library and capturing the output.
/// Here we just print the output, but you could use it in your application.
use bat::{
    config::{Config, InputFile, StyleComponent, StyleComponents},
    errors::default_error_handler,
    Controller, HighlightingAssets,
};
use std::process;

fn main() {
    let files = std::env::args_os().skip(1).collect::<Vec<_>>();

    if files.is_empty() {
        eprintln!("No input files specified");
        process::exit(1);
    }

    let mut output = Vec::new();

    let config = Config {
        term_width: 80,
        colored_output: true, // you can still get coloured output
        true_color: true,
        style_components: StyleComponents::new(&[
            StyleComponent::Numbers,
        ]),
        files: files.iter().map(|file| InputFile::Ordinary(file)).collect(),
        ..Default::default()
    };
    let assets = HighlightingAssets::from_binary();

    let no_errors = Controller::new(&config, &assets).run_with_writer(&mut output, default_error_handler);
    if !no_errors {
        eprintln!("There were errors!");
        process::exit(1);
    }

    println!("{}", String::from_utf8(output).unwrap());
}

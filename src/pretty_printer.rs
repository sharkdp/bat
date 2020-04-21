use std::ffi::OsStr;

use crate::{
    config::{Config, InputFile, OrdinaryFile},
    errors::Result,
    Controller, HighlightingAssets,
};

pub struct PrettyPrinter<'a> {
    config: Config<'a>,
    assets: HighlightingAssets,
}

impl<'a> PrettyPrinter<'a> {
    pub fn new() -> Self {
        let mut config = Config::default();

        config.colored_output = true;
        config.true_color = true;

        PrettyPrinter {
            config,
            assets: HighlightingAssets::from_binary(),
        }
    }

    pub fn file(&'a mut self, path: &'a OsStr) -> &'a mut Self {
        self.config
            .files
            .push(InputFile::Ordinary(OrdinaryFile::from_path(path)));
        self
    }

    /// Whether or not the output should be colorized (default: true)
    pub fn colored_output(&mut self, yes: bool) -> &mut Self {
        self.config.colored_output = yes;
        self
    }

    pub fn run(&'a self) -> Result<bool> {
        let controller = Controller::new(&self.config, &self.assets);
        controller.run()
    }
}

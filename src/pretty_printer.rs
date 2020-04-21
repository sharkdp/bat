use std::ffi::OsStr;

use crate::{
    config::{Config, InputFile, OrdinaryFile, StyleComponents, WrappingMode},
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

    /// Add a file which should be pretty-printed
    pub fn file(&mut self, path: &OsStr) -> &mut Self {
        self.config
            .files
            .push(InputFile::Ordinary(OrdinaryFile::from_path(path)));
        self
    }

    /// Add multiple files which should be pretty-printed
    pub fn files<I, P>(&mut self, paths: I) -> &mut Self
    where
        I: IntoIterator<Item = P>,
        P: AsRef<OsStr>,
    {
        for path in paths {
            self.config
                .files
                .push(InputFile::Ordinary(OrdinaryFile::from_path(path.as_ref())));
        }
        self
    }

    /// The character width of the terminal (default: unlimited)
    pub fn term_width(&mut self, width: usize) -> &mut Self {
        self.config.term_width = width;
        self
    }

    /// The width of tab characters (default: None - do not turn tabs to spaces)
    pub fn tab_width(&mut self, tab_width: Option<usize>) -> &mut Self {
        self.config.tab_width = tab_width.unwrap_or(0);
        self
    }

    /// Whether or not the output should be colorized (default: true)
    pub fn colored_output(&mut self, yes: bool) -> &mut Self {
        self.config.colored_output = yes;
        self
    }

    /// Whether or not to output 24bit colors (default: true)
    pub fn true_color(&mut self, yes: bool) -> &mut Self {
        self.config.true_color = yes;
        self
    }

    /// Configure style elements (grid, line numbers, ...)
    pub fn style_components(&mut self, components: StyleComponents) -> &mut Self {
        self.config.style_components = components;
        self
    }

    /// Text wrapping mode (default: do not wrap)
    pub fn wrapping_mode(&mut self, wrapping_mode: WrappingMode) -> &mut Self {
        self.config.wrapping_mode = wrapping_mode;
        self
    }

    pub fn run(&'a self) -> Result<bool> {
        let controller = Controller::new(&self.config, &self.assets);
        controller.run()
    }
}

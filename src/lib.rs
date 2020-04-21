// `error_chain!` can recurse deeply
#![recursion_limit = "1024"]

pub(crate) mod assets;
pub(crate) mod assets_metadata;
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
pub mod pretty_printer;
pub(crate) mod printer;
pub(crate) mod style;
pub(crate) mod syntax_mapping;
mod terminal;
pub(crate) mod wrap;

pub use assets::HighlightingAssets;
pub use assets_metadata::AssetsMetadata;
pub use controller::Controller;
pub use pretty_printer::PrettyPrinter;
pub use printer::{InteractivePrinter, Printer, SimplePrinter};

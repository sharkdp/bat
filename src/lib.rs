/// `bat` is a library to print syntax highlighted content.
///
/// ```
/// use bat::PrettyPrinter;
///
/// PrettyPrinter::new()
///     .input_from_bytes(b"<span style=\"color: #ff00cc\">Hello world!</span>\n")
///     .language("html")
///     .run()
///     .expect("no errors");
/// ```
pub mod assets;
pub mod assets_metadata;
pub mod config;
pub mod controller;
mod decorations;
mod diff;
pub mod errors;
pub mod input;
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

pub use line_range::{HighlightedLineRanges, LineRange, LineRanges};
pub use pretty_printer::PrettyPrinter;
pub use style::{StyleComponent, StyleComponents};
pub use syntax_mapping::{MappingTarget, SyntaxMapping};
pub use wrap::WrappingMode;

#[cfg(feature = "paging")]
pub use config::PagingMode;

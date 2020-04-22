/// A very simple colorized `cat` clone, using `bat` as a library.
/// See `src/bin/bat` for the full `bat` application.
use bat::{PrettyPrinter, StyleComponent};
use console::Term;

fn main() {
    PrettyPrinter::new()
        .term_width(Term::stdout().size().1 as usize)
        .style_components(&[
            StyleComponent::Header,
            StyleComponent::Grid,
            StyleComponent::LineNumbers,
        ])
        .input_files(std::env::args_os().skip(1))
        .print()
        .expect("no errors");
}

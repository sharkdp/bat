/// A very simple colorized `cat` clone, using `bat` as a library.
/// See `src/bin/bat` for the full `bat` application.
use bat::{PrettyPrinter, StyleComponent, StyleComponents};
use console::Term;

fn main() {
    let mut printer = PrettyPrinter::new();

    printer
        .term_width(Term::stdout().size().1 as usize)
        .style_components(StyleComponents::new(&[
            StyleComponent::Header,
            StyleComponent::Grid,
            StyleComponent::Numbers,
        ]))
        .files(std::env::args_os().skip(1));

    printer.run().expect("no errors");
}

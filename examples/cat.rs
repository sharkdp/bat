/// A very simple colorized `cat` clone, using `bat` as a library.
/// See `src/bin/bat` for the full `bat` application.
use bat::PrettyPrinter;

fn main() {
    PrettyPrinter::new()
        .header(true)
        .grid(true)
        .line_numbers(true)
        .input_files(std::env::args_os().skip(1))
        .print()
        .unwrap();
}

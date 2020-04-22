/// A program that prints its own source code using the bat library
use bat::{LineRange, PrettyPrinter, WrappingMode};

fn main() {
    PrettyPrinter::new()
        .header(true)
        .grid(true)
        .line_numbers(true)
        .use_italics(true)
        // The following line will be highlighted in the output:
        .highlight(LineRange::new(line!() as usize, line!() as usize))
        .theme("1337")
        .wrapping_mode(WrappingMode::Character)
        .input_file(file!())
        .print()
        .expect("no errors");
}

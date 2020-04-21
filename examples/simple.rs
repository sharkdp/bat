/// A simple program that prints its own source code using the bat library
use bat::PrettyPrinter;
use std::ffi::OsStr;

fn main() {
    let path_to_this_file = OsStr::new(file!());

    let mut printer = PrettyPrinter::new();

    printer.file(path_to_this_file);

    printer.run().expect("no errors");
}

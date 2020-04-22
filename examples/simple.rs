/// A simple program that prints its own source code using the bat library
use bat::PrettyPrinter;
use std::ffi::OsStr;

fn main() {
    let path_to_this_file = OsStr::new(file!());

    PrettyPrinter::new()
        .input_file(path_to_this_file)
        .print()
        .expect("no errors");
}

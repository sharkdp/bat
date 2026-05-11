/// A small demonstration of the Input API.
/// This prints embedded bytes with a custom header and then reads from STDIN.
use bat::{Input, PrettyPrinter};

fn main() {
    PrettyPrinter::new()
        .header(true)
        .grid(true)
        .line_numbers(true)
        .inputs(vec![
            Input::from_bytes(b"echo 'Hello World!'")
                .name("embedded.sh") // Dummy name provided to detect the syntax.
                .kind("Embedded")
                .title("An embedded shell script."),
            Input::from_stdin().title("Standard Input").kind("FD"),
        ])
        .print()
        .unwrap();
}

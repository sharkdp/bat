/// A simple program that lists all supported syntaxes and themes.
use bat::PrettyPrinter;

fn main() {
    let printer = PrettyPrinter::new();

    println!("Syntaxes:");
    for syntax in printer.syntaxes() {
        println!("- {} ({})", syntax.name, syntax.file_extensions.join(", "));
    }

    println!();

    println!("Themes:");
    for theme in printer.themes() {
        println!("- {theme}");
    }
}

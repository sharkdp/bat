use ansi_term::Colour::Green;
use app::Config;
use assets::HighlightingAssets;
use diff::get_git_diff;
use errors::*;
use output::OutputType;
use printer::Printer;
use std::fs::File;
use std::io::{self, BufRead, BufReader};
use syntect::easy::HighlightLines;
use syntect::highlighting::Theme;
use syntect::parsing::SyntaxDefinition;

pub fn list_languages(assets: &HighlightingAssets, term_width: usize) {
    let mut languages = assets
        .syntax_set
        .syntaxes()
        .iter()
        .filter(|syntax| !syntax.hidden && !syntax.file_extensions.is_empty())
        .collect::<Vec<_>>();
    languages.sort_by_key(|lang| lang.name.to_uppercase());

    let longest = languages
        .iter()
        .map(|syntax| syntax.name.len())
        .max()
        .unwrap_or(32); // Fallback width if they have no language definitions.

    let comma_separator = ", ";
    let separator = " ";
    // Line-wrapping for the possible file extension overflow.
    let desired_width = term_width - longest - separator.len();

    for lang in languages {
        print!("{:width$}{}", lang.name, separator, width = longest);

        // Number of characters on this line so far, wrap before `desired_width`
        let mut num_chars = 0;

        let mut extension = lang.file_extensions.iter().peekable();
        while let Some(word) = extension.next() {
            // If we can't fit this word in, then create a line break and align it in.
            let new_chars = word.len() + comma_separator.len();
            if num_chars + new_chars >= desired_width {
                num_chars = 0;
                print!("\n{:width$}{}", "", separator, width = longest);
            }

            num_chars += new_chars;
            print!("{}", Green.paint(&word[..]));
            if extension.peek().is_some() {
                print!("{}", comma_separator);
            }
        }
        println!();
    }
}

pub fn print_files(assets: &HighlightingAssets, config: &Config) -> Result<bool> {
    let theme = assets.get_theme(config.theme.unwrap_or("Default"))?;
    let mut output_type = OutputType::from_mode(config.paging_mode);
    let handle = output_type.handle()?;
    let mut printer = Printer::new(handle, &config);
    let mut no_errors: bool = true;

    for file in &config.files {
        printer.line_changes = file.and_then(|filename| get_git_diff(filename));
        let syntax = assets.get_syntax(config.language, *file);

        let result = print_file(theme, &syntax, &mut printer, *file);

        if let Err(error) = result {
            handle_error(&error);
            no_errors = false;
        }
    }

    Ok(no_errors)
}

fn print_file(
    theme: &Theme,
    syntax: &SyntaxDefinition,
    printer: &mut Printer,
    filename: Option<&str>,
) -> Result<()> {
    let stdin = io::stdin(); // TODO: this is not always needed

    let mut reader: Box<BufRead> = match filename {
        None => Box::new(stdin.lock()),
        Some(filename) => Box::new(BufReader::new(File::open(filename)?)),
    };

    let mut highlighter = HighlightLines::new(syntax, theme);

    printer.print_header(filename)?;

    let mut buffer = Vec::new();
    while reader.read_until(b'\n', &mut buffer)? > 0 {
        {
            let line = String::from_utf8_lossy(&buffer);
            let regions = highlighter.highlight(line.as_ref());

            printer.print_line(&regions)?;
        }
        buffer.clear();
    }

    printer.print_footer()?;

    Ok(())
}

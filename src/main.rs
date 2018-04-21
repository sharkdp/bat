extern crate ansi_term;
extern crate atty;
extern crate console;
extern crate syntect;

#[macro_use]
extern crate clap;

use std::io::{BufRead, Result};
use std::path::Path;
use std::process;

use ansi_term::Colour::Fixed;
use atty::Stream;
use clap::{App, AppSettings, Arg, ArgMatches};
use console::Term;

use syntect::easy::HighlightFile;
use syntect::highlighting::ThemeSet;
use syntect::parsing::SyntaxSet;
use syntect::util::as_24_bit_terminal_escaped;

fn print_file<P: AsRef<Path>>(filename: P) -> Result<()> {
    let ss = SyntaxSet::load_defaults_nonewlines();
    let ts = ThemeSet::load_from_folder("/home/shark/Informatik/rust/bat/themes").unwrap();
    let theme = &ts.themes["Monokai"];

    let mut highlighter = HighlightFile::new(filename, &ss, theme).unwrap();

    let term = Term::stdout();
    let (_height, width) = term.size();

    println!("{}", width);
    let prefix = "     ┌";
    let line = "─".repeat(width as usize - prefix.len());
    println!("{}{}", Fixed(238).paint(prefix), Fixed(238).paint(line));

    for (line_nr, maybe_line) in highlighter.reader.lines().enumerate() {
        let line = maybe_line.unwrap_or("<INVALID UTF-8>".into());
        let regions = highlighter.highlight_lines.highlight(&line);

        println!(
            "{} {} {}",
            Fixed(244).paint(format!("{:4}", line_nr)),
            Fixed(238).paint("│"),
            as_24_bit_terminal_escaped(&regions, false)
        );
    }

    Ok(())
}

fn run(matches: &ArgMatches) -> Result<()> {
    if let Some(files) = matches.values_of("file") {
        for file in files {
            print_file(file)?;
        }
    }

    Ok(())
}

fn main() {
    let clap_color_setting = if atty::is(Stream::Stdout) {
        AppSettings::ColoredHelp
    } else {
        AppSettings::ColorNever
    };

    let matches = App::new("bat")
        .version(crate_version!())
        .setting(clap_color_setting)
        .setting(AppSettings::DeriveDisplayOrder)
        .setting(AppSettings::UnifiedHelpMessage)
        .setting(AppSettings::NextLineHelp)
        .max_term_width(90)
        .about("A cat(1) clone with wings.")
        .arg(
            Arg::with_name("file")
                .help("Files to print")
                .multiple(true)
                .empty_values(false),
        )
        .help_message("Print this help message.")
        .version_message("Show version information.")
        .get_matches();

    let result = run(&matches);

    if let Err(e) = result {
        eprintln!("Error: {}", e);
        process::exit(1);
    }
}

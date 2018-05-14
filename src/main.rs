// `error_chain!` can recurse deeply
#![recursion_limit = "1024"]

#[macro_use]
extern crate error_chain;

#[macro_use]
extern crate clap;

#[macro_use]
extern crate lazy_static;

extern crate ansi_term;
extern crate atty;
extern crate console;
extern crate directories;
extern crate git2;
extern crate syntect;

mod app;
mod assets;
mod diff;
mod printer;
mod style;
mod terminal;
mod decorations;

use std::fs::{self, File};
use std::io::{self, BufRead, BufReader, Write};
use std::process::{self, Child, Command, Stdio};

#[cfg(unix)]
use std::os::unix::fs::FileTypeExt;

use ansi_term::Colour::{Fixed, Green, Red, White, Yellow};
use ansi_term::Style;

use syntect::easy::HighlightLines;
use syntect::highlighting::Theme;
use syntect::parsing::SyntaxSet;

use app::{App, Config};
use assets::{config_dir, syntax_set_path, theme_set_path, HighlightingAssets};
use diff::get_git_diff;
use printer::Printer;

mod errors {
    error_chain! {
        foreign_links {
            Clap(::clap::Error);
            Io(::std::io::Error);
        }
    }
}

use errors::*;

enum OutputType {
    Pager(Child),
    Stdout(io::Stdout),
}

impl OutputType {
    fn pager() -> Result<Self> {
        Ok(OutputType::Pager(Command::new("less")
            .args(&["--quit-if-one-screen", "--RAW-CONTROL-CHARS", "--no-init"])
            .stdin(Stdio::piped())
            .spawn()
            .chain_err(|| "Could not spawn pager")?))
    }

    fn stdout() -> Self {
        OutputType::Stdout(io::stdout())
    }

    fn handle(&mut self) -> Result<&mut Write> {
        Ok(match *self {
            OutputType::Pager(ref mut command) => command
                .stdin
                .as_mut()
                .chain_err(|| "Could not open stdin for pager")?,
            OutputType::Stdout(ref mut handle) => handle,
        })
    }
}

impl Drop for OutputType {
    fn drop(&mut self) {
        if let OutputType::Pager(ref mut command) = *self {
            let _ = command.wait();
        }
    }
}

const GRID_COLOR: u8 = 238;
const LINE_NUMBER_COLOR: u8 = 244;

#[derive(Default)]
pub struct Colors {
    pub grid: Style,
    pub filename: Style,
    pub git_added: Style,
    pub git_removed: Style,
    pub git_modified: Style,
    pub line_number: Style,
}

impl Colors {
    fn plain() -> Self {
        Colors::default()
    }

    fn colored() -> Self {
        Colors {
            grid: Fixed(GRID_COLOR).normal(),
            filename: White.bold(),
            git_added: Green.normal(),
            git_removed: Red.normal(),
            git_modified: Yellow.normal(),
            line_number: Fixed(LINE_NUMBER_COLOR).normal(),
        }
    }
}

fn print_file(
    config: &Config,
    theme: &Theme,
    syntax_set: &SyntaxSet,
    printer: &mut Printer,
    filename: Option<&str>,
) -> Result<()> {
    let stdin = io::stdin(); // TODO: this is not always needed

    let mut reader: Box<BufRead> = match filename {
        None => Box::new(stdin.lock()),
        Some(filename) => Box::new(BufReader::new(File::open(filename)?)),
    };

    let syntax = match (config.language, filename) {
        (Some(language), _) => syntax_set.find_syntax_by_token(language),
        (None, Some(filename)) => {
            #[cfg(not(unix))]
            let may_read_from_file = true;

            // Do not peek at the file (to determine the syntax) if it is a FIFO because they can
            // only be read once.
            #[cfg(unix)]
            let may_read_from_file = !fs::metadata(filename)?.file_type().is_fifo();

            if may_read_from_file {
                syntax_set.find_syntax_for_file(filename)?
            } else {
                None
            }
        }
        (None, None) => None,
    };

    let syntax = syntax.unwrap_or_else(|| syntax_set.find_syntax_plain_text());
    let mut highlighter = HighlightLines::new(syntax, theme);

    printer.print_header(filename)?;

    let mut line_nr = 1;
    let mut line_buffer = String::new();
    loop {
        line_buffer.clear();
        let num_bytes = reader.read_line(&mut line_buffer);

        let line = match num_bytes {
            Ok(0) => {
                break;
            }
            Ok(_) => {
                if !line_buffer.ends_with('\n') {
                    line_buffer.push('\n');
                }
                &line_buffer
            }
            Err(_) => "<bat: INVALID UTF-8>\n",
        };

        let regions = highlighter.highlight(line);

        printer.print_line(line_nr, &regions)?;

        line_nr += 1;
    }

    printer.print_footer()?;

    Ok(())
}

fn get_output_type(paging: bool) -> OutputType {
    if paging {
        OutputType::pager().unwrap_or_else(|_| OutputType::stdout())
    } else {
        OutputType::stdout()
    }
}

fn run() -> Result<()> {
    let app = App::new();

    match app.matches.subcommand() {
        ("cache", Some(cache_matches)) => {
            if cache_matches.is_present("init") {
                let assets = HighlightingAssets::from_files()?;
                assets.save()?;
            } else if cache_matches.is_present("clear") {
                print!("Clearing theme set cache ... ");
                fs::remove_file(theme_set_path())?;
                println!("okay");

                print!("Clearing syntax set cache ... ");
                fs::remove_file(syntax_set_path())?;
                println!("okay");
            } else if cache_matches.is_present("config-dir") {
                println!("{}", config_dir());
            }
        }
        _ => {
            let config = app.config()?;

            let assets = HighlightingAssets::new();
            let theme = assets.get_theme(config.theme.unwrap_or("Default"))?;

            if app.matches.is_present("list-languages") {
                let languages = assets.syntax_set.syntaxes();

                let longest = languages
                    .iter()
                    .filter(|s| !s.hidden && !s.file_extensions.is_empty())
                    .map(|s| s.name.len())
                    .max()
                    .unwrap_or(32); // Fallback width if they have no language definitions.

                let separator = " ";
                for lang in languages {
                    if lang.hidden || lang.file_extensions.is_empty() {
                        continue;
                    }
                    print!("{:width$}{}", lang.name, separator, width = longest);

                    // Line-wrapping for the possible file extension overflow.
                    let desired_width = config.term_width - longest - separator.len();
                    // Number of characters on this line so far, wrap before `desired_width`
                    let mut num_chars = 0;

                    let comma_separator = ", ";
                    let mut extension = lang.file_extensions.iter().peekable();
                    while let Some(word) = extension.next() {
                        // If we can't fit this word in, then create a line break and align it in.
                        let new_chars = word.len() + comma_separator.len();
                        if num_chars + new_chars >= desired_width {
                            num_chars = 0;
                            print!("\n{:width$}{}", "", separator, width = longest);
                        }

                        num_chars += new_chars;
                        print!("{}", Green.paint(word as &str));
                        if extension.peek().is_some() {
                            print!("{}", comma_separator);
                        }
                    }
                    println!();
                }

                return Ok(());
            }

            if app.matches.is_present("list-themes") {
                let themes = &assets.theme_set.themes;
                for (theme, _) in themes.iter() {
                    println!("{}", theme);
                }
                return Ok(());
            }

            let mut output_type = get_output_type(config.paging);
            let handle = output_type.handle()?;
            let mut printer = Printer::new(handle, &config);

            for file in &config.files {
                printer.line_changes = file.and_then(|filename| get_git_diff(filename));

                print_file(&config, theme, &assets.syntax_set, &mut printer, *file)?;
            }
        }
    }

    Ok(())
}

fn main() {
    let result = run();

    if let Err(error) = result {
        match error {
            Error(ErrorKind::Io(ref io_error), _)
                if io_error.kind() == io::ErrorKind::BrokenPipe => {}
            _ => {
                eprintln!("{}: {}", Red.paint("[bat error]"), error);

                process::exit(1);
            }
        };
    }
}

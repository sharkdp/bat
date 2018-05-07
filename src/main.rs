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

mod printer;
mod terminal;

use std::collections::HashMap;
use std::env;
use std::fs::{self, File};
use std::io::{self, BufRead, BufReader, Write};
use std::path::{Path, PathBuf};
use std::process::{self, Child, Command, Stdio};

use ansi_term::Colour::{Fixed, Green, Red, White, Yellow};
use ansi_term::Style;
use atty::Stream;
use clap::{App, AppSettings, Arg, ArgGroup, SubCommand};
use directories::ProjectDirs;
use git2::{DiffOptions, IntoCString, Repository};

use syntect::dumps::{dump_to_file, from_binary, from_reader};
use syntect::easy::HighlightLines;
use syntect::highlighting::{Theme, ThemeSet};
use syntect::parsing::SyntaxSet;

use printer::Printer;

lazy_static! {
    static ref PROJECT_DIRS: ProjectDirs = ProjectDirs::from("", "", crate_name!());
}

mod errors {
    error_chain! {
        foreign_links {
            Io(::std::io::Error);
        }
    }
}

use errors::*;

pub enum OptionsStyle {
    Plain,
    LineNumbers,
    Full,
}

pub struct Options<'a> {
    pub true_color: bool,
    pub style: OptionsStyle,
    pub language: Option<&'a str>,
    pub colored_output: bool,
    pub paging: bool,
}

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

#[derive(Copy, Clone, Debug)]
pub enum LineChange {
    Added,
    RemovedAbove,
    RemovedBelow,
    Modified,
}

pub type LineChanges = HashMap<u32, LineChange>;

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
    options: &Options,
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

    let syntax = match (options.language, filename) {
        (Some(language), _) => syntax_set.find_syntax_by_token(language),
        (None, Some(filename)) => syntax_set.find_syntax_for_file(filename)?,
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
            Ok(_) => &line_buffer,
            Err(_) => "<bat: INVALID UTF-8>\n",
        };

        let regions = highlighter.highlight(line);

        printer.print_line(line_nr, &regions)?;

        line_nr += 1;
    }

    printer.print_footer()?;

    Ok(())
}

fn get_git_diff(filename: &str) -> Option<LineChanges> {
    let repo = Repository::discover(&filename).ok()?;
    let path_absolute = fs::canonicalize(&filename).ok()?;
    let path_relative_to_repo = path_absolute.strip_prefix(repo.workdir()?).ok()?;

    let mut diff_options = DiffOptions::new();
    let pathspec = path_relative_to_repo.into_c_string().ok()?;
    diff_options.pathspec(pathspec);
    diff_options.context_lines(0);

    let diff = repo.diff_index_to_workdir(None, Some(&mut diff_options))
        .ok()?;

    let mut line_changes: LineChanges = HashMap::new();

    let mark_section =
        |line_changes: &mut LineChanges, start: u32, end: i32, change: LineChange| {
            for line in start..(end + 1) as u32 {
                line_changes.insert(line, change);
            }
        };

    let _ = diff.foreach(
        &mut |_, _| true,
        None,
        Some(&mut |delta, hunk| {
            let path = delta.new_file().path().unwrap_or_else(|| Path::new(""));

            if path_relative_to_repo != path {
                return false;
            }

            let old_lines = hunk.old_lines();
            let new_start = hunk.new_start();
            let new_lines = hunk.new_lines();
            let new_end = (new_start + new_lines) as i32 - 1;

            if old_lines == 0 && new_lines > 0 {
                mark_section(&mut line_changes, new_start, new_end, LineChange::Added);
            } else if new_lines == 0 && old_lines > 0 {
                if new_start == 0 {
                    mark_section(&mut line_changes, 1, 1, LineChange::RemovedAbove);
                } else {
                    mark_section(
                        &mut line_changes,
                        new_start,
                        new_start as i32,
                        LineChange::RemovedBelow,
                    );
                }
            } else {
                mark_section(&mut line_changes, new_start, new_end, LineChange::Modified);
            }

            true
        }),
        None,
    );

    Some(line_changes)
}

fn get_output_type(paging: bool) -> OutputType {
    if paging {
        OutputType::pager().unwrap_or_else(|_| OutputType::stdout())
    } else {
        OutputType::stdout()
    }
}

fn is_truecolor_terminal() -> bool {
    env::var("COLORTERM")
        .map(|colorterm| colorterm == "truecolor" || colorterm == "24bit")
        .unwrap_or(false)
}

struct HighlightingAssets {
    pub syntax_set: SyntaxSet,
    pub theme_set: ThemeSet,
}

impl HighlightingAssets {
    fn theme_set_path() -> PathBuf {
        PROJECT_DIRS.cache_dir().join("theme_set")
    }

    fn syntax_set_path() -> PathBuf {
        PROJECT_DIRS.cache_dir().join("syntax_set")
    }

    fn from_files() -> Result<Self> {
        let config_dir = PROJECT_DIRS.config_dir();

        let theme_dir = config_dir.join("themes");

        let theme_set = ThemeSet::load_from_folder(&theme_dir).map_err(|_| {
            io::Error::new(
                io::ErrorKind::Other,
                format!(
                    "Could not load themes from '{}'",
                    theme_dir.to_string_lossy()
                ),
            )
        })?;

        let mut syntax_set = SyntaxSet::new();
        let syntax_dir = config_dir.join("syntax");
        let _ = syntax_set.load_syntaxes(syntax_dir, true);
        syntax_set.load_plain_text_syntax();

        Ok(HighlightingAssets {
            syntax_set,
            theme_set,
        })
    }

    fn save(&self) -> Result<()> {
        let cache_dir = PROJECT_DIRS.cache_dir();
        let _ = fs::create_dir(cache_dir);

        print!(
            "Writing theme set to {} ... ",
            Self::theme_set_path().to_string_lossy()
        );
        dump_to_file(&self.theme_set, &Self::theme_set_path()).map_err(|_| {
            io::Error::new(
                io::ErrorKind::Other,
                format!(
                    "Could not save theme set to {}",
                    Self::theme_set_path().to_string_lossy()
                ),
            )
        })?;
        println!("okay");

        print!(
            "Writing syntax set to {} ... ",
            Self::syntax_set_path().to_string_lossy()
        );
        dump_to_file(&self.syntax_set, &Self::syntax_set_path()).map_err(|_| {
            io::Error::new(
                io::ErrorKind::Other,
                format!(
                    "Could not save syntax set to {}",
                    Self::syntax_set_path().to_string_lossy()
                ),
            )
        })?;
        println!("okay");

        Ok(())
    }

    fn from_cache() -> Result<Self> {
        let syntax_set_file = File::open(&Self::syntax_set_path()).chain_err(|| {
            format!(
                "Could not load cached syntax set '{}'",
                Self::syntax_set_path().to_string_lossy()
            )
        })?;
        let mut syntax_set: SyntaxSet = from_reader(syntax_set_file).map_err(|_| {
            io::Error::new(
                io::ErrorKind::Other,
                format!("Could not parse cached syntax set"),
            )
        })?;
        syntax_set.link_syntaxes();

        let theme_set_file = File::open(&Self::theme_set_path()).chain_err(|| {
            format!(
                "Could not load cached theme set '{}'",
                Self::theme_set_path().to_string_lossy()
            )
        })?;
        let theme_set: ThemeSet = from_reader(theme_set_file).map_err(|_| {
            io::Error::new(
                io::ErrorKind::Other,
                format!("Could not parse cached theme set"),
            )
        })?;

        Ok(HighlightingAssets {
            syntax_set,
            theme_set,
        })
    }

    fn from_binary() -> Self {
        let mut syntax_set: SyntaxSet = from_binary(include_bytes!("../assets/syntax_set"));
        syntax_set.link_syntaxes();
        let theme_set: ThemeSet = from_binary(include_bytes!("../assets/theme_set"));

        HighlightingAssets {
            syntax_set,
            theme_set,
        }
    }
}

fn run() -> Result<()> {
    let interactive_terminal = atty::is(Stream::Stdout);

    let clap_color_setting = if interactive_terminal {
        AppSettings::ColoredHelp
    } else {
        AppSettings::ColorNever
    };

    let app_matches = App::new(crate_name!())
        .version(crate_version!())
        .global_setting(clap_color_setting)
        .global_setting(AppSettings::DeriveDisplayOrder)
        .global_setting(AppSettings::UnifiedHelpMessage)
        .global_setting(AppSettings::NextLineHelp)
        .setting(AppSettings::InferSubcommands)
        .setting(AppSettings::ArgsNegateSubcommands)
        .setting(AppSettings::DisableHelpSubcommand)
        .setting(AppSettings::VersionlessSubcommands)
        .max_term_width(90)
        .about(crate_description!())
        .arg(
            Arg::with_name("language")
                .short("l")
                .long("language")
                .help("Set the language for highlighting")
                .takes_value(true),
        )
        .arg(
            Arg::with_name("FILE")
                .help("File(s) to print")
                .multiple(true)
                .empty_values(false),
        )
        .arg(
            Arg::with_name("style")
                .long("style")
                .possible_values(&["auto", "plain", "line-numbers", "full"])
                .default_value("auto")
                .help("Additional info to display along with content"),
        )
        .arg(
            Arg::with_name("color")
                .long("color")
                .takes_value(true)
                .possible_values(&["auto", "never", "always"])
                .default_value("auto")
                .help("When to use colors"),
        )
        .arg(
            Arg::with_name("paging")
                .long("paging")
                .takes_value(true)
                .possible_values(&["auto", "never", "always"])
                .default_value("auto")
                .help("When to use the pager"),
        )
        .arg(
            Arg::with_name("list-languages")
                .long("list-languages")
                .help("Displays supported languages")
        )
        .subcommand(
            SubCommand::with_name("cache")
                .about("Modify the syntax-definition and theme cache")
                .arg(
                    Arg::with_name("init")
                        .long("init")
                        .short("i")
                        .help("Initialize the cache by loading from the config dir"),
                )
                .arg(
                    Arg::with_name("clear")
                        .long("clear")
                        .short("c")
                        .help("Reset the cache"),
                )
                .arg(
                    Arg::with_name("config-dir")
                        .long("config-dir")
                        .short("d")
                        .help("Show the configuration directory"),
                )
                .group(
                    ArgGroup::with_name("cache-actions")
                        .args(&["init", "clear", "config-dir"])
                        .required(true),
                ),
        )
        .help_message("Print this help message.")
        .version_message("Show version information.")
        .get_matches();

    match app_matches.subcommand() {
        ("cache", Some(cache_matches)) => {
            if cache_matches.is_present("init") {
                let assets = HighlightingAssets::from_files()?;
                assets.save()?;
            } else if cache_matches.is_present("clear") {
                print!("Clearing theme set cache ... ");
                fs::remove_file(HighlightingAssets::theme_set_path())?;
                println!("okay");

                print!("Clearing syntax set cache ... ");
                fs::remove_file(HighlightingAssets::syntax_set_path())?;
                println!("okay");
            } else if cache_matches.is_present("config-dir") {
                println!("{}", PROJECT_DIRS.config_dir().to_string_lossy());
            }
        }
        _ => {
            let files: Vec<Option<&str>> = app_matches
                .values_of("FILE")
                .map(|values| {
                    values
                        .map(|filename| {
                            if filename == "-" {
                                None
                            } else {
                                Some(filename)
                            }
                        })
                        .collect()
                })
                .unwrap_or_else(|| vec![None]); // read from stdin (None) if no args are given

            let options = Options {
                true_color: is_truecolor_terminal(),
                style: match app_matches.value_of("style") {
                    Some("plain") => OptionsStyle::Plain,
                    Some("line-numbers") => OptionsStyle::LineNumbers,
                    Some("full") => OptionsStyle::Full,
                    Some("auto") | _ => if interactive_terminal {
                        OptionsStyle::Full
                    } else {
                        OptionsStyle::Plain
                    },
                },
                language: app_matches.value_of("language"),
                colored_output: match app_matches.value_of("color") {
                    Some("always") => true,
                    Some("never") => false,
                    _ => interactive_terminal,
                },
                paging: match app_matches.value_of("paging") {
                    Some("always") => true,
                    Some("never") => false,
                    Some("auto") | _ => if files.contains(&None) {
                        // If we are reading from stdin, only enable paging if we write to an
                        // interactive terminal and if we do not *read* from an interactive
                        // terminal.
                        interactive_terminal && !atty::is(Stream::Stdin)
                    } else {
                        interactive_terminal
                    },
                },
            };

            let assets =
                HighlightingAssets::from_cache().unwrap_or(HighlightingAssets::from_binary());

            let theme = assets.theme_set.themes.get("Default").ok_or_else(|| {
                io::Error::new(
                    io::ErrorKind::Other,
                    format!("Could not find 'Default' theme"),
                )
            })?;

            if app_matches.is_present("list-languages") {
                let languages = assets.syntax_set.syntaxes();

                let longest = languages.iter()
                    .map(|s| s.name.len())
                    .max()
                    .unwrap_or(32); // Fallback width if they have no language definitions.

                let separator = " | ";
                for lang in languages {
                    print!("{:width$}{}", lang.name, separator, width = longest);

                    // Line-wrapping for the possible file extension overflow.
                    let desired_width = 48;
                    // Number of characters on this line so far, wrap before `desired_width`
                    let mut num_chars = 0;

                    let mut extension = lang.file_extensions.iter().peekable();
                    while let Some(word) = extension.next() {
                        // If we can't fit this word in, then create a line break and align it in.
                        if word.len() + num_chars >= desired_width {
                            num_chars = 0;
                            print!("\n{:width$}{}", "", separator, width = longest);
                        }

                        num_chars += word.len();
                        print!("{}", word);
                        if extension.peek().is_some() {
                            print!(", ");
                        }
                    }
                    println!();
                }

                return Ok(());
            }

            let mut output_type = get_output_type(options.paging);
            let handle = output_type.handle()?;
            let mut printer = Printer::new(handle, &options);

            for file in files {
                printer.line_changes = file.and_then(|filename| get_git_diff(filename));

                print_file(&options, theme, &assets.syntax_set, &mut printer, file)?;
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

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

mod terminal;

use std::collections::HashMap;
use std::env;
use std::fs::{self, File};
use std::io::{self, BufRead, BufReader, Stdout, StdoutLock, Write};
use std::path::Path;
use std::process::{self, Child, Command, Stdio};

use ansi_term::Colour::{Fixed, Green, Red, White, Yellow};
use ansi_term::Style;
use atty::Stream;
use clap::{App, AppSettings, Arg, SubCommand};
use console::Term;
use directories::ProjectDirs;
use git2::{DiffOptions, IntoCString, Repository};

use syntect::dumps::{dump_to_file, from_binary, from_reader};
use syntect::easy::HighlightLines;
use syntect::highlighting::{Theme, ThemeSet};
use syntect::parsing::SyntaxSet;

use terminal::as_terminal_escaped;

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

enum OptionsStyle {
    Plain,
    LineNumbers,
    Full,
}

struct Options<'a> {
    true_color: bool,
    style: OptionsStyle,
    language: Option<&'a str>,
    interactive_terminal: bool,
    colored_output: bool,
}

enum OutputType<'a> {
    Pager(Child),
    Stdout(StdoutLock<'a>),
}

impl<'a> OutputType<'a> {
    fn new_pager() -> Result<Self> {
        Ok(OutputType::Pager(Command::new("less")
            .args(&["--quit-if-one-screen", "--RAW-CONTROL-CHARS", "--no-init"])
            .stdin(Stdio::piped())
            .spawn()
            .chain_err(|| "Could not spawn pager")?))
    }

    fn new_stdout(stdout: &'a Stdout) -> Self {
        OutputType::Stdout(stdout.lock())
    }

    fn stdout(&mut self) -> Result<&mut Write> {
        Ok(match *self {
            OutputType::Pager(ref mut command) => command
                .stdin
                .as_mut()
                .chain_err(|| "Could not open stdin for pager")?,
            OutputType::Stdout(ref mut handle) => handle,
        })
    }
}

impl<'a> Drop for OutputType<'a> {
    fn drop(&mut self) {
        if let OutputType::Pager(ref mut command) = *self {
            let _ = command.wait();
        }
    }
}

#[derive(Copy, Clone, Debug)]
enum LineChange {
    Added,
    RemovedAbove,
    RemovedBelow,
    Modified,
}

type LineChanges = HashMap<u32, LineChange>;

const PANEL_WIDTH: usize = 7;
const GRID_COLOR: u8 = 238;
const LINE_NUMBER_COLOR: u8 = 244;

#[derive(Default)]
struct Colors {
    grid: Style,
    filename: Style,
    git_added: Style,
    git_removed: Style,
    git_modified: Style,
    line_number: Style,
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

fn print_horizontal_line(
    handle: &mut Write,
    grid_color: &Style,
    grid_char: char,
    term_width: usize,
) -> Result<()> {
    let hline = "─".repeat(term_width - (PANEL_WIDTH + 1));
    let hline = format!("{}{}{}", "─".repeat(PANEL_WIDTH), grid_char, hline);

    writeln!(handle, "{}", grid_color.paint(hline))?;

    Ok(())
}

fn print_file<P: AsRef<Path>>(
    options: &Options,
    theme: &Theme,
    syntax_set: &SyntaxSet,
    filename: P,
    line_changes: &Option<LineChanges>,
) -> Result<()> {
    let reader = BufReader::new(File::open(filename.as_ref())?);
    let syntax = match options.language {
        Some(language) => syntax_set.find_syntax_by_token(language),
        None => syntax_set.find_syntax_for_file(filename.as_ref())?,
    };

    let syntax = syntax.unwrap_or_else(|| syntax_set.find_syntax_plain_text());
    let mut highlighter = HighlightLines::new(syntax, theme);

    let stdout = io::stdout();
    let mut output_type = if options.interactive_terminal {
        match OutputType::new_pager() {
            Ok(pager) => pager,
            Err(_) => OutputType::new_stdout(&stdout),
        }
    } else {
        OutputType::new_stdout(&stdout)
    };
    let handle = output_type.stdout()?;

    let term = Term::stdout();
    let (_, term_width) = term.size();
    let term_width = term_width as usize;

    let colors = if options.colored_output {
        Colors::colored()
    } else {
        Colors::plain()
    };

    // Show file name and bars for all but plain style
    match options.style {
        OptionsStyle::LineNumbers | OptionsStyle::Full => {
            print_horizontal_line(handle, &colors.grid, '┬', term_width)?;

            writeln!(
                handle,
                "{}{} File {}",
                " ".repeat(PANEL_WIDTH),
                colors.grid.paint("│"),
                colors.filename.paint(filename.as_ref().to_string_lossy())
            )?;

            print_horizontal_line(handle, &colors.grid, '┼', term_width)?;
        }
        OptionsStyle::Plain => {}
    };

    for (idx, maybe_line) in reader.lines().enumerate() {
        let line_nr = idx + 1;
        let line = maybe_line.unwrap_or_else(|_| "<INVALID UTF-8>".into());
        let regions = highlighter.highlight(&line);

        let line_change = if let Some(ref changes) = *line_changes {
            match changes.get(&(line_nr as u32)) {
                Some(&LineChange::Added) => colors.git_added.paint("+"),
                Some(&LineChange::RemovedAbove) => colors.git_removed.paint("‾"),
                Some(&LineChange::RemovedBelow) => colors.git_removed.paint("_"),
                Some(&LineChange::Modified) => colors.git_modified.paint("~"),
                _ => Style::default().paint(" "),
            }
        } else {
            Style::default().paint(" ")
        };

        match options.style {
            // Show only content for plain style
            OptionsStyle::Plain => writeln!(
                handle,
                "{}",
                as_terminal_escaped(&regions, options.true_color, options.colored_output)
            )?,
            _ => writeln!(
                handle,
                "{} {} {} {}",
                colors.line_number.paint(format!("{:4}", line_nr)),
                // Show git modification markers only for full style
                match options.style {
                    OptionsStyle::Full => line_change,
                    _ => Style::default().paint(" "),
                },
                colors.grid.paint("│"),
                as_terminal_escaped(&regions, options.true_color, options.colored_output)
            )?,
        }
    }

    // Show bars for all but plain style
    match options.style {
        OptionsStyle::LineNumbers | OptionsStyle::Full => {
            print_horizontal_line(handle, &colors.grid, '┴', term_width)?
        }
        OptionsStyle::Plain => {}
    };

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
        let _ = syntax_set.load_syntaxes(syntax_dir, false);
        syntax_set.load_plain_text_syntax();

        Ok(HighlightingAssets {
            syntax_set,
            theme_set,
        })
    }

    fn save(&self) -> Result<()> {
        let cache_dir = PROJECT_DIRS.cache_dir();
        let theme_set_path = cache_dir.join("theme_set");
        let syntax_set_path = cache_dir.join("syntax_set");

        let _ = fs::create_dir(cache_dir);

        dump_to_file(&self.theme_set, &theme_set_path).map_err(|_| {
            io::Error::new(
                io::ErrorKind::Other,
                format!(
                    "Could not save theme set to {}",
                    theme_set_path.to_string_lossy()
                ),
            )
        })?;
        println!("Wrote theme set to {}", theme_set_path.to_string_lossy());

        dump_to_file(&self.syntax_set, &syntax_set_path).map_err(|_| {
            io::Error::new(
                io::ErrorKind::Other,
                format!(
                    "Could not save syntax set to {}",
                    syntax_set_path.to_string_lossy()
                ),
            )
        })?;
        println!("Wrote syntax set to {}", syntax_set_path.to_string_lossy());

        Ok(())
    }

    fn from_cache() -> Result<Self> {
        let cache_dir = PROJECT_DIRS.cache_dir();
        let theme_set_path = cache_dir.join("theme_set");
        let syntax_set_path = cache_dir.join("syntax_set");

        let syntax_set_file = File::open(&syntax_set_path).chain_err(|| {
            format!(
                "Could not load cached syntax set '{}'",
                syntax_set_path.to_string_lossy()
            )
        })?;
        let mut syntax_set: SyntaxSet = from_reader(syntax_set_file).map_err(|_| {
            io::Error::new(
                io::ErrorKind::Other,
                format!("Could not parse cached syntax set"),
            )
        })?;
        syntax_set.link_syntaxes();

        let theme_set_file = File::open(&theme_set_path).chain_err(|| {
            format!(
                "Could not load cached theme set '{}'",
                theme_set_path.to_string_lossy()
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
        .setting(clap_color_setting)
        .setting(AppSettings::DeriveDisplayOrder)
        .setting(AppSettings::UnifiedHelpMessage)
        .setting(AppSettings::NextLineHelp)
        .setting(AppSettings::DisableVersion)
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
        .subcommand(
            SubCommand::with_name("init-cache")
                .about("Load syntax definitions and themes into cache"),
        )
        .help_message("Print this help message.")
        .version_message("Show version information.")
        .get_matches();

    match app_matches.subcommand() {
        ("init-cache", Some(_)) => {
            let assets = HighlightingAssets::from_files()?;
            assets.save()?;
        }
        _ => {
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
                interactive_terminal,
                colored_output: match app_matches.value_of("color") {
                    Some("always") => true,
                    Some("never") => false,
                    _ => interactive_terminal,
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

            if let Some(files) = app_matches.values_of("FILE") {
                for file in files {
                    let line_changes = get_git_diff(&file.to_string());
                    print_file(&options, theme, &assets.syntax_set, file, &line_changes)?;
                }
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

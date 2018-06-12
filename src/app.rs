use atty::{self, Stream};
use clap::{App as ClapApp, AppSettings, Arg, ArgGroup, ArgMatches, SubCommand};
use console::Term;
use errors::*;
use std::collections::HashSet;
use std::env;
use style::{OutputComponent, OutputComponents, OutputWrap};

#[cfg(windows)]
use ansi_term;

pub struct App {
    pub matches: ArgMatches<'static>,
    interactive_output: bool,
}

impl App {
    pub fn new() -> Self {
        let interactive_output = atty::is(Stream::Stdout);

        #[cfg(windows)]
        let interactive_output = interactive_output && ansi_term::enable_ansi_support().is_ok();

        App {
            matches: Self::matches(interactive_output),
            interactive_output,
        }
    }

    fn matches(interactive_output: bool) -> ArgMatches<'static> {
        let clap_color_setting = if interactive_output {
            AppSettings::ColoredHelp
        } else {
            AppSettings::ColorNever
        };

        ClapApp::new(crate_name!())
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
            .about(
                "A cat(1) clone with wings.\n\n\
                 Use '--help' instead of '-h' to see a more detailed version of the help text.",
            )
            .long_about("A cat(1) clone with syntax highlighting and Git integration.")
            .arg(
                Arg::with_name("language")
                    .short("l")
                    .long("language")
                    .overrides_with("language")
                    .help("Set the language for highlighting")
                    .long_help(
                        "Set the language for syntax highlighting. The language can be \
                         specified as a name (like 'C++' or 'LaTeX') or possible file \
                         extension (like 'cpp', 'hpp' or 'md'). Use '--list-languages' \
                         to show all supported language names and file extensions",
                    )
                    .takes_value(true),
            )
            .arg(
                Arg::with_name("FILE")
                    .help("File(s) to print / concatenate. Use '-' for standard input.")
                    .long_help(
                        "File(s) to print. Use no argument or '-' to read from standard \
                         input",
                    )
                    .multiple(true)
                    .empty_values(false),
            )
            .arg(
                Arg::with_name("style")
                    .long("style")
                    .value_name("style-components")
                    .use_delimiter(true)
                    .takes_value(true)
                    .possible_values(&[
                        "auto", "full", "plain", "changes", "header", "grid", "numbers",
                    ])
                    .default_value("auto")
                    .help("Comma-separated list of style elements to display")
                    .long_help(
                        "Configure which elements (line numbers, file headers, grid \
                         borders, Git modifications, ..) to display in addition to the \
                         file contents. The argument is a comma-separated list of \
                         components to display (e.g. 'numbers,changes,grid') or a \
                         pre-defined style ('full')",
                    ),
            )
            .arg(
                Arg::with_name("color")
                    .long("color")
                    .overrides_with("color")
                    .takes_value(true)
                    .value_name("when")
                    .possible_values(&["auto", "never", "always"])
                    .default_value("auto")
                    .help("When to use colors"),
            )
            .arg(
                Arg::with_name("paging")
                    .long("paging")
                    .overrides_with("paging")
                    .takes_value(true)
                    .value_name("when")
                    .possible_values(&["auto", "never", "always"])
                    .default_value("auto")
                    .help("When to use the pager")
                    .long_help("Specify when to use the pager (less)"),
            )
            .arg(
                Arg::with_name("wrap")
                    .long("wrap")
                    .overrides_with("wrap")
                    .takes_value(true)
                    .value_name("mode")
                    .possible_values(&["character", "never"])
                    .default_value("character")
                    .help("When to wrap text"),
            )
            .arg(
                Arg::with_name("list-languages")
                    .long("list-languages")
                    .help("Displays supported languages")
                    .long_help("Display a list of supported languages"),
            )
            .arg(
                Arg::with_name("theme")
                    .long("theme")
                    .overrides_with("theme")
                    .takes_value(true)
                    .help("Set the theme for highlighting")
                    .long_help(
                        "Set the theme for syntax highlighting. Use '--list-themes' to \
                         see all available themes",
                    ),
            )
            .arg(
                Arg::with_name("line-range")
                    .long("line-range")
                    .overrides_with("line-range")
                    .takes_value(true)
                    .value_name("n:m")
                    .help("Only print the lines from n to m")
                    .long_help(
                        "Print a specified range or ranges of lines from the files. \
                         For example: '--line-range 30:40' will print lines 30 to 40 \n\
                         '--line-range :40' will print lines 1 to 40 \n\
                         '--line-range 40:' will print lines 40 to the end of the file",
                    ),
            )
            .arg(
                Arg::with_name("list-themes")
                    .long("list-themes")
                    .help("Displays supported themes")
                    .help("Display a list of supported themes for syntax highlighting"),
            )
            .arg(
                Arg::with_name("number")
                    .long("number")
                    .overrides_with("number")
                    .short("n")
                    .conflicts_with("style")
                    .help("Show line numbers (alias for '--style=numbers')")
                    .long_help(
                        "Show line numbers (no other decorations). This is an alias for \
                         '--style=numbers'",
                    ),
            )
            .arg(
                Arg::with_name("unbuffered")
                    .short("u")
                    .help("(ignored)")
                    // TODO: use '.hidden_short_help(true)' when the next clap version is released
                    .long_help(
                        "This option exists for POSIX-compliance reasons ('u' is for \
                         'unbuffered'). The output is always unbuffered - this option \
                         it simply ignored.",
                    ),
            )
            .subcommand(
                SubCommand::with_name("cache")
                    .about("Modify the syntax-definition and theme cache")
                    .arg(
                        Arg::with_name("init")
                            .long("init")
                            .short("i")
                            .help("Initialize the syntax/theme cache")
                            .long_help(
                                "Initialize the syntax/theme cache by loading from the \
                                 source directory (default: the configuration directory)",
                            ),
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
                    )
                    .arg(
                        Arg::with_name("source")
                            .long("source")
                            .requires("init")
                            .takes_value(true)
                            .value_name("dir")
                            .help("Use a different source for loading syntaxes and themes from"),
                    )
                    .arg(
                        Arg::with_name("target")
                            .long("target")
                            .requires("init")
                            .takes_value(true)
                            .value_name("dir")
                            .help(
                                "Use a different source to store the cached syntax and theme set",
                            ),
                    ),
            )
            .help_message("Print this help message.")
            .version_message("Show version information.")
            .get_matches()
    }

    pub fn config(&self) -> Result<Config> {
        let files = self.files();

        Ok(Config {
            true_color: is_truecolor_terminal(),
            output_components: self.output_components()?,
            language: self.matches.value_of("language"),
            output_wrap: if !self.interactive_output {
                // We don't have the tty width when piping to another program.
                // There's no point in wrapping when this is the case.
                OutputWrap::None
            } else {
                match self.matches.value_of("wrap") {
                    Some("character") => OutputWrap::Character,
                    Some("never") | _ => OutputWrap::None,
                }
            },
            colored_output: match self.matches.value_of("color") {
                Some("always") => true,
                Some("never") => false,
                Some("auto") | _ => self.interactive_output,
            },
            paging_mode: match self.matches.value_of("paging") {
                Some("always") => PagingMode::Always,
                Some("never") => PagingMode::Never,
                Some("auto") | _ => if files.contains(&None) {
                    // If we are reading from stdin, only enable paging if we write to an
                    // interactive terminal and if we do not *read* from an interactive
                    // terminal.
                    if self.interactive_output && !atty::is(Stream::Stdin) {
                        PagingMode::QuitIfOneScreen
                    } else {
                        PagingMode::Never
                    }
                } else {
                    if self.interactive_output {
                        PagingMode::QuitIfOneScreen
                    } else {
                        PagingMode::Never
                    }
                },
            },
            term_width: Term::stdout().size().1 as usize,
            files,
            theme: self.matches.value_of("theme"),
            line_range: transpose(self.matches.value_of("line-range").map(LineRange::from))?,
        })
    }

    fn files(&self) -> Vec<Option<&str>> {
        self.matches
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
            .unwrap_or_else(|| vec![None]) // read from stdin (None) if no args are given
    }

    fn output_components(&self) -> Result<OutputComponents> {
        let matches = &self.matches;
        Ok(OutputComponents(if matches.is_present("number") {
            [OutputComponent::Numbers].iter().cloned().collect()
        } else {
            values_t!(matches.values_of("style"), OutputComponent)?
                .into_iter()
                .map(|style| style.components(self.interactive_output))
                .fold(HashSet::new(), |mut acc, components| {
                    acc.extend(components.iter().cloned());
                    acc
                })
        }))
    }
}

#[derive(Debug, Clone, Copy)]
pub enum PagingMode {
    Always,
    QuitIfOneScreen,
    Never,
}

pub struct Config<'a> {
    pub true_color: bool,
    pub output_wrap: OutputWrap,
    pub output_components: OutputComponents,
    pub language: Option<&'a str>,
    pub colored_output: bool,
    pub paging_mode: PagingMode,
    pub term_width: usize,
    pub files: Vec<Option<&'a str>>,
    pub theme: Option<&'a str>,
    pub line_range: Option<LineRange>,
}

fn is_truecolor_terminal() -> bool {
    env::var("COLORTERM")
        .map(|colorterm| colorterm == "truecolor" || colorterm == "24bit")
        .unwrap_or(false)
}

pub struct LineRange {
    pub lower: usize,
    pub upper: usize,
}

impl LineRange {
    pub fn from(range_raw: &str) -> Result<LineRange> {
        LineRange::parse_range(range_raw)
    }

    pub fn new() -> LineRange {
        LineRange {
            lower: usize::min_value(),
            upper: usize::max_value(),
        }
    }

    pub fn parse_range(range_raw: &str) -> Result<LineRange> {
        let mut new_range = LineRange::new();

        if range_raw.bytes().nth(0).ok_or("Empty line range")? == b':' {
            new_range.upper = range_raw[1..].parse()?;
            return Ok(new_range);
        } else if range_raw.bytes().last().ok_or("Empty line range")? == b':' {
            new_range.lower = range_raw[..range_raw.len() - 1].parse()?;
            return Ok(new_range);
        }

        let line_numbers: Vec<&str> = range_raw.split(':').collect();
        if line_numbers.len() == 2 {
            new_range.lower = line_numbers[0].parse()?;
            new_range.upper = line_numbers[1].parse()?;
            return Ok(new_range);
        }
        Err("expected single ':' character".into())
    }
}

#[test]
fn test_parse_line_range_full() {
    let range = LineRange::from("40:50").expect("Shouldn't fail on test!");
    assert_eq!(40, range.lower);
    assert_eq!(50, range.upper);
}

#[test]
fn test_parse_line_range_partial_min() {
    let range = LineRange::from(":50").expect("Shouldn't fail on test!");
    assert_eq!(usize::min_value(), range.lower);
    assert_eq!(50, range.upper);
}

#[test]
fn test_parse_line_range_partial_max() {
    let range = LineRange::from("40:").expect("Shouldn't fail on test!");
    assert_eq!(40, range.lower);
    assert_eq!(usize::max_value(), range.upper);
}

#[test]
fn test_parse_line_range_fail() {
    let range = LineRange::from("40:50:80");
    assert!(range.is_err());
    let range = LineRange::from("40::80");
    assert!(range.is_err());
    let range = LineRange::from(":40:");
    assert!(range.is_err());
    let range = LineRange::from("40");
    assert!(range.is_err());
}

fn transpose<T>(opt: Option<Result<T>>) -> Result<Option<T>> {
    opt.map_or(Ok(None), |res| res.map(Some))
}

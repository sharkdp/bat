use std::collections::HashSet;
use std::env;
use std::path::Path;
use std::str::FromStr;

use atty::{self, Stream};

use clap::{App as ClapApp, AppSettings, Arg, ArgGroup, ArgMatches, SubCommand};

use console::Term;

#[cfg(windows)]
use ansi_term;

use assets::BAT_THEME_DEFAULT;
use errors::*;
use line_range::LineRange;
use style::{OutputComponent, OutputComponents, OutputWrap};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum PagingMode {
    Always,
    QuitIfOneScreen,
    Never,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum InputFile<'a> {
    StdIn,
    Ordinary(&'a str),
    ThemePreviewFile,
}

#[derive(Clone)]
pub struct Config<'a> {
    /// List of files to print
    pub files: Vec<InputFile<'a>>,

    /// The explicitly configured language, if any
    pub language: Option<&'a str>,

    /// The character width of the terminal
    pub term_width: usize,

    /// The width of tab characters.
    /// Currently, a value of 0 will cause tabs to be passed through without expanding them.
    pub tab_width: usize,

    /// Whether or not to simply loop through all input (`cat` mode)
    pub loop_through: bool,

    /// Whether or not the output should be colorized
    pub colored_output: bool,

    /// Whether or not the output terminal supports true color
    pub true_color: bool,

    /// Style elements (grid, line numbers, ...)
    pub output_components: OutputComponents,

    /// Text wrapping mode
    pub output_wrap: OutputWrap,

    /// Pager or STDOUT
    pub paging_mode: PagingMode,

    /// The range lines that should be printed, if specified
    pub line_range: Option<LineRange>,

    /// The syntax highlighting theme
    pub theme: String,
}

fn is_truecolor_terminal() -> bool {
    env::var("COLORTERM")
        .map(|colorterm| colorterm == "truecolor" || colorterm == "24bit")
        .unwrap_or(false)
}

/// Helper function that should might appear in Rust stable at some point
/// (https://doc.rust-lang.org/stable/std/option/enum.Option.html#method.transpose)
fn transpose<T>(opt: Option<Result<T>>) -> Result<Option<T>> {
    opt.map_or(Ok(None), |res| res.map(Some))
}

pub struct App {
    pub matches: ArgMatches<'static>,
    interactive_output: bool,
}

impl App {
    pub fn new() -> Self {
        #[cfg(windows)]
        let _ = ansi_term::enable_ansi_support();

        let interactive_output = atty::is(Stream::Stdout);

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

        // Check if the current directory contains a file name cache, if it does
        // do not make the arguements for subcommand 'cache' required.
        let arg_group_required = !Path::new("cache").exists();

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
            ).long_about("A cat(1) clone with syntax highlighting and Git integration.")
            .arg(
                Arg::with_name("FILE")
                    .help("File(s) to print / concatenate. Use '-' for standard input.")
                    .long_help(
                        "File(s) to print / concatenate. Use a dash ('-') or no argument at all \
                         to read from standard input.",
                    ).multiple(true)
                    .empty_values(false),
            )
            .arg(
                Arg::with_name("language")
                    .short("l")
                    .long("language")
                    .overrides_with("language")
                    .help("Set the language for syntax highlighting.")
                    .long_help(
                        "Explicitly set the language for syntax highlighting. The language can be \
                        specified as a name (like 'C++' or 'LaTeX') or possible file extension \
                        (like 'cpp', 'hpp' or 'md'). Use '--list-languages' to show all supported \
                        language names and file extensions."
                    ).takes_value(true),
            ).arg(
            Arg::with_name("list-languages")
                .long("list-languages")
                .conflicts_with("list-themes")
                .help("Display all supported languages.")
                .long_help("Display a list of supported languages for syntax highlighting."),
        ).arg(
            Arg::with_name("theme")
                .long("theme")
                .overrides_with("theme")
                .takes_value(true)
                .help("Set the color theme for syntax highlighting.")
                .long_help(
                    "Set the theme for syntax highlighting. Use '--list-themes' to \
                         see all available themes. To set a default theme, export the \
                         BAT_THEME environment variable (e.g.: export \
                         BAT_THEME=\"TwoDark\").",
                ),
        ).arg(
            Arg::with_name("list-themes")
                .long("list-themes")
                .help("Display all supported highlighting themes.")
                .long_help("Display a list of supported themes for syntax highlighting."),
        ).arg(
            Arg::with_name("style")
                .long("style")
                .value_name("style-components")
                .use_delimiter(true)
                .takes_value(true)
                .possible_values(&[
                    "auto", "full", "plain", "changes", "header", "grid", "numbers",
                ])
                .help("Comma-separated list of style elements to display.")
                .long_help(
                    "Configure which elements (line numbers, file headers, grid \
                         borders, Git modifications, ..) to display in addition to the \
                         file contents. The argument is a comma-separated list of \
                         components to display (e.g. 'numbers,changes,grid') or a \
                         pre-defined style ('full'). To set a default theme, export the \
                         BAT_STYLE environment variable (e.g.: export BAT_STYLE=\"numbers\").",
                ),
        ).arg(
            Arg::with_name("plain")
                .overrides_with("plain")
                .short("p")
                .long("plain")
                .conflicts_with("style")
                .conflicts_with("number")
                .help("Show plain style (alias for '--style=plain').")
                .long_help(
                    "Only show plain style, no decorations. This is an alias for \
                         '--style=plain'",
                ),
        ).arg(
            Arg::with_name("number")
                .long("number")
                .overrides_with("number")
                .short("n")
                .conflicts_with("style")
                .help("Show line numbers (alias for '--style=numbers').")
                .long_help(
                    "Only show line numbers, no other decorations. This is an alias for \
                         '--style=numbers'",
                ),
        ).arg(
            Arg::with_name("line-range")
                .long("line-range")
                .overrides_with("line-range")
                .takes_value(true)
                .value_name("N:M")
                .help("Only print the lines from N to M.")
                .long_help(
                    "Only print the specified range of lines for each file. \
                         For example:\n  \
                         '--line-range 30:40' prints lines 30 to 40\n  \
                         '--line-range :40' prints lines 1 to 40\n  \
                         '--line-range 40:' prints lines 40 to the end of the file",
                ),
        ).arg(
            Arg::with_name("color")
                .long("color")
                .overrides_with("color")
                .takes_value(true)
                .value_name("when")
                .possible_values(&["auto", "never", "always"])
                .default_value("auto")
                .help("When to use colors.")
                .long_help("Specify when to use colored output. The automatic mode \
                                only enables colors if an interactive terminal is detected."),
        ).arg(
            Arg::with_name("decorations")
                .long("decorations")
                .overrides_with("decorations")
                .takes_value(true)
                .value_name("when")
                .possible_values(&["auto", "never", "always"])
                .default_value("auto")
                .help("When to show the decorations specified by '--style'.")
                .long_help("Specify when to use the decorations that have been specified \
                                via '--style'. The automatic mode only enables decorations if \
                                an interactive terminal is detected."),
        ).arg(
            Arg::with_name("paging")
                .long("paging")
                .overrides_with("paging")
                .takes_value(true)
                .value_name("when")
                .possible_values(&["auto", "never", "always"])
                .default_value("auto")
                .help("Specify when to use the pager.")
                .long_help("Specify when to use the pager. To control which pager \
                                is used, set the PAGER or BAT_PAGER environment \
                                variables (the latter takes precedence). The default \
                                pager is 'less'. To disable the pager permanently, set \
                                BAT_PAGER to an empty string."),
        ).arg(
            Arg::with_name("wrap")
                .long("wrap")
                .overrides_with("wrap")
                .takes_value(true)
                .value_name("mode")
                .possible_values(&["auto", "never", "character"])
                .default_value("auto")
                .help("Specify the text-wrapping mode.")
                .long_help("Specify the text-wrapping mode."),
        ).arg(
            Arg::with_name("unbuffered")
                .short("u")
                .hidden_short_help(true)
                .long_help(
                    "This option exists for POSIX-compliance reasons ('u' is for \
                         'unbuffered'). The output is always unbuffered - this option \
                         is simply ignored.",
                ),
        ).arg(
            Arg::with_name("tabs")
                .long("tabs")
                .takes_value(true)
                .value_name("tabs")
                .validator(
                    |t| t.parse::<u32>()
                        .map_err(|_t| "must be a number")
                        .map(|_t| ()) // Convert to Result<(), &str>
                        .map_err(|e| e.to_string()) // Convert to Result<(), String>
                )
                .help("Sets the tab width.")
                .long_help("Sets the tab width. Use a width of 0 to pass tabs through \
                        directly"),
        ).arg(
            Arg::with_name("terminal-width")
                .long("terminal-width")
                .takes_value(true)
                .value_name("width")
                .hidden(true)
                .help("Set the width of the terminal"),
        ).subcommand(
            SubCommand::with_name("cache")
                .about("Modify the syntax-definition and theme cache")
                .arg(
                    Arg::with_name("init")
                        .long("init")
                        .short("i")
                        .help("Initialize the syntax/theme cache.")
                        .long_help(
                            "Initialize the syntax/theme cache by loading from the \
                                 source directory (default: the configuration directory).",
                        ),
                ).arg(
                Arg::with_name("clear")
                    .long("clear")
                    .short("c")
                    .help("Remove the cached syntax definitions and themes."),
            ).arg(
                Arg::with_name("config-dir")
                    .long("config-dir")
                    .short("d")
                    .help("Show bat's configuration directory."),
            ).group(
                ArgGroup::with_name("cache-actions")
                    .args(&["init", "clear", "config-dir"])
                    .required(arg_group_required),
            ).arg(
                Arg::with_name("source")
                    .long("source")
                    .requires("init")
                    .takes_value(true)
                    .value_name("dir")
                    .help("Use a different directory to load syntaxes and themes from."),
            ).arg(
                Arg::with_name("target")
                    .long("target")
                    .requires("init")
                    .takes_value(true)
                    .value_name("dir")
                    .help(
                        "Use a different directory to store the cached syntax and theme set.",
                    ),
            ).arg(
                Arg::with_name("blank")
                    .long("blank")
                    .requires("init")
                    .help("Create completely new syntax and theme sets \
                                   (instead of appending to the default sets).")
            ),
        ).help_message("Print this help message.")
            .version_message("Show version information.")
            .get_matches()
    }

    pub fn config(&self) -> Result<Config> {
        let files = self.files();
        let output_components = self.output_components()?;

        let paging_mode = match self.matches.value_of("paging") {
            Some("always") => PagingMode::Always,
            Some("never") => PagingMode::Never,
            Some("auto") | _ => if files.contains(&InputFile::StdIn) {
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
        };

        Ok(Config {
            true_color: is_truecolor_terminal(),
            language: self.matches.value_of("language"),
            output_wrap: if !self.interactive_output {
                // We don't have the tty width when piping to another program.
                // There's no point in wrapping when this is the case.
                OutputWrap::None
            } else {
                match self.matches.value_of("wrap") {
                    Some("character") => OutputWrap::Character,
                    Some("never") => OutputWrap::None,
                    Some("auto") | _ => if output_components.plain() {
                        OutputWrap::None
                    } else {
                        OutputWrap::Character
                    },
                }
            },
            colored_output: match self.matches.value_of("color") {
                Some("always") => true,
                Some("never") => false,
                Some("auto") | _ => self.interactive_output,
            },
            paging_mode,
            term_width: self
                .matches
                .value_of("terminal-width")
                .and_then(|w| w.parse().ok())
                .unwrap_or(Term::stdout().size().1 as usize),
            loop_through: !(self.interactive_output
                || self.matches.value_of("color") == Some("always")
                || self.matches.value_of("decorations") == Some("always")),
            files,
            tab_width: self
                .matches
                .value_of("tabs")
                .map(String::from)
                .or_else(|| env::var("BAT_TABS").ok())
                .and_then(|t| t.parse().ok())
                .unwrap_or(
                    if output_components.plain() && paging_mode == PagingMode::Never {
                        0
                    } else {
                        8
                    },
                ),
            theme: self
                .matches
                .value_of("theme")
                .map(String::from)
                .or_else(|| env::var("BAT_THEME").ok())
                .unwrap_or(String::from(BAT_THEME_DEFAULT)),
            line_range: transpose(self.matches.value_of("line-range").map(LineRange::from))?,
            output_components,
        })
    }

    fn files(&self) -> Vec<InputFile> {
        self.matches
            .values_of("FILE")
            .map(|values| {
                values
                    .map(|filename| {
                        if filename == "-" {
                            InputFile::StdIn
                        } else {
                            InputFile::Ordinary(filename)
                        }
                    }).collect()
            }).unwrap_or_else(|| vec![InputFile::StdIn])
    }

    fn output_components(&self) -> Result<OutputComponents> {
        let matches = &self.matches;
        Ok(OutputComponents(
            if matches.value_of("decorations") == Some("never") {
                HashSet::new()
            } else if matches.is_present("number") {
                [OutputComponent::Numbers].iter().cloned().collect()
            } else if matches.is_present("plain") {
                [OutputComponent::Plain].iter().cloned().collect()
            } else {
                let env_style_components: Option<Vec<OutputComponent>> =
                    transpose(env::var("BAT_STYLE").ok().map(|style_str| {
                        style_str
                            .split(",")
                            .map(|x| OutputComponent::from_str(&x))
                            .collect::<Result<Vec<OutputComponent>>>()
                    }))?;

                values_t!(matches.values_of("style"), OutputComponent)
                    .ok()
                    .or(env_style_components)
                    .unwrap_or(vec![OutputComponent::Full])
                    .into_iter()
                    .map(|style| style.components(self.interactive_output))
                    .fold(HashSet::new(), |mut acc, components| {
                        acc.extend(components.iter().cloned());
                        acc
                    })
            },
        ))
    }
}

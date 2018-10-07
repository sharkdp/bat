use std::collections::HashSet;
use std::env;
use std::str::FromStr;

use atty::{self, Stream};

use clap::ArgMatches;
use clap_app;
use wild;

use console::Term;

#[cfg(windows)]
use ansi_term;

use assets::BAT_THEME_DEFAULT;
use config::get_args_from_config_file;
use errors::*;
use inputfile::InputFile;
use line_range::LineRange;
use style::{OutputComponent, OutputComponents, OutputWrap};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum PagingMode {
    Always,
    QuitIfOneScreen,
    Never,
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

/// Helper function that might appear in Rust stable at some point
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
        let args = if wild::args_os().nth(1) == Some("cache".into()) {
            // Skip the arguments in bats config file

            wild::args_os().collect::<Vec<_>>()
        } else {
            let mut cli_args = wild::args_os();

            // Read arguments from bats config file
            let mut args = get_args_from_config_file();

            // Put the zero-th CLI argument (program name) first
            args.insert(0, cli_args.next().unwrap());

            // .. and the rest at the end
            cli_args.for_each(|a| args.push(a));

            args
        };

        clap_app::build_app(interactive_output).get_matches_from(args)
    }

    pub fn config(&self) -> Result<Config> {
        let files = self.files();
        let output_components = self.output_components()?;

        let paging_mode = match self.matches.value_of("paging") {
            Some("always") => PagingMode::Always,
            Some("never") => PagingMode::Never,
            Some("auto") | _ => {
                if files.contains(&InputFile::StdIn) {
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
                }
            }
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
                    Some("auto") | _ => {
                        if output_components.plain() {
                            OutputWrap::None
                        } else {
                            OutputWrap::Character
                        }
                    }
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
                    })
                    .collect()
            })
            .unwrap_or_else(|| vec![InputFile::StdIn])
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

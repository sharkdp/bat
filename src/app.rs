use atty::{self, Stream};
use clap::{App as ClapApp, AppSettings, Arg, ArgGroup, ArgMatches, SubCommand};
use console::Term;
use errors::*;
use std::collections::HashSet;
use std::env;
use style::{OutputComponent, OutputComponents, OutputWrap};

pub struct App {
    pub matches: ArgMatches<'static>,
    interactive_output: bool,
}

impl App {
    pub fn new() -> Self {
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
                    .use_delimiter(true)
                    .takes_value(true)
                    .possible_values(&[
                        "auto", "full", "plain", "changes", "header", "grid", "numbers"
                    ])
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
                Arg::with_name("wrap")
                    .long("wrap")
                    .takes_value(true)
                    .possible_values(&["character", "never"])
                    .default_value("character")
                    .help("When to wrap text"),
            )
            .arg(
                Arg::with_name("list-languages")
                    .long("list-languages")
                    .help("Displays supported languages"),
            )
            .arg(
                Arg::with_name("theme")
                    .long("theme")
                    .takes_value(true)
                    .help("Set the theme for highlighting"),
            )
            .arg(
                Arg::with_name("list-themes")
                    .long("list-themes")
                    .help("Displays supported themes"),
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
            .get_matches()
    }

    pub fn config(&self) -> Result<Config> {
        let files = self.files();

        Ok(Config {
            true_color: is_truecolor_terminal(),
            output_components: self.output_components()?,
            language: self.matches.value_of("language"),
            output_wrap: if ! self.interactive_output {
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
            paging: match self.matches.value_of("paging") {
                Some("always") => true,
                Some("never") => false,
                Some("auto") | _ => if files.contains(&None) {
                    // If we are reading from stdin, only enable paging if we write to an
                    // interactive terminal and if we do not *read* from an interactive
                    // terminal.
                    self.interactive_output && !atty::is(Stream::Stdin)
                } else {
                    self.interactive_output
                },
            },
            term_width: Term::stdout().size().1 as usize,
            files,
            theme: self.matches.value_of("theme"),
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
        Ok(OutputComponents(
            values_t!(matches.values_of("style"), OutputComponent)?
                .into_iter()
                .map(|style| style.components(self.interactive_output))
                .fold(HashSet::new(), |mut acc, components| {
                    acc.extend(components.iter().cloned());
                    acc
                }),
        ))
    }
}

pub struct Config<'a> {
    pub true_color: bool,
    pub output_wrap: OutputWrap,
    pub output_components: OutputComponents,
    pub language: Option<&'a str>,
    pub colored_output: bool,
    pub paging: bool,
    pub term_width: usize,
    pub files: Vec<Option<&'a str>>,
    pub theme: Option<&'a str>,
}

fn is_truecolor_terminal() -> bool {
    env::var("COLORTERM")
        .map(|colorterm| colorterm == "truecolor" || colorterm == "24bit")
        .unwrap_or(false)
}

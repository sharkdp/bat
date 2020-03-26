use std::collections::HashSet;
use std::env;
use std::str::FromStr;

use atty::{self, Stream};

use crate::{
    clap_app,
    config::{get_args_from_config_file, get_args_from_env_var},
};
use clap::ArgMatches;
use wild;

use console::Term;

#[cfg(windows)]
use ansi_term;

use bat::{
    config::{
        Config, HighlightedLineRanges, InputFile, LineRange, LineRanges, MappingTarget, OutputWrap,
        PagingMode, StyleComponent, StyleComponents, SyntaxMapping,
    },
    errors::*,
    HighlightingAssets,
};

fn is_truecolor_terminal() -> bool {
    env::var("COLORTERM")
        .map(|colorterm| colorterm == "truecolor" || colorterm == "24bit")
        .unwrap_or(false)
}

pub struct App {
    pub matches: ArgMatches<'static>,
    interactive_output: bool,
}

impl App {
    pub fn new() -> Result<Self> {
        #[cfg(windows)]
        let _ = ansi_term::enable_ansi_support();

        let interactive_output = atty::is(Stream::Stdout);

        Ok(App {
            matches: Self::matches(interactive_output)?,
            interactive_output,
        })
    }

    fn matches(interactive_output: bool) -> Result<ArgMatches<'static>> {
        let args = if wild::args_os().nth(1) == Some("cache".into())
            || wild::args_os().any(|arg| arg == "--no-config")
        {
            // Skip the arguments in bats config file

            wild::args_os().collect::<Vec<_>>()
        } else {
            let mut cli_args = wild::args_os();

            // Read arguments from bats config file
            let mut args = get_args_from_env_var()
                .unwrap_or_else(get_args_from_config_file)
                .chain_err(|| "Could not parse configuration file")?;

            // Put the zero-th CLI argument (program name) first
            args.insert(0, cli_args.next().unwrap());

            // .. and the rest at the end
            cli_args.for_each(|a| args.push(a));

            args
        };

        Ok(clap_app::build_app(interactive_output).get_matches_from(args))
    }

    pub fn config(&self) -> Result<Config> {
        let files = self.files();
        let style_components = self.style_components()?;

        let paging_mode = match self.matches.value_of("paging") {
            Some("always") => PagingMode::Always,
            Some("never") => PagingMode::Never,
            Some("auto") | _ => {
                if self.matches.occurrences_of("plain") > 1 {
                    // If we have -pp as an option when in auto mode, the pager should be disabled.
                    PagingMode::Never
                } else if files.contains(&InputFile::StdIn) {
                    // If we are reading from stdin, only enable paging if we write to an
                    // interactive terminal and if we do not *read* from an interactive
                    // terminal.
                    if self.interactive_output && !atty::is(Stream::Stdin) {
                        PagingMode::QuitIfOneScreen
                    } else {
                        PagingMode::Never
                    }
                } else if self.interactive_output {
                    PagingMode::QuitIfOneScreen
                } else {
                    PagingMode::Never
                }
            }
        };

        let mut syntax_mapping = SyntaxMapping::builtin();

        if let Some(values) = self.matches.values_of("map-syntax") {
            for from_to in values {
                let parts: Vec<_> = from_to.split(':').collect();

                if parts.len() != 2 {
                    return Err("Invalid syntax mapping. The format of the -m/--map-syntax option is '<glob-pattern>:<syntax-name>'. For example: '*.cpp:C++'.".into());
                }

                syntax_mapping.insert(parts[0], MappingTarget::MapTo(parts[1]))?;
            }
        }

        let maybe_term_width = self.matches.value_of("terminal-width").and_then(|w| {
            if w.starts_with('+') || w.starts_with('-') {
                // Treat argument as a delta to the current terminal width
                w.parse().ok().map(|delta: i16| {
                    let old_width: u16 = Term::stdout().size().1;
                    let new_width: i32 = i32::from(old_width) + i32::from(delta);

                    if new_width <= 0 {
                        old_width as usize
                    } else {
                        new_width as usize
                    }
                })
            } else {
                w.parse().ok()
            }
        });

        match self.matches.values_of("file-name") {
            Some(ref filenames) if filenames.len() != files.len() => {
                return Err(format!("{} {}", filenames.len(), files.len()).into());
            }
            _ => {}
        }

        Ok(Config {
            true_color: is_truecolor_terminal(),
            language: self.matches.value_of("language").or_else(|| {
                if self.matches.is_present("show-all") {
                    Some("show-nonprintable")
                } else {
                    None
                }
            }),
            show_nonprintable: self.matches.is_present("show-all"),
            output_wrap: if self.interactive_output || maybe_term_width.is_some() {
                match self.matches.value_of("wrap") {
                    Some("character") => OutputWrap::Character,
                    Some("never") => OutputWrap::None,
                    Some("auto") | _ => {
                        if style_components.plain() {
                            OutputWrap::None
                        } else {
                            OutputWrap::Character
                        }
                    }
                }
            } else {
                // We don't have the tty width when piping to another program.
                // There's no point in wrapping when this is the case.
                OutputWrap::None
            },
            colored_output: match self.matches.value_of("color") {
                Some("always") => true,
                Some("never") => false,
                Some("auto") | _ => self.interactive_output,
            },
            paging_mode,
            term_width: maybe_term_width.unwrap_or(Term::stdout().size().1 as usize),
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
                    if style_components.plain() && paging_mode == PagingMode::Never {
                        0
                    } else {
                        4
                    },
                ),
            theme: self
                .matches
                .value_of("theme")
                .map(String::from)
                .or_else(|| env::var("BAT_THEME").ok())
                .map(|s| {
                    if s == "default" {
                        String::from(HighlightingAssets::default_theme())
                    } else {
                        s
                    }
                })
                .unwrap_or_else(|| String::from(HighlightingAssets::default_theme())),
            line_ranges: self
                .matches
                .values_of("line-range")
                .map(|vs| vs.map(LineRange::from).collect())
                .transpose()?
                .map(LineRanges::from)
                .unwrap_or_default(),
            style_components,
            syntax_mapping,
            pager: self.matches.value_of("pager"),
            use_italic_text: match self.matches.value_of("italic-text") {
                Some("always") => true,
                _ => false,
            },
            highlighted_lines: self
                .matches
                .values_of("highlight-line")
                .map(|ws| ws.map(LineRange::from).collect())
                .transpose()?
                .map(LineRanges::from)
                .map(|lr| HighlightedLineRanges(lr))
                .unwrap_or_default(),
            filenames: self
                .matches
                .values_of("file-name")
                .map(|values| values.collect()),
        })
    }

    fn files(&self) -> Vec<InputFile> {
        self.matches
            .values_of_os("FILE")
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

    fn style_components(&self) -> Result<StyleComponents> {
        let matches = &self.matches;
        Ok(StyleComponents(
            if matches.value_of("decorations") == Some("never") {
                HashSet::new()
            } else if matches.is_present("number") {
                [StyleComponent::Numbers].iter().cloned().collect()
            } else if matches.is_present("plain") {
                [StyleComponent::Plain].iter().cloned().collect()
            } else {
                let env_style_components: Option<Vec<StyleComponent>> = env::var("BAT_STYLE")
                    .ok()
                    .map(|style_str| {
                        style_str
                            .split(',')
                            .map(|x| StyleComponent::from_str(&x))
                            .collect::<Result<Vec<StyleComponent>>>()
                    })
                    .transpose()?;

                matches
                    .value_of("style")
                    .map(|styles| {
                        styles
                            .split(',')
                            .map(|style| style.parse::<StyleComponent>())
                            .filter_map(|style| style.ok())
                            .collect::<Vec<_>>()
                    })
                    .or(env_style_components)
                    .unwrap_or_else(|| vec![StyleComponent::Full])
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

use std::collections::HashSet;
use std::env;
use std::ffi::OsStr;
use std::str::FromStr;

use atty::{self, Stream};

use crate::{
    clap_app,
    config::{get_args_from_config_file, get_args_from_env_var},
};
use clap::ArgMatches;

use console::Term;

use crate::input::{new_file_input, new_stdin_input};
use bat::{
    assets::HighlightingAssets,
    config::{Config, VisibleLines},
    error::*,
    input::Input,
    line_range::{HighlightedLineRanges, LineRange, LineRanges},
    style::{StyleComponent, StyleComponents},
    MappingTarget, PagingMode, SyntaxMapping, WrappingMode,
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

    pub fn config(&self, inputs: &[Input]) -> Result<Config> {
        let style_components = self.style_components()?;

        let paging_mode = match self.matches.value_of("paging") {
            Some("always") => PagingMode::Always,
            Some("never") => PagingMode::Never,
            Some("auto") | None => {
                if self.matches.occurrences_of("plain") > 1 {
                    // If we have -pp as an option when in auto mode, the pager should be disabled.
                    PagingMode::Never
                } else if self.matches.is_present("no-paging") {
                    PagingMode::Never
                } else if inputs.iter().any(Input::is_stdin) {
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
            _ => unreachable!("other values for --paging are not allowed"),
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
            wrapping_mode: if self.interactive_output || maybe_term_width.is_some() {
                match self.matches.value_of("wrap") {
                    Some("character") => WrappingMode::Character,
                    Some("never") => WrappingMode::NoWrapping,
                    Some("auto") | None => {
                        if style_components.plain() {
                            WrappingMode::NoWrapping
                        } else {
                            WrappingMode::Character
                        }
                    }
                    _ => unreachable!("other values for --paging are not allowed"),
                }
            } else {
                // We don't have the tty width when piping to another program.
                // There's no point in wrapping when this is the case.
                WrappingMode::NoWrapping
            },
            colored_output: self.matches.is_present("force-colorization")
                || match self.matches.value_of("color") {
                    Some("always") => true,
                    Some("never") => false,
                    Some("auto") | _ => {
                        env::var_os("NO_COLOR").is_none() && self.interactive_output
                    }
                },
            paging_mode,
            term_width: maybe_term_width.unwrap_or(Term::stdout().size().1 as usize),
            loop_through: !(self.interactive_output
                || self.matches.value_of("color") == Some("always")
                || self.matches.value_of("decorations") == Some("always")
                || self.matches.is_present("force-colorization")),
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
            visible_lines: match self.matches.is_present("diff") {
                #[cfg(feature = "git")]
                true => VisibleLines::DiffContext(
                    self.matches
                        .value_of("diff-context")
                        .and_then(|t| t.parse().ok())
                        .unwrap_or(2),
                ),

                _ => VisibleLines::Ranges(
                    self.matches
                        .values_of("line-range")
                        .map(|vs| vs.map(LineRange::from).collect())
                        .transpose()?
                        .map(LineRanges::from)
                        .unwrap_or_default(),
                ),
            },
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
        })
    }

    pub fn inputs(&self) -> Result<Vec<Input>> {
        // verify equal length of file-names and input FILEs
        match self.matches.values_of("file-name") {
            Some(ref filenames)
                if self.matches.values_of_os("FILE").is_some()
                    && filenames.len() != self.matches.values_of_os("FILE").unwrap().len() =>
            {
                return Err("Must be one file name per input type.".into());
            }
            _ => {}
        }
        let filenames: Option<Vec<&str>> = self
            .matches
            .values_of("file-name")
            .map(|values| values.collect());

        let mut filenames_or_none: Box<dyn Iterator<Item = _>> = match filenames {
            Some(ref filenames) => Box::new(filenames.iter().map(|name| Some(OsStr::new(*name)))),
            None => Box::new(std::iter::repeat(None)),
        };
        let files: Option<Vec<&OsStr>> = self.matches.values_of_os("FILE").map(|vs| vs.collect());

        if files.is_none() {
            return Ok(vec![new_stdin_input(
                filenames_or_none.next().unwrap_or(None),
            )]);
        }
        let files_or_none: Box<dyn Iterator<Item = _>> = match files {
            Some(ref files) => Box::new(files.iter().map(|name| Some(*name))),
            None => Box::new(std::iter::repeat(None)),
        };

        let mut file_input = Vec::new();
        for (filepath, provided_name) in files_or_none.zip(filenames_or_none) {
            if let Some(filepath) = filepath {
                if filepath.to_str().unwrap_or_default() == "-" {
                    file_input.push(new_stdin_input(provided_name));
                } else {
                    file_input.push(new_file_input(filepath, provided_name));
                }
            }
        }
        Ok(file_input)
    }

    fn style_components(&self) -> Result<StyleComponents> {
        let matches = &self.matches;
        Ok(StyleComponents(
            if matches.value_of("decorations") == Some("never") {
                HashSet::new()
            } else if matches.is_present("number") {
                [StyleComponent::LineNumbers].iter().cloned().collect()
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

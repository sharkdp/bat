use std::collections::HashSet;
use std::env;
use std::path::{Path, PathBuf};

use is_terminal::IsTerminal;

use crate::{
    clap_app,
    config::{get_args_from_config_file, get_args_from_env_opts_var, get_args_from_env_vars},
};
use clap::ArgMatches;

use console::Term;

use crate::input::{new_file_input, new_stdin_input};
use bat::{
    assets::HighlightingAssets,
    bat_warning,
    config::{Config, VisibleLines},
    error::*,
    input::Input,
    line_range::{HighlightedLineRanges, LineRange, LineRanges},
    style::{StyleComponent, StyleComponents},
    MappingTarget, NonprintableNotation, PagingMode, SyntaxMapping, WrappingMode,
};

fn is_truecolor_terminal() -> bool {
    env::var("COLORTERM")
        .map(|colorterm| colorterm == "truecolor" || colorterm == "24bit")
        .unwrap_or(false)
}

pub struct App {
    pub matches: ArgMatches,
    interactive_output: bool,
}

impl App {
    pub fn new() -> Result<Self> {
        #[cfg(windows)]
        let _ = nu_ansi_term::enable_ansi_support();

        let interactive_output = std::io::stdout().is_terminal();

        Ok(App {
            matches: Self::matches(interactive_output)?,
            interactive_output,
        })
    }

    fn matches(interactive_output: bool) -> Result<ArgMatches> {
        let args = if wild::args_os().nth(1) == Some("cache".into()) {
            // Skip the config file and env vars

            wild::args_os().collect::<Vec<_>>()
        } else if wild::args_os().any(|arg| arg == "--no-config") {
            // Skip the arguments in bats config file

            let mut cli_args = wild::args_os();
            let mut args = get_args_from_env_vars();

            // Put the zero-th CLI argument (program name) first
            args.insert(0, cli_args.next().unwrap());

            // .. and the rest at the end
            cli_args.for_each(|a| args.push(a));

            args
        } else {
            let mut cli_args = wild::args_os();

            // Read arguments from bats config file
            let mut args = get_args_from_env_opts_var()
                .unwrap_or_else(get_args_from_config_file)
                .map_err(|_| "Could not parse configuration file")?;

            // Selected env vars supersede config vars
            args.extend(get_args_from_env_vars());

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

        let paging_mode = match self.matches.get_one::<String>("paging").map(|s| s.as_str()) {
            Some("always") => PagingMode::Always,
            Some("never") => PagingMode::Never,
            Some("auto") | None => {
                // If we have -pp as an option when in auto mode, the pager should be disabled.
                let extra_plain = self.matches.get_count("plain") > 1;
                if extra_plain || self.matches.get_flag("no-paging") {
                    PagingMode::Never
                } else if inputs.iter().any(Input::is_stdin) {
                    // If we are reading from stdin, only enable paging if we write to an
                    // interactive terminal and if we do not *read* from an interactive
                    // terminal.
                    if self.interactive_output && !std::io::stdin().is_terminal() {
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

        if let Some(values) = self.matches.get_many::<String>("ignored-suffix") {
            for suffix in values {
                syntax_mapping.insert_ignored_suffix(suffix);
            }
        }

        if let Some(values) = self.matches.get_many::<String>("map-syntax") {
            for from_to in values {
                let parts: Vec<_> = from_to.split(':').collect();

                if parts.len() != 2 {
                    return Err("Invalid syntax mapping. The format of the -m/--map-syntax option is '<glob-pattern>:<syntax-name>'. For example: '*.cpp:C++'.".into());
                }

                syntax_mapping.insert(parts[0], MappingTarget::MapTo(parts[1]))?;
            }
        }

        let maybe_term_width = self
            .matches
            .get_one::<String>("terminal-width")
            .and_then(|w| {
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
            language: self
                .matches
                .get_one::<String>("language")
                .map(|s| s.as_str())
                .or_else(|| {
                    if self.matches.get_flag("show-all") {
                        Some("show-nonprintable")
                    } else {
                        None
                    }
                }),
            show_nonprintable: self.matches.get_flag("show-all"),
            nonprintable_notation: match self
                .matches
                .get_one::<String>("nonprintable-notation")
                .map(|s| s.as_str())
            {
                Some("unicode") => NonprintableNotation::Unicode,
                Some("caret") => NonprintableNotation::Caret,
                _ => unreachable!("other values for --nonprintable-notation are not allowed"),
            },
            wrapping_mode: if self.interactive_output || maybe_term_width.is_some() {
                if !self.matches.get_flag("chop-long-lines") {
                    match self.matches.get_one::<String>("wrap").map(|s| s.as_str()) {
                        Some("character") => WrappingMode::Character,
                        Some("never") => WrappingMode::NoWrapping(true),
                        Some("auto") | None => {
                            if style_components.plain() {
                                WrappingMode::NoWrapping(false)
                            } else {
                                WrappingMode::Character
                            }
                        }
                        _ => unreachable!("other values for --wrap are not allowed"),
                    }
                } else {
                    WrappingMode::NoWrapping(true)
                }
            } else {
                // We don't have the tty width when piping to another program.
                // There's no point in wrapping when this is the case.
                WrappingMode::NoWrapping(false)
            },
            colored_output: self.matches.get_flag("force-colorization")
                || match self.matches.get_one::<String>("color").map(|s| s.as_str()) {
                    Some("always") => true,
                    Some("never") => false,
                    Some("auto") => env::var_os("NO_COLOR").is_none() && self.interactive_output,
                    _ => unreachable!("other values for --color are not allowed"),
                },
            paging_mode,
            term_width: maybe_term_width.unwrap_or(Term::stdout().size().1 as usize),
            loop_through: !(self.interactive_output
                || self.matches.get_one::<String>("color").map(|s| s.as_str()) == Some("always")
                || self
                    .matches
                    .get_one::<String>("decorations")
                    .map(|s| s.as_str())
                    == Some("always")
                || self.matches.get_flag("force-colorization")),
            tab_width: self
                .matches
                .get_one::<String>("tabs")
                .map(String::from)
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
                .get_one::<String>("theme")
                .map(String::from)
                .map(|s| {
                    if s == "default" {
                        String::from(HighlightingAssets::default_theme())
                    } else {
                        s
                    }
                })
                .unwrap_or_else(|| String::from(HighlightingAssets::default_theme())),
            visible_lines: match self.matches.try_contains_id("diff").unwrap_or_default()
                && self.matches.get_flag("diff")
            {
                #[cfg(feature = "git")]
                true => VisibleLines::DiffContext(
                    self.matches
                        .get_one::<String>("diff-context")
                        .and_then(|t| t.parse().ok())
                        .unwrap_or(2),
                ),

                _ => VisibleLines::Ranges(
                    self.matches
                        .get_many::<String>("line-range")
                        .map(|vs| vs.map(|s| LineRange::from(s.as_str())).collect())
                        .transpose()?
                        .map(LineRanges::from)
                        .unwrap_or_default(),
                ),
            },
            style_components,
            syntax_mapping,
            pager: self.matches.get_one::<String>("pager").map(|s| s.as_str()),
            use_italic_text: self
                .matches
                .get_one::<String>("italic-text")
                .map(|s| s.as_str())
                == Some("always"),
            highlighted_lines: self
                .matches
                .get_many::<String>("highlight-line")
                .map(|ws| ws.map(|s| LineRange::from(s.as_str())).collect())
                .transpose()?
                .map(LineRanges::from)
                .map(HighlightedLineRanges)
                .unwrap_or_default(),
            use_custom_assets: !self.matches.get_flag("no-custom-assets"),
        })
    }

    pub fn inputs(&self) -> Result<Vec<Input>> {
        let filenames: Option<Vec<&Path>> = self
            .matches
            .get_many::<PathBuf>("file-name")
            .map(|vs| vs.map(|p| p.as_path()).collect::<Vec<_>>());

        let files: Option<Vec<&Path>> = self
            .matches
            .get_many::<PathBuf>("FILE")
            .map(|vs| vs.map(|p| p.as_path()).collect::<Vec<_>>());

        // verify equal length of file-names and input FILEs
        if filenames.is_some()
            && files.is_some()
            && filenames.as_ref().map(|v| v.len()) != files.as_ref().map(|v| v.len())
        {
            return Err("Must be one file name per input type.".into());
        }

        let mut filenames_or_none: Box<dyn Iterator<Item = Option<&Path>>> = match filenames {
            Some(filenames) => Box::new(filenames.into_iter().map(Some)),
            None => Box::new(std::iter::repeat(None)),
        };
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
        let mut styled_components = StyleComponents(
            if matches.get_one::<String>("decorations").map(|s| s.as_str()) == Some("never") {
                HashSet::new()
            } else if matches.get_flag("number") {
                [StyleComponent::LineNumbers].iter().cloned().collect()
            } else if 0 < matches.get_count("plain") {
                [StyleComponent::Plain].iter().cloned().collect()
            } else {
                matches
                    .get_one::<String>("style")
                    .map(|styles| {
                        styles
                            .split(',')
                            .map(|style| style.parse::<StyleComponent>())
                            .filter_map(|style| style.ok())
                            .collect::<Vec<_>>()
                    })
                    .unwrap_or_else(|| vec![StyleComponent::Default])
                    .into_iter()
                    .map(|style| style.components(self.interactive_output))
                    .fold(HashSet::new(), |mut acc, components| {
                        acc.extend(components.iter().cloned());
                        acc
                    })
            },
        );

        // If `grid` is set, remove `rule` as it is a subset of `grid`, and print a warning.
        if styled_components.grid() && styled_components.0.remove(&StyleComponent::Rule) {
            bat_warning!("Style 'rule' is a subset of style 'grid', 'rule' will not be visible.");
        }

        Ok(styled_components)
    }
}

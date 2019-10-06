// `error_chain!` can recurse deeply
#![recursion_limit = "1024"]

#[macro_use]
extern crate clap;

mod app;
mod clap_app;
mod config;

use std::collections::HashSet;
use std::io;
use std::io::Write;
use std::path::Path;
use std::process;

use ansi_term::Colour::Green;
use ansi_term::Style;

use crate::{app::App, config::config_file};
use bat::controller::Controller;

use bat::{
    assets::{cache_dir, clear_assets, config_dir, HighlightingAssets},
    errors::*,
    inputfile::InputFile,
    style::{OutputComponent, OutputComponents},
    Config,
};

fn run_cache_subcommand(matches: &clap::ArgMatches) -> Result<()> {
    if matches.is_present("build") {
        let source_dir = matches.value_of("source").map(Path::new);
        let target_dir = matches.value_of("target").map(Path::new);

        let blank = matches.is_present("blank");

        let assets = HighlightingAssets::from_files(source_dir, blank)?;
        assets.save(target_dir)?;
    } else if matches.is_present("clear") {
        clear_assets();
    }

    Ok(())
}

pub fn list_languages(config: &Config) -> Result<()> {
    let assets = HighlightingAssets::new();
    let mut languages = assets
        .syntax_set
        .syntaxes()
        .iter()
        .filter(|syntax| !syntax.hidden && !syntax.file_extensions.is_empty())
        .collect::<Vec<_>>();
    languages.sort_by_key(|lang| lang.name.to_uppercase());

    let stdout = io::stdout();
    let mut stdout = stdout.lock();

    if config.loop_through {
        for lang in languages {
            write!(stdout, "{}:{}\n", lang.name, lang.file_extensions.join(","))?;
        }
    } else {
        let longest = languages
            .iter()
            .map(|syntax| syntax.name.len())
            .max()
            .unwrap_or(32); // Fallback width if they have no language definitions.

        let comma_separator = ", ";
        let separator = " ";
        // Line-wrapping for the possible file extension overflow.
        let desired_width = config.term_width - longest - separator.len();

        let style = if config.colored_output {
            Green.normal()
        } else {
            Style::default()
        };

        for lang in languages {
            write!(stdout, "{:width$}{}", lang.name, separator, width = longest)?;

            // Number of characters on this line so far, wrap before `desired_width`
            let mut num_chars = 0;

            let mut extension = lang.file_extensions.iter().peekable();
            while let Some(word) = extension.next() {
                // If we can't fit this word in, then create a line break and align it in.
                let new_chars = word.len() + comma_separator.len();
                if num_chars + new_chars >= desired_width {
                    num_chars = 0;
                    write!(stdout, "\n{:width$}{}", "", separator, width = longest)?;
                }

                num_chars += new_chars;
                write!(stdout, "{}", style.paint(&word[..]))?;
                if extension.peek().is_some() {
                    write!(stdout, "{}", comma_separator)?;
                }
            }
            writeln!(stdout)?;
        }
    }

    Ok(())
}

pub fn list_themes(cfg: &Config) -> Result<()> {
    let assets = HighlightingAssets::new();
    let themes = &assets.theme_set.themes;
    let mut config = cfg.clone();
    let mut style = HashSet::new();
    style.insert(OutputComponent::Plain);
    config.files = vec![InputFile::ThemePreviewFile];
    config.output_components = OutputComponents(style);

    let stdout = io::stdout();
    let mut stdout = stdout.lock();

    if config.colored_output {
        for (theme, _) in themes.iter() {
            writeln!(
                stdout,
                "Theme: {}\n",
                Style::new().bold().paint(theme.to_string())
            )?;
            config.theme = theme.to_string();
            let _controller = Controller::new(&config, &assets).run();
            writeln!(stdout)?;
        }
    } else {
        for (theme, _) in themes.iter() {
            writeln!(stdout, "{}", theme)?;
        }
    }

    Ok(())
}

fn run_controller(config: &Config) -> Result<bool> {
    let assets = HighlightingAssets::new();
    let controller = Controller::new(&config, &assets);
    controller.run()
}

/// Returns `Err(..)` upon fatal errors. Otherwise, returns `Ok(true)` on full success and
/// `Ok(false)` if any intermediate errors occurred (were printed).
fn run() -> Result<bool> {
    let app = App::new()?;

    match app.matches.subcommand() {
        ("cache", Some(cache_matches)) => {
            // If there is a file named 'cache' in the current working directory,
            // arguments for subcommand 'cache' are not mandatory.
            // If there are non-zero arguments, execute the subcommand cache, else, open the file cache.
            if !cache_matches.args.is_empty() {
                run_cache_subcommand(cache_matches)?;
                Ok(true)
            } else {
                let mut config = app.config()?;
                config.files = vec![InputFile::Ordinary(&"cache")];

                run_controller(&config)
            }
        }
        _ => {
            let config = app.config()?;

            if app.matches.is_present("list-languages") {
                list_languages(&config)?;

                Ok(true)
            } else if app.matches.is_present("list-themes") {
                list_themes(&config)?;

                Ok(true)
            } else if app.matches.is_present("config-file") {
                println!("{}", config_file().to_string_lossy());

                Ok(true)
            } else if app.matches.is_present("config-dir") {
                writeln!(io::stdout(), "{}", config_dir())?;
                Ok(true)
            } else if app.matches.is_present("cache-dir") {
                writeln!(io::stdout(), "{}", cache_dir())?;
                Ok(true)
            } else {
                run_controller(&config)
            }
        }
    }
}

fn main() {
    let result = run();

    match result {
        Err(error) => {
            handle_error(&error);
            process::exit(1);
        }
        Ok(false) => {
            process::exit(1);
        }
        Ok(true) => {
            process::exit(0);
        }
    }
}

// `error_chain!` can recurse deeply
#![recursion_limit = "1024"]

mod app;
mod assets;
mod clap_app;
mod config;
mod directories;
mod input;

use std::collections::{HashMap, HashSet};
use std::ffi::OsStr;
use std::io;
use std::io::{BufReader, Write};
use std::path::Path;
use std::process;

use ansi_term::Colour::Green;
use ansi_term::Style;

use crate::{
    app::App,
    config::{config_file, generate_config_file},
};

use assets::{assets_from_cache_or_binary, cache_dir, clear_assets, config_dir};
use clap::crate_version;
use directories::PROJECT_DIRS;
use globset::GlobMatcher;

use bat::{
    assets::HighlightingAssets,
    config::Config,
    controller::Controller,
    error::*,
    input::Input,
    style::{StyleComponent, StyleComponents},
    MappingTarget,
};

const THEME_PREVIEW_DATA: &[u8] = include_bytes!("../../../assets/theme_preview.rs");

fn run_cache_subcommand(matches: &clap::ArgMatches) -> Result<()> {
    if matches.is_present("build") {
        let source_dir = matches
            .value_of("source")
            .map(Path::new)
            .unwrap_or_else(|| PROJECT_DIRS.config_dir());
        let target_dir = matches
            .value_of("target")
            .map(Path::new)
            .unwrap_or_else(|| PROJECT_DIRS.cache_dir());

        let blank = matches.is_present("blank");

        let assets = HighlightingAssets::from_files(source_dir, !blank)?;
        assets.save_to_cache(target_dir, crate_version!())?;
    } else if matches.is_present("clear") {
        clear_assets();
    }

    Ok(())
}

fn get_syntax_mapping_to_paths<'a>(
    mappings: &[(GlobMatcher, MappingTarget<'a>)],
) -> HashMap<&'a str, Vec<String>> {
    let mut map = HashMap::new();
    for mapping in mappings {
        match mapping {
            (_, MappingTarget::MapToUnknown) => {}
            (matcher, MappingTarget::MapTo(s)) => {
                let globs = map.entry(*s).or_insert_with(Vec::new);
                globs.push(matcher.glob().glob().into());
            }
        }
    }
    map
}

pub fn list_languages(config: &Config) -> Result<()> {
    let assets = assets_from_cache_or_binary()?;
    let mut languages = assets
        .syntaxes()
        .iter()
        .filter(|syntax| !syntax.hidden && !syntax.file_extensions.is_empty())
        .cloned()
        .collect::<Vec<_>>();

    // Handling of file-extension conflicts, see issue #1076
    for lang in languages.iter_mut() {
        let lang_name = lang.name.clone();
        lang.file_extensions.retain(|extension| {
            // The 'extension' variable is not certainly a real extension.
            //
            // Skip if 'extension' starts with '.', likely a hidden file like '.vimrc'
            // Also skip if the 'extension' contains another real extension, likely
            // that is a full match file name like 'CMakeLists.txt' and 'Cargo.lock'
            if extension.starts_with('.') || Path::new(extension).extension().is_some() {
                true
            } else {
                let test_file = Path::new("test").with_extension(extension);
                match assets.syntax_for_file_name(test_file, &config.syntax_mapping) {
                    Some(syntax) => syntax.name == lang_name,
                    None => false,
                }
            }
        });
    }

    languages.sort_by_key(|lang| lang.name.to_uppercase());

    let configured_languages = get_syntax_mapping_to_paths(config.syntax_mapping.mappings());

    for lang in languages.iter_mut() {
        if let Some(additional_paths) = configured_languages.get(lang.name.as_str()) {
            lang.file_extensions
                .extend(additional_paths.iter().cloned());
        }
    }

    let stdout = io::stdout();
    let mut stdout = stdout.lock();

    if config.loop_through {
        for lang in languages {
            writeln!(stdout, "{}:{}", lang.name, lang.file_extensions.join(","))?;
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

fn theme_preview_file<'a>() -> Input<'a> {
    Input::from_reader(Box::new(BufReader::new(THEME_PREVIEW_DATA)))
}

pub fn list_themes(cfg: &Config) -> Result<()> {
    let assets = assets_from_cache_or_binary()?;
    let mut config = cfg.clone();
    let mut style = HashSet::new();
    style.insert(StyleComponent::Plain);
    config.language = Some("Rust");
    config.style_components = StyleComponents(style);

    let stdout = io::stdout();
    let mut stdout = stdout.lock();

    if config.colored_output {
        for theme in assets.themes() {
            writeln!(
                stdout,
                "Theme: {}\n",
                Style::new().bold().paint(theme.to_string())
            )?;
            config.theme = theme.to_string();
            Controller::new(&config, &assets)
                .run(vec![theme_preview_file()])
                .ok();
            writeln!(stdout)?;
        }
    } else {
        for theme in assets.themes() {
            writeln!(stdout, "{}", theme)?;
        }
    }

    Ok(())
}

fn run_controller(inputs: Vec<Input>, config: &Config) -> Result<bool> {
    let assets = assets_from_cache_or_binary()?;
    let controller = Controller::new(&config, &assets);
    controller.run(inputs)
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
                let inputs = vec![Input::ordinary_file(OsStr::new("cache"))];
                let config = app.config(&inputs)?;

                run_controller(inputs, &config)
            }
        }
        _ => {
            let inputs = app.inputs()?;
            let config = app.config(&inputs)?;

            if app.matches.is_present("list-languages") {
                list_languages(&config)?;
                Ok(true)
            } else if app.matches.is_present("list-themes") {
                list_themes(&config)?;
                Ok(true)
            } else if app.matches.is_present("config-file") {
                println!("{}", config_file().to_string_lossy());
                Ok(true)
            } else if app.matches.is_present("generate-config-file") {
                generate_config_file()?;
                Ok(true)
            } else if app.matches.is_present("config-dir") {
                writeln!(io::stdout(), "{}", config_dir())?;
                Ok(true)
            } else if app.matches.is_present("cache-dir") {
                writeln!(io::stdout(), "{}", cache_dir())?;
                Ok(true)
            } else {
                run_controller(inputs, &config)
            }
        }
    }
}

fn main() {
    let result = run();

    match result {
        Err(error) => {
            let stderr = std::io::stderr();
            default_error_handler(&error, &mut stderr.lock());
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

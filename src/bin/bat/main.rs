#![deny(unsafe_code)]

mod app;
mod assets;
mod clap_app;
mod config;
mod directories;
mod input;

use std::collections::{HashMap, HashSet};
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
use directories::PROJECT_DIRS;
use globset::GlobMatcher;

use bat::{
    config::Config,
    controller::Controller,
    error::*,
    input::Input,
    style::{StyleComponent, StyleComponents},
    MappingTarget, PagingMode,
};

const THEME_PREVIEW_DATA: &[u8] = include_bytes!("../../../assets/theme_preview.rs");

#[cfg(feature = "build-assets")]
fn build_assets(matches: &clap::ArgMatches) -> Result<()> {
    let source_dir = matches
        .value_of("source")
        .map(Path::new)
        .unwrap_or_else(|| PROJECT_DIRS.config_dir());
    let target_dir = matches
        .value_of("target")
        .map(Path::new)
        .unwrap_or_else(|| PROJECT_DIRS.cache_dir());

    let blank = matches.is_present("blank");

    bat::assets::build(source_dir, !blank, target_dir, clap::crate_version!())
}

fn run_cache_subcommand(matches: &clap::ArgMatches) -> Result<()> {
    if matches.is_present("build") {
        #[cfg(feature = "build-assets")]
        build_assets(matches)?;
        #[cfg(not(feature = "build-assets"))]
        println!("bat has been built without the 'build-assets' feature. The 'cache --build' option is not available.");
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
            (_, MappingTarget::MapExtensionToUnknown) => {}
            (matcher, MappingTarget::MapTo(s)) => {
                let globs = map.entry(*s).or_insert_with(Vec::new);
                globs.push(matcher.glob().glob().into());
            }
        }
    }
    map
}

pub fn get_languages(config: &Config) -> Result<String> {
    let mut result: String = String::new();

    let assets = assets_from_cache_or_binary(config.use_custom_assets)?;
    let mut languages = assets
        .get_syntaxes()?
        .iter()
        .filter(|syntax| !syntax.hidden && !syntax.file_extensions.is_empty())
        .cloned()
        .collect::<Vec<_>>();

    // Handling of file-extension conflicts, see issue #1076
    for lang in &mut languages {
        let lang_name = lang.name.clone();
        lang.file_extensions.retain(|extension| {
            // The 'extension' variable is not certainly a real extension.
            //
            // Skip if 'extension' starts with '.', likely a hidden file like '.vimrc'
            // Also skip if the 'extension' contains another real extension, likely
            // that is a full match file name like 'CMakeLists.txt' and 'Cargo.lock'
            if extension.starts_with('.') || Path::new(extension).extension().is_some() {
                return true;
            }

            let test_file = Path::new("test").with_extension(extension);
            let syntax_in_set = assets.get_syntax_for_path(test_file, &config.syntax_mapping);
            matches!(syntax_in_set, Ok(syntax_in_set) if syntax_in_set.syntax.name == lang_name)
        });
    }

    languages.sort_by_key(|lang| lang.name.to_uppercase());

    let configured_languages = get_syntax_mapping_to_paths(config.syntax_mapping.mappings());

    for lang in &mut languages {
        if let Some(additional_paths) = configured_languages.get(lang.name.as_str()) {
            lang.file_extensions
                .extend(additional_paths.iter().cloned());
        }
    }

    if config.loop_through {
        for lang in languages {
            result += &format!("{}:{}\n", lang.name, lang.file_extensions.join(","));
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
            result += &format!("{:width$}{}", lang.name, separator, width = longest);

            // Number of characters on this line so far, wrap before `desired_width`
            let mut num_chars = 0;

            let mut extension = lang.file_extensions.iter().peekable();
            while let Some(word) = extension.next() {
                // If we can't fit this word in, then create a line break and align it in.
                let new_chars = word.len() + comma_separator.len();
                if num_chars + new_chars >= desired_width {
                    num_chars = 0;
                    result += &format!("\n{:width$}{}", "", separator, width = longest);
                }

                num_chars += new_chars;
                result += &format!("{}", style.paint(&word[..]));
                if extension.peek().is_some() {
                    result += comma_separator;
                }
            }
            result += "\n";
        }
    }

    Ok(result)
}

fn theme_preview_file<'a>() -> Input<'a> {
    Input::from_reader(Box::new(BufReader::new(THEME_PREVIEW_DATA)))
}

pub fn list_themes(cfg: &Config) -> Result<()> {
    let assets = assets_from_cache_or_binary(cfg.use_custom_assets)?;
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
        writeln!(
            stdout,
            "Further themes can be installed to '{}', \
            and are added to the cache with `bat cache --build`. \
            For more information, see:\n\n  \
            https://github.com/sharkdp/bat#adding-new-themes",
            PROJECT_DIRS.config_dir().join("themes").to_string_lossy()
        )?;
    } else {
        for theme in assets.themes() {
            writeln!(stdout, "{}", theme)?;
        }
    }

    Ok(())
}

fn run_controller(inputs: Vec<Input>, config: &Config) -> Result<bool> {
    let assets = assets_from_cache_or_binary(config.use_custom_assets)?;
    let controller = Controller::new(config, &assets);
    controller.run(inputs)
}

#[cfg(feature = "bugreport")]
fn invoke_bugreport(app: &App) {
    use bugreport::{bugreport, collector::*, format::Markdown};
    let pager = bat::config::get_pager_executable(app.matches.value_of("pager"))
        .unwrap_or_else(|| "less".to_owned()); // FIXME: Avoid non-canonical path to "less".

    let mut report = bugreport!()
        .info(SoftwareVersion::default())
        .info(OperatingSystem::default())
        .info(CommandLine::default())
        .info(EnvironmentVariables::list(&[
            "SHELL",
            "PAGER",
            "LESS",
            "LANG",
            "LC_ALL",
            "BAT_PAGER",
            "BAT_CACHE_PATH",
            "BAT_CONFIG_PATH",
            "BAT_OPTS",
            "BAT_STYLE",
            "BAT_TABS",
            "BAT_THEME",
            "XDG_CONFIG_HOME",
            "XDG_CACHE_HOME",
            "COLORTERM",
            "NO_COLOR",
            "MANPAGER",
        ]))
        .info(FileContent::new("Config file", config_file()))
        .info(CompileTimeInformation::default());

    #[cfg(feature = "paging")]
    if let Ok(resolved_path) = grep_cli::resolve_binary(pager) {
        report = report.info(CommandOutput::new(
            "Less version",
            resolved_path,
            &["--version"],
        ))
    };

    report.print::<Markdown>();
}

/// Returns `Err(..)` upon fatal errors. Otherwise, returns `Ok(true)` on full success and
/// `Ok(false)` if any intermediate errors occurred (were printed).
fn run() -> Result<bool> {
    let app = App::new()?;

    if app.matches.is_present("diagnostic") {
        #[cfg(feature = "bugreport")]
        invoke_bugreport(&app);
        #[cfg(not(feature = "bugreport"))]
        println!("bat has been built without the 'bugreport' feature. The '--diagnostic' option is not available.");
        return Ok(true);
    }

    match app.matches.subcommand() {
        ("cache", Some(cache_matches)) => {
            // If there is a file named 'cache' in the current working directory,
            // arguments for subcommand 'cache' are not mandatory.
            // If there are non-zero arguments, execute the subcommand cache, else, open the file cache.
            if !cache_matches.args.is_empty() {
                run_cache_subcommand(cache_matches)?;
                Ok(true)
            } else {
                let inputs = vec![Input::ordinary_file("cache")];
                let config = app.config(&inputs)?;

                run_controller(inputs, &config)
            }
        }
        _ => {
            let inputs = app.inputs()?;
            let config = app.config(&inputs)?;

            if app.matches.is_present("list-languages") {
                let languages: String = get_languages(&config)?;
                let inputs: Vec<Input> = vec![Input::from_reader(Box::new(languages.as_bytes()))];
                let plain_config = Config {
                    style_components: StyleComponents::new(StyleComponent::Plain.components(false)),
                    paging_mode: PagingMode::QuitIfOneScreen,
                    ..Default::default()
                };
                run_controller(inputs, &plain_config)
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

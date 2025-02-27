#![deny(unsafe_code)]

mod app;
mod assets;
mod clap_app;
#[cfg(feature = "application")]
mod completions;
mod config;
mod directories;
mod input;

use std::collections::{HashMap, HashSet};
use std::fmt::Write as _;
use std::io;
use std::io::{BufReader, Write};
use std::path::Path;
use std::process;

use bat::output::{OutputHandle, OutputType};
use bat::theme::DetectColorScheme;
use nu_ansi_term::Color::Green;
use nu_ansi_term::Style;

use crate::{
    app::App,
    config::{config_file, generate_config_file},
};

#[cfg(feature = "bugreport")]
use crate::config::system_config_file;

use assets::{assets_from_cache_or_binary, clear_assets};
use directories::PROJECT_DIRS;
use globset::GlobMatcher;

use bat::{
    config::Config,
    controller::Controller,
    error::*,
    input::Input,
    style::{StyleComponent, StyleComponents},
    theme::{color_scheme, default_theme, ColorScheme},
    MappingTarget, PagingMode,
};

const THEME_PREVIEW_DATA: &[u8] = include_bytes!("../../../assets/theme_preview.rs");

#[cfg(feature = "build-assets")]
fn build_assets(matches: &clap::ArgMatches, config_dir: &Path, cache_dir: &Path) -> Result<()> {
    let source_dir = matches
        .get_one::<String>("source")
        .map(Path::new)
        .unwrap_or_else(|| config_dir);

    bat::assets::build(
        source_dir,
        !matches.get_flag("blank"),
        matches.get_flag("acknowledgements"),
        cache_dir,
        clap::crate_version!(),
    )
}

fn run_cache_subcommand(
    matches: &clap::ArgMatches,
    #[cfg(feature = "build-assets")] config_dir: &Path,
    default_cache_dir: &Path,
) -> Result<()> {
    let cache_dir = matches
        .get_one::<String>("target")
        .map(Path::new)
        .unwrap_or_else(|| default_cache_dir);

    if matches.get_flag("build") {
        #[cfg(feature = "build-assets")]
        build_assets(matches, config_dir, cache_dir)?;
        #[cfg(not(feature = "build-assets"))]
        println!("bat has been built without the 'build-assets' feature. The 'cache --build' option is not available.");
    } else if matches.get_flag("clear") {
        clear_assets(cache_dir);
    }

    Ok(())
}

fn get_syntax_mapping_to_paths<'r, 't, I>(mappings: I) -> HashMap<&'t str, Vec<String>>
where
    I: IntoIterator<Item = (&'r GlobMatcher, &'r MappingTarget<'t>)>,
    't: 'r, // target text outlives rule
{
    let mut map = HashMap::new();
    for mapping in mappings {
        if let (matcher, MappingTarget::MapTo(s)) = mapping {
            let globs = map.entry(*s).or_insert_with(Vec::new);
            globs.push(matcher.glob().glob().into());
        }
    }
    map
}

pub fn get_languages(config: &Config, cache_dir: &Path) -> Result<String> {
    let mut result: String = String::new();

    let assets = assets_from_cache_or_binary(config.use_custom_assets, cache_dir)?;
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

    let configured_languages = get_syntax_mapping_to_paths(config.syntax_mapping.all_mappings());

    for lang in &mut languages {
        if let Some(additional_paths) = configured_languages.get(lang.name.as_str()) {
            lang.file_extensions
                .extend(additional_paths.iter().cloned());
        }
    }

    if config.loop_through {
        for lang in languages {
            writeln!(result, "{}:{}", lang.name, lang.file_extensions.join(",")).ok();
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
            write!(result, "{:width$}{separator}", lang.name, width = longest).ok();

            // Number of characters on this line so far, wrap before `desired_width`
            let mut num_chars = 0;

            let mut extension = lang.file_extensions.iter().peekable();
            while let Some(word) = extension.next() {
                // If we can't fit this word in, then create a line break and align it in.
                let new_chars = word.len() + comma_separator.len();
                if num_chars + new_chars >= desired_width {
                    num_chars = 0;
                    write!(result, "\n{:width$}{separator}", "", width = longest).ok();
                }

                num_chars += new_chars;
                write!(result, "{}", style.paint(&word[..])).ok();
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

pub fn list_themes(
    cfg: &Config,
    config_dir: &Path,
    cache_dir: &Path,
    detect_color_scheme: DetectColorScheme,
) -> Result<()> {
    let assets = assets_from_cache_or_binary(cfg.use_custom_assets, cache_dir)?;
    let mut config = cfg.clone();
    let mut style = HashSet::new();
    style.insert(StyleComponent::Plain);
    config.language = Some("Rust");
    config.style_components = StyleComponents(style);

    let mut output_type =
        OutputType::from_mode(config.paging_mode, config.wrapping_mode, config.pager)?;
    let mut writer = output_type.handle()?;

    let default_theme_name = default_theme(color_scheme(detect_color_scheme).unwrap_or_default());
    for theme in assets.themes() {
        let default_theme_info = if default_theme_name == theme {
            " (default)"
        } else if default_theme(ColorScheme::Dark) == theme {
            " (default dark)"
        } else if default_theme(ColorScheme::Light) == theme {
            " (default light)"
        } else {
            ""
        };
        if config.colored_output {
            writeln!(
                writer,
                "Theme: {}{default_theme_info}\n",
                Style::new().bold().paint(theme.to_string()),
            )?;
            config.theme = theme.to_string();
            Controller::new(&config, &assets)
                .run(
                    vec![theme_preview_file()],
                    Some(OutputHandle::IoWrite(&mut writer)),
                )
                .ok();
            writeln!(writer)?;
        } else if config.loop_through {
            writeln!(writer, "{theme}")?;
        } else {
            writeln!(writer, "{theme}{default_theme_info}")?;
        }
    }

    if config.colored_output {
        writeln!(
            writer,
            "Further themes can be installed to '{}', \
            and are added to the cache with `bat cache --build`. \
            For more information, see:\n\n  \
            https://github.com/sharkdp/bat#adding-new-themes",
            config_dir.join("themes").to_string_lossy()
        )?;
    }

    Ok(())
}

fn set_terminal_title_to(new_terminal_title: String) {
    let osc_command_for_setting_terminal_title = "\x1b]0;";
    let osc_end_command = "\x07";
    print!("{osc_command_for_setting_terminal_title}{new_terminal_title}{osc_end_command}");
    io::stdout().flush().unwrap();
}

fn get_new_terminal_title(inputs: &Vec<Input>) -> String {
    let mut new_terminal_title = "bat: ".to_string();
    for (index, input) in inputs.iter().enumerate() {
        new_terminal_title += input.description().title();
        if index < inputs.len() - 1 {
            new_terminal_title += ", ";
        }
    }
    new_terminal_title
}

fn run_controller(inputs: Vec<Input>, config: &Config, cache_dir: &Path) -> Result<bool> {
    let assets = assets_from_cache_or_binary(config.use_custom_assets, cache_dir)?;
    let controller = Controller::new(config, &assets);
    if config.paging_mode != PagingMode::Never && config.set_terminal_title {
        set_terminal_title_to(get_new_terminal_title(&inputs));
    }
    controller.run(inputs, None)
}

#[cfg(feature = "bugreport")]
fn invoke_bugreport(app: &App, cache_dir: &Path) {
    use bugreport::{bugreport, collector::*, format::Markdown};
    let pager = bat::config::get_pager_executable(
        app.matches.get_one::<String>("pager").map(|s| s.as_str()),
    )
    .unwrap_or_else(|| "less".to_owned()); // FIXME: Avoid non-canonical path to "less".

    let mut custom_assets_metadata = cache_dir.to_path_buf();
    custom_assets_metadata.push("metadata.yaml");

    let mut report = bugreport!()
        .info(SoftwareVersion::default())
        .info(OperatingSystem::default())
        .info(CommandLine::default())
        .info(EnvironmentVariables::list(&[
            "BAT_CACHE_PATH",
            "BAT_CONFIG_PATH",
            "BAT_OPTS",
            "BAT_PAGER",
            "BAT_PAGING",
            "BAT_STYLE",
            "BAT_TABS",
            "BAT_THEME",
            "COLORTERM",
            "LANG",
            "LC_ALL",
            "LESS",
            "MANPAGER",
            "NO_COLOR",
            "PAGER",
            "SHELL",
            "TERM",
            "XDG_CACHE_HOME",
            "XDG_CONFIG_HOME",
        ]))
        .info(FileContent::new("System Config file", system_config_file()))
        .info(FileContent::new("Config file", config_file()))
        .info(FileContent::new(
            "Custom assets metadata",
            custom_assets_metadata,
        ))
        .info(DirectoryEntries::new("Custom assets", cache_dir))
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
    let config_dir = PROJECT_DIRS.config_dir();
    let cache_dir = PROJECT_DIRS.cache_dir();

    if app.matches.get_flag("diagnostic") {
        #[cfg(feature = "bugreport")]
        invoke_bugreport(&app, cache_dir);
        #[cfg(not(feature = "bugreport"))]
        println!("bat has been built without the 'bugreport' feature. The '--diagnostic' option is not available.");
        return Ok(true);
    }

    #[cfg(feature = "application")]
    if let Some(shell) = app.matches.get_one::<String>("completion") {
        match shell.as_str() {
            "bash" => println!("{}", completions::BASH_COMPLETION),
            "fish" => println!("{}", completions::FISH_COMPLETION),
            "ps1" => println!("{}", completions::PS1_COMPLETION),
            "zsh" => println!("{}", completions::ZSH_COMPLETION),
            _ => unreachable!("No completion for shell '{shell}' available."),
        }
        return Ok(true);
    }

    match app.matches.subcommand() {
        Some(("cache", cache_matches)) => {
            // If there is a file named 'cache' in the current working directory,
            // arguments for subcommand 'cache' are not mandatory.
            // If there are non-zero arguments, execute the subcommand cache, else, open the file cache.
            if cache_matches.args_present() {
                run_cache_subcommand(
                    cache_matches,
                    #[cfg(feature = "build-assets")]
                    config_dir,
                    cache_dir,
                )?;
                Ok(true)
            } else {
                let inputs = vec![Input::ordinary_file("cache")];
                let config = app.config(&inputs)?;

                run_controller(inputs, &config, cache_dir)
            }
        }
        _ => {
            let inputs = app.inputs()?;
            let config = app.config(&inputs)?;

            if app.matches.get_flag("list-languages") {
                let languages: String = get_languages(&config, cache_dir)?;
                let inputs: Vec<Input> = vec![Input::from_reader(Box::new(languages.as_bytes()))];
                let plain_config = Config {
                    style_components: StyleComponents::new(StyleComponent::Plain.components(false)),
                    paging_mode: PagingMode::QuitIfOneScreen,
                    ..Default::default()
                };
                run_controller(inputs, &plain_config, cache_dir)
            } else if app.matches.get_flag("list-themes") {
                list_themes(&config, config_dir, cache_dir, DetectColorScheme::default())?;
                Ok(true)
            } else if app.matches.get_flag("config-file") {
                println!("{}", config_file().to_string_lossy());
                Ok(true)
            } else if app.matches.get_flag("generate-config-file") {
                generate_config_file()?;
                Ok(true)
            } else if app.matches.get_flag("config-dir") {
                writeln!(io::stdout(), "{}", config_dir.to_string_lossy())?;
                Ok(true)
            } else if app.matches.get_flag("cache-dir") {
                writeln!(io::stdout(), "{}", cache_dir.to_string_lossy())?;
                Ok(true)
            } else if app.matches.get_flag("acknowledgements") {
                writeln!(io::stdout(), "{}", bat::assets::get_acknowledgements())?;
                Ok(true)
            } else {
                run_controller(inputs, &config, cache_dir)
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

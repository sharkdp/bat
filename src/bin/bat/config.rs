use std::env;
use std::ffi::OsString;
use std::fs;
use std::io::{self, Write};
use std::path::PathBuf;

use crate::directories::PROJECT_DIRS;

#[cfg(not(target_os = "windows"))]
const DEFAULT_SYSTEM_CONFIG_PREFIX: &str = "/etc";

#[cfg(target_os = "windows")]
const DEFAULT_SYSTEM_CONFIG_PREFIX: &str = "C:\\ProgramData";

pub fn system_config_file() -> PathBuf {
    let folder = option_env!("BAT_SYSTEM_CONFIG_PREFIX").unwrap_or(DEFAULT_SYSTEM_CONFIG_PREFIX);
    let mut path = PathBuf::from(folder);

    path.push("bat");
    path.push("config");

    path
}

pub fn config_file() -> PathBuf {
    env::var("BAT_CONFIG_PATH")
        .ok()
        .map(PathBuf::from)
        .unwrap_or_else(|| PROJECT_DIRS.config_dir().join("config"))
}

pub fn generate_config_file() -> bat::error::Result<()> {
    let config_file = config_file();
    if config_file.is_file() {
        println!(
            "A config file already exists at: {}",
            config_file.to_string_lossy()
        );

        print!("Overwrite? (y/N): ");
        io::stdout().flush()?;
        let mut decision = String::new();
        io::stdin().read_line(&mut decision)?;

        if !decision.trim().eq_ignore_ascii_case("Y") {
            return Ok(());
        }
    } else {
        let config_dir = config_file.parent();
        match config_dir {
            Some(path) => fs::create_dir_all(path)?,
            None => {
                return Err(format!(
                    "Unable to write config file to: {}",
                    config_file.to_string_lossy()
                )
                .into());
            }
        }
    }

    let default_config = r#"# This is `bat`s configuration file. Each line either contains a comment or
# a command-line option that you want to pass to `bat` by default. You can
# run `bat --help` to get a list of all possible configuration options.

# Specify desired highlighting theme (e.g. "TwoDark"). Run `bat --list-themes`
# for a list of all available themes
#--theme="TwoDark"

# Enable this to use italic text on the terminal. This is not supported on all
# terminal emulators (like tmux, by default):
#--italic-text=always

# Uncomment the following line to disable automatic paging:
#--paging=never

# Uncomment the following line if you are using less version >= 551 and want to
# enable mouse scrolling support in `bat` when running inside tmux. This might
# disable text selection, unless you press shift.
#--pager="less --RAW-CONTROL-CHARS --quit-if-one-screen --mouse"

# Syntax mappings: map a certain filename pattern to a language.
#   Example 1: use the C++ syntax for Arduino .ino files
#   Example 2: Use ".gitignore"-style highlighting for ".ignore" files
#--map-syntax "*.ino:C++"
#--map-syntax ".ignore:Git Ignore"
"#;

    fs::write(&config_file, default_config).map_err(|e| {
        format!(
            "Failed to create config file at '{}': {e}",
            config_file.to_string_lossy(),
        )
    })?;

    println!(
        "Success! Config file written to {}",
        config_file.to_string_lossy()
    );

    Ok(())
}

pub fn get_args_from_config_file() -> Result<Vec<OsString>, shell_words::ParseError> {
    let mut config = String::new();

    if let Ok(c) = fs::read_to_string(system_config_file()) {
        config.push_str(&c);
        config.push('\n');
    }

    if let Ok(c) = fs::read_to_string(config_file()) {
        config.push_str(&c);
    }

    get_args_from_str(&config)
}

pub fn get_args_from_env_opts_var() -> Option<Result<Vec<OsString>, shell_words::ParseError>> {
    env::var("BAT_OPTS").ok().map(|s| get_args_from_str(&s))
}

fn get_args_from_str(content: &str) -> Result<Vec<OsString>, shell_words::ParseError> {
    let args_per_line = content
        .split('\n')
        .map(|line| line.trim())
        .filter(|line| !line.is_empty())
        .filter(|line| !line.starts_with('#'))
        .map(shell_words::split)
        .collect::<Result<Vec<_>, _>>()?;

    Ok(args_per_line
        .iter()
        .flatten()
        .map(|line| line.into())
        .collect())
}

pub fn get_args_from_env_vars() -> Vec<OsString> {
    [
        ("--tabs", "BAT_TABS"),
        ("--theme", bat::theme::env::BAT_THEME),
        ("--theme-dark", bat::theme::env::BAT_THEME_DARK),
        ("--theme-light", bat::theme::env::BAT_THEME_LIGHT),
        ("--pager", "BAT_PAGER"),
        ("--paging", "BAT_PAGING"),
        ("--style", "BAT_STYLE"),
    ]
    .iter()
    .filter_map(|(flag, key)| {
        env::var(key)
            .ok()
            .map(|var| [flag.to_string(), var].join("="))
    })
    .map(|a| a.into())
    .collect()
}

#[test]
fn empty() {
    let args = get_args_from_str("").unwrap();
    assert!(args.is_empty());
}

#[test]
fn single() {
    assert_eq!(vec!["--plain"], get_args_from_str("--plain").unwrap());
}

#[test]
fn multiple() {
    assert_eq!(
        vec!["--plain", "--language=cpp"],
        get_args_from_str("--plain --language=cpp").unwrap()
    );
}

#[test]
fn quotes() {
    assert_eq!(
        vec!["--theme", "Sublime Snazzy"],
        get_args_from_str("--theme \"Sublime Snazzy\"").unwrap()
    );
}

#[test]
fn multi_line() {
    let config = "
    -p
    --style numbers,changes

    --color=always
    ";
    assert_eq!(
        vec!["-p", "--style", "numbers,changes", "--color=always"],
        get_args_from_str(config).unwrap()
    );
}

#[test]
fn comments() {
    let config = "
    # plain style
    -p

    # show line numbers and Git modifications
    --style numbers,changes

    # Always show ANSI colors
    --color=always
    ";
    assert_eq!(
        vec!["-p", "--style", "numbers,changes", "--color=always"],
        get_args_from_str(config).unwrap()
    );
}

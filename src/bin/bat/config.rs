use std::env;
use std::ffi::OsString;
use std::fs;
use std::io::{self, Write};
use std::path::{Path, PathBuf};

use crate::directories::PROJECT_DIRS;

#[cfg(not(target_os = "windows"))]
const DEFAULT_SYSTEM_CONFIG_PREFIX: &str = "/etc";

#[cfg(target_os = "windows")]
const DEFAULT_SYSTEM_CONFIG_PREFIX: &str = "C:\\ProgramData";

pub fn system_config_file() -> PathBuf {
    let folder = option_env!("BAT_SYSTEM_CONFIG_PREFIX").unwrap_or(DEFAULT_SYSTEM_CONFIG_PREFIX);
    let mut path = PathBuf::from(folder);

    path.push("bat");
    config_in_directory(&path)
}

pub fn config_file() -> PathBuf {
    env::var("BAT_CONFIG_PATH")
        .ok()
        .map(PathBuf::from)
        .unwrap_or_else(|| config_in_directory(PROJECT_DIRS.config_dir()))
}

fn config_in_directory(directory: &Path) -> PathBuf {
    let legacy = directory.join("config");
    let toml = directory.join("config.toml");
    if !legacy.exists() && toml.is_file() {
        toml
    } else {
        legacy
    }
}

fn is_toml(path: &Path) -> bool {
    path.extension()
        .is_some_and(|extension| extension.eq_ignore_ascii_case("toml"))
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

    let default_config = if is_toml(&config_file) {
        "# Options use their long command-line names.\n\
         # String and integer values are passed as option values.\n\
         # Arrays repeat an option; true enables a flag and false omits it.\n\n\
         # theme = \"TwoDark\"\n\
         # italic-text = \"always\"\n\
         # paging = \"never\"\n\
         # map-syntax = [\"*.ino:C++\", \".ignore:Git Ignore\"]\n"
    } else {
        default_config
    };

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

pub fn get_args_from_config_file() -> bat::error::Result<Vec<OsString>> {
    let system_config = system_config_file();
    let user_config = config_file();

    let mut args = read_config(&system_config)?;

    // Skip the user config if it resolves to the same file as the system config,
    // which can happen when BAT_CONFIG_DIR is set to e.g. "/etc/bat". See #3589.
    if !same_file(&system_config, &user_config) {
        args.extend(read_config(&user_config)?);
    }
    Ok(args)
}

fn read_config(path: &Path) -> bat::error::Result<Vec<OsString>> {
    let Ok(content) = fs::read_to_string(path) else {
        return Ok(Vec::new());
    };
    let result = if is_toml(path) {
        get_args_from_toml(&content)
    } else {
        get_args_from_str(&content).map_err(|error| error.to_string())
    };
    result.map_err(|error| {
        format!(
            "Could not parse configuration file '{}': {error}",
            path.display()
        )
        .into()
    })
}

fn get_args_from_toml(content: &str) -> Result<Vec<OsString>, String> {
    let table: toml::Table = toml::from_str(content).map_err(|error| format!("{error}"))?;
    let command = crate::clap_app::build_app(false);
    let mut args = Vec::new();
    for (key, value) in table {
        let option = command
            .get_arguments()
            .find(|arg| arg.get_long() == Some(key.as_str()))
            .ok_or_else(|| format!("Unknown option '{key}'"))?;
        match option.get_action() {
            clap::ArgAction::SetTrue => match value {
                toml::Value::Boolean(true) => args.push(format!("--{key}").into()),
                toml::Value::Boolean(false) => {}
                _ => return Err(format!("Option '{key}' requires a boolean")),
            },
            clap::ArgAction::Count => {
                let count = match value {
                    toml::Value::Boolean(enabled) => u8::from(enabled),
                    toml::Value::Integer(count) => u8::try_from(count)
                        .map_err(|_| format!("Option '{key}' requires a count from 0 to 255"))?,
                    _ => return Err(format!("Option '{key}' requires a boolean or count")),
                };
                args.extend(std::iter::repeat_n(
                    OsString::from(format!("--{key}")),
                    count.into(),
                ));
            }
            _ => {
                let values = match value {
                    toml::Value::Array(values) => values,
                    value => vec![value],
                };
                for value in values {
                    let value = match value {
                        toml::Value::String(value) => value,
                        toml::Value::Integer(value) => value.to_string(),
                        _ => {
                            return Err(format!(
                            "Option '{key}' requires a string, integer, or array of these values"
                        ))
                        }
                    };
                    args.push(format!("--{key}={value}").into());
                }
            }
        }
    }
    Ok(args)
}

fn same_file(a: &Path, b: &Path) -> bool {
    match (fs::canonicalize(a), fs::canonicalize(b)) {
        (Ok(a), Ok(b)) => a == b,
        _ => a == b,
    }
}

pub fn get_args_from_env_opts_var() -> Option<bat::error::Result<Vec<OsString>>> {
    env::var("BAT_OPTS").ok().map(|s| {
        get_args_from_str(&s).map_err(|error| format!("Could not parse BAT_OPTS: {error}").into())
    })
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
        ("--terminal-width", "BAT_WIDTH"),
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

#[test]
fn same_file_identical_paths() {
    let dir = tempfile::tempdir().unwrap();
    let file = dir.path().join("config");
    fs::write(&file, "").unwrap();
    assert!(same_file(&file, &file));
}

#[test]
fn same_file_different_paths() {
    let dir = tempfile::tempdir().unwrap();
    let a = dir.path().join("a");
    let b = dir.path().join("b");
    fs::write(&a, "").unwrap();
    fs::write(&b, "").unwrap();
    assert!(!same_file(&a, &b));
}

#[test]
fn same_file_nonexistent() {
    let dir = tempfile::tempdir().unwrap();
    let a = dir.path().join("a");
    let b = dir.path().join("b");
    assert!(!same_file(&a, &b));
}

#[cfg(unix)]
#[test]
fn same_file_via_symlink() {
    let dir = tempfile::tempdir().unwrap();
    let original = dir.path().join("config");
    let link = dir.path().join("link");
    fs::write(&original, "").unwrap();
    std::os::unix::fs::symlink(&original, &link).unwrap();
    assert!(same_file(&original, &link));
}

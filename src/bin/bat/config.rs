use std::env;
use std::ffi::OsString;
use std::fs;
use std::io::{self, Write};
use std::path::PathBuf;

use crate::directories::PROJECT_DIRS;

pub fn config_file() -> PathBuf {
    env::var("BAT_CONFIG_PATH")
        .ok()
        .map(PathBuf::from)
        .filter(|config_path| config_path.is_file())
        .unwrap_or_else(|| PROJECT_DIRS.config_dir().join("config"))
}

pub fn generate_config_file() -> bat::error::Result<()> {
    let config_file = config_file();
    if config_file.exists() {
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

    let default_config =
        r#"# This is `bat`s configuration file. Each line either contains a comment or
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
#   Example 1: use the C++ syntax for .ino files
#   Example 2: Use ".gitignore"-style highlighting for ".ignore" files
#--map-syntax "*.ino:C++"
#--map-syntax ".ignore:Git Ignore"
"#;

    fs::write(&config_file, default_config)?;
    println!(
        "Success! Config file written to {}",
        config_file.to_string_lossy()
    );

    Ok(())
}

pub fn get_args_from_config_file() -> Result<Vec<OsString>, shell_words::ParseError> {
    Ok(fs::read_to_string(config_file())
        .ok()
        .map(|content| get_args_from_str(&content))
        .transpose()?
        .unwrap_or_else(|| vec![]))
}

pub fn get_args_from_env_var() -> Option<Result<Vec<OsString>, shell_words::ParseError>> {
    env::var("BAT_OPTS").ok().map(|s| get_args_from_str(&s))
}

fn get_args_from_str(content: &str) -> Result<Vec<OsString>, shell_words::ParseError> {
    let args_per_line = content
        .split('\n')
        .map(|line| line.trim())
        .filter(|line| !line.is_empty())
        .filter(|line| !line.starts_with('#'))
        .map(|line| shell_words::split(line))
        .collect::<Result<Vec<_>, _>>()?;

    Ok(args_per_line
        .iter()
        .flatten()
        .map(|line| line.into())
        .collect())
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

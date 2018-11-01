use std::env;
use std::ffi::OsString;
use std::fs;
use std::path::PathBuf;

use shell_words;

use dirs::PROJECT_DIRS;
use util::transpose;

pub fn config_file() -> PathBuf {
    env::var("BAT_CONFIG_PATH")
        .ok()
        .map(PathBuf::from)
        .filter(|config_path| config_path.is_file())
        .unwrap_or(PROJECT_DIRS.config_dir().join("config"))
}

pub fn get_args_from_config_file() -> Result<Vec<OsString>, shell_words::ParseError> {
    Ok(transpose(
        fs::read_to_string(config_file())
            .ok()
            .map(|content| get_args_from_str(&content)),
    )?
    .unwrap_or(vec![]))
}

pub fn get_args_from_env_var() -> Option<Result<Vec<OsString>, shell_words::ParseError>> {
    env::var("BAT_OPTS").ok().map(|s| get_args_from_str(&s))
}

fn get_args_from_str<'a>(content: &'a str) -> Result<Vec<OsString>, shell_words::ParseError> {
    let args_per_line = content
        .split('\n')
        .map(|line| line.trim())
        .filter(|line| !line.is_empty())
        .filter(|line| !line.starts_with("#"))
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

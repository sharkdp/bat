use std::ffi::OsString;
use std::fs;

use shell_words;

use dirs::PROJECT_DIRS;

pub fn get_args_from_config_file() -> Vec<OsString> {
    let config_file = PROJECT_DIRS.config_dir().join("config");
    fs::read_to_string(config_file)
        .map(|content| get_args_from_str(&content))
        .unwrap_or(vec![])
}

fn get_args_from_str<'a>(content: &'a str) -> Vec<OsString> {
    content
        .split('\n')
        .map(|line| line.trim())
        .filter(|line| !line.is_empty())
        .filter(|line| !line.starts_with("#"))
        .flat_map(|line| shell_words::split(line).unwrap())
        .map(|line| line.into())
        .collect()
}

#[test]
fn empty() {
    let args = get_args_from_str("");
    assert!(args.is_empty());
}

#[test]
fn single() {
    assert_eq!(vec!["--plain"], get_args_from_str("--plain"));
}

#[test]
fn multiple() {
    assert_eq!(
        vec!["--plain", "--language=cpp"],
        get_args_from_str("--plain --language=cpp")
    );
}

#[test]
fn quotes() {
    assert_eq!(
        vec!["--theme", "Sublime Snazzy"],
        get_args_from_str("--theme \"Sublime Snazzy\"")
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
        get_args_from_str(config)
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
        get_args_from_str(config)
    );
}

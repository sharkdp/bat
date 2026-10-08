//! Import the TextMate-compatible parts of VS Code JSON color themes.

use std::fs;
use std::path::{Path, PathBuf};

use serde_json::{Map, Value};
use syntect::highlighting::{Color, FontStyle, StyleModifier, Theme, ThemeItem, ThemeSet};

use crate::error::Result;

pub(super) fn add_from_folder(themes: &mut ThemeSet, directory: &Path) -> Result<()> {
    let mut paths = Vec::new();
    for entry in walkdir::WalkDir::new(directory).follow_links(true) {
        let entry = entry.map_err(|error| error.to_string())?;
        if entry.file_type().is_file()
            && entry.path().extension().is_some_and(|extension| {
                extension.eq_ignore_ascii_case("json") || extension.eq_ignore_ascii_case("jsonc")
            })
        {
            paths.push(entry.into_path());
        }
    }
    paths.sort();
    for path in paths {
        if let Some(mut theme) = load(&path, &mut Vec::new())? {
            // VS Code gives later rules priority; syntect keeps the first rule
            // when selectors have equal specificity.
            theme.scopes.reverse();
            let name = path
                .file_stem()
                .unwrap_or_default()
                .to_string_lossy()
                .into_owned();
            themes.themes.insert(name, theme);
        }
    }
    Ok(())
}

fn load(path: &Path, parents: &mut Vec<PathBuf>) -> Result<Option<Theme>> {
    let path = fs::canonicalize(path)
        .map_err(|error| format!("Could not load theme '{}': {error}", path.display()))?;
    if parents.len() >= 32 || parents.contains(&path) {
        return Err(format!(
            "Cyclic or excessively deep theme include at '{}'",
            path.display()
        )
        .into());
    }
    parents.push(path.clone());
    let result = load_inner(&path, parents);
    parents.pop();
    result.map_err(|error| format!("Could not load theme '{}': {error}", path.display()).into())
}

fn load_inner(path: &Path, parents: &mut Vec<PathBuf>) -> Result<Option<Theme>> {
    if path
        .extension()
        .is_some_and(|extension| extension.eq_ignore_ascii_case("tmTheme"))
    {
        return Ok(Some(ThemeSet::get_theme(path)?));
    }
    let text = fs::read_to_string(path)?;
    let value: Value = serde_json::from_str(&jsonc(&text)?).map_err(|error| error.to_string())?;
    let Some(object) = value.as_object() else {
        return Ok(None);
    };
    if !["colors", "tokenColors", "settings"]
        .iter()
        .any(|key| object.contains_key(*key))
        && !object.get("include").is_some_and(Value::is_string)
    {
        return Ok(None);
    }
    let mut theme = if let Some(include) = object.get("include") {
        let include = include
            .as_str()
            .ok_or("Theme include must be a file path")?;
        load(
            &path.parent().unwrap_or(Path::new(".")).join(include),
            parents,
        )?
        .ok_or("Included file is not a color theme")?
    } else {
        Theme::default()
    };
    theme.name = object
        .get("name")
        .and_then(Value::as_str)
        .map(str::to_owned)
        .or_else(|| {
            path.file_stem()
                .map(|stem| stem.to_string_lossy().into_owned())
        });

    if let Some(colors) = object.get("colors") {
        let colors = colors.as_object().ok_or("Theme colors must be an object")?;
        macro_rules! setting {
            ($key:literal, $field:ident) => {
                if colors.get($key).and_then(Value::as_str) == Some("default") {
                    theme.settings.$field = None;
                } else if let Some(color) = color_setting(colors, $key)? {
                    theme.settings.$field = Some(color);
                }
            };
        }
        setting!("editor.foreground", foreground);
        setting!("editor.background", background);
        setting!("editorCursor.foreground", caret);
        setting!("editor.lineHighlightBackground", line_highlight);
        setting!("editorLineNumber.foreground", gutter_foreground);
        setting!("editorGutter.background", gutter);
        setting!("editor.selectionBackground", selection);
        setting!("editor.selectionForeground", selection_foreground);
        setting!("editor.inactiveSelectionBackground", inactive_selection);
        setting!("editor.findMatchBackground", find_highlight);
        setting!("editor.findMatchForeground", find_highlight_foreground);
    }
    if let Some(tokens) = object.get("tokenColors").or_else(|| object.get("settings")) {
        if let Some(file) = tokens.as_str() {
            let external = load(&path.parent().unwrap_or(Path::new(".")).join(file), parents)?
                .ok_or("Token color file is not a color theme")?;
            theme.scopes.extend(external.scopes);
            if theme.settings.foreground.is_none() {
                theme.settings.foreground = external.settings.foreground;
            }
            if theme.settings.background.is_none() {
                theme.settings.background = external.settings.background;
            }
        } else {
            for rule in tokens
                .as_array()
                .ok_or("Theme tokenColors must be an array or file path")?
            {
                let settings = rule
                    .get("settings")
                    .and_then(Value::as_object)
                    .ok_or("Token color rule must contain a settings object")?;
                let style = StyleModifier {
                    foreground: color_setting(settings, "foreground")?,
                    background: color_setting(settings, "background")?,
                    font_style: settings
                        .get("fontStyle")
                        .map(|value| {
                            let value = value.as_str().ok_or("fontStyle must be a string")?;
                            let mut style = FontStyle::empty();
                            for word in value.split_whitespace() {
                                style |= match word {
                                    "bold" => FontStyle::BOLD,
                                    "italic" => FontStyle::ITALIC,
                                    "underline" => FontStyle::UNDERLINE,
                                    _ => FontStyle::empty(),
                                };
                            }
                            Ok::<_, crate::error::Error>(style)
                        })
                        .transpose()?,
                };
                let scopes = match rule.get("scope") {
                    None => String::new(),
                    Some(Value::String(scope)) => scope.clone(),
                    Some(Value::Array(scopes)) => scopes
                        .iter()
                        .map(|scope| scope.as_str().ok_or("Token scopes must be strings"))
                        .collect::<std::result::Result<Vec<_>, _>>()?
                        .join(", "),
                    _ => return Err("Token scope must be a string or array".into()),
                };
                if scopes.trim().is_empty() {
                    if style.foreground.is_some() {
                        theme.settings.foreground = style.foreground;
                    }
                    if style.background.is_some() {
                        theme.settings.background = style.background;
                    }
                } else {
                    theme.scopes.push(ThemeItem {
                        scope: scopes
                            .parse()
                            .map_err(|error| format!("Invalid scope selector: {error}"))?,
                        style,
                    });
                }
            }
        }
    }
    Ok(Some(theme))
}

fn color_setting(object: &Map<String, Value>, key: &str) -> Result<Option<Color>> {
    match object.get(key) {
        None | Some(Value::Null) => Ok(None),
        Some(Value::String(value)) => parse_color(value)
            .map(Some)
            .map_err(|error| format!("Invalid color for '{key}': {error}").into()),
        _ => Err(format!("Color '{key}' must be a hexadecimal string").into()),
    }
}

fn parse_color(value: &str) -> Result<Color> {
    let digits = value
        .strip_prefix('#')
        .ok_or("Expected a hexadecimal color starting with #")?;
    if ![3, 4, 6, 8].contains(&digits.len()) || !digits.bytes().all(|byte| byte.is_ascii_hexdigit())
    {
        return Err(format!(
            "Unsupported color '{value}' (expected #RGB, #RGBA, #RRGGBB, or #RRGGBBAA)"
        )
        .into());
    }
    let mut rgba = [0, 0, 0, 255];
    if digits.len() <= 4 {
        for (slot, digit) in rgba.iter_mut().zip(digits.chars()) {
            *slot = digit.to_digit(16).unwrap() as u8 * 17;
        }
    } else {
        for (slot, pair) in rgba.iter_mut().zip(digits.as_bytes().as_chunks::<2>().0) {
            *slot = u8::from_str_radix(std::str::from_utf8(pair).unwrap(), 16).unwrap();
        }
    }
    Ok(Color {
        r: rgba[0],
        g: rgba[1],
        b: rgba[2],
        a: rgba[3],
    })
}

/// Replace comments and trailing commas with spaces, preserving string contents
/// and line numbers so JSON parse errors still identify the original location.
fn jsonc(text: &str) -> Result<String> {
    let mut bytes = text.trim_start_matches('\u{feff}').as_bytes().to_vec();
    let mut i = 0;
    let mut string = false;
    while i < bytes.len() {
        match bytes[i] {
            b'\\' if string => {
                i += 2;
                continue;
            }
            b'"' => string = !string,
            b'/' if !string && bytes.get(i + 1) == Some(&b'/') => {
                while i < bytes.len() && !matches!(bytes[i], b'\r' | b'\n') {
                    bytes[i] = b' ';
                    i += 1;
                }
                continue;
            }
            b'/' if !string && bytes.get(i + 1) == Some(&b'*') => {
                bytes[i] = b' ';
                bytes[i + 1] = b' ';
                i += 2;
                while i + 1 < bytes.len() && !(bytes[i] == b'*' && bytes[i + 1] == b'/') {
                    if !matches!(bytes[i], b'\r' | b'\n') {
                        bytes[i] = b' ';
                    }
                    i += 1;
                }
                if i + 1 >= bytes.len() {
                    return Err("Unterminated JSON comment".into());
                }
                bytes[i] = b' ';
                bytes[i + 1] = b' ';
                i += 2;
                continue;
            }
            _ => {}
        }
        i += 1;
    }
    i = 0;
    string = false;
    while i < bytes.len() {
        match bytes[i] {
            b'\\' if string => {
                i += 2;
                continue;
            }
            b'"' => string = !string,
            b',' if !string => {
                let next = bytes[i + 1..]
                    .iter()
                    .find(|byte| !byte.is_ascii_whitespace());
                if matches!(next, Some(b']' | b'}')) {
                    bytes[i] = b' ';
                }
            }
            _ => {}
        }
        i += 1;
    }
    String::from_utf8(bytes).map_err(|error| error.to_string().into())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn json_comments_and_trailing_commas_preserve_strings() {
        let input = r#"{
            // A line comment
            "url": "https://example.test/a/*literal*/",
            "quote": "a\"//b", /* block comment */
            "unicode": "日本語", "values": [1, 2,],
        }"#;
        let output = jsonc(input).unwrap();
        assert_eq!(input.lines().count(), output.lines().count());
        let value: Value = serde_json::from_str(&output).unwrap();
        assert_eq!(value["url"], "https://example.test/a/*literal*/");
        assert_eq!(value["quote"], "a\"//b");
        assert_eq!(value["unicode"], "日本語");
        assert_eq!(value["values"], serde_json::json!([1, 2]));
        assert!(jsonc("{/* unclosed").is_err());
    }

    #[test]
    fn hexadecimal_colors_accept_supported_lengths_only() {
        for (text, color) in [
            (
                "#abc",
                Color {
                    r: 170,
                    g: 187,
                    b: 204,
                    a: 255,
                },
            ),
            (
                "#AbCd",
                Color {
                    r: 170,
                    g: 187,
                    b: 204,
                    a: 221,
                },
            ),
            (
                "#123456",
                Color {
                    r: 18,
                    g: 52,
                    b: 86,
                    a: 255,
                },
            ),
            (
                "#12345678",
                Color {
                    r: 18,
                    g: 52,
                    b: 86,
                    a: 120,
                },
            ),
        ] {
            assert_eq!(parse_color(text).unwrap(), color);
        }
        for text in ["abc", "#12", "#12345", "#00000g", "#日本語", ""] {
            assert!(parse_color(text).is_err(), "{text}");
        }
    }

    #[test]
    fn includes_and_scope_rules_merge_without_resetting_missing_styles() {
        let directory = tempfile::tempdir().unwrap();
        fs::write(directory.path().join("base.json"), r##"{
            "colors": {"editor.foreground":"#aaaaaa", "editor.background":"#111111"},
            "tokenColors":[{"scope":"comment", "settings":{"foreground":"#ff0000", "fontStyle":"italic"}}]
        }"##).unwrap();
        let path = directory.path().join("child.jsonc");
        fs::write(
            &path,
            r##"{
            "include":"base.json",
            "colors":{"editor.foreground":"#bbbbbb", "editorLineNumber.foreground":"#123456"},
            "tokenColors":[
                {"scope":["comment", "string"], "settings":{"foreground":"#00ff00"}},
                {"scope":"comment.line", "settings":{"fontStyle":""}},
            ],
            "semanticTokenColors":{"variable.readonly":"#111111"},
        }"##,
        )
        .unwrap();
        let theme = load(&path, &mut Vec::new()).unwrap().unwrap();
        assert_eq!(
            theme.settings.foreground,
            Some(parse_color("#bbbbbb").unwrap())
        );
        assert_eq!(
            theme.settings.background,
            Some(parse_color("#111111").unwrap())
        );
        assert_eq!(
            theme.settings.gutter_foreground,
            Some(parse_color("#123456").unwrap())
        );
        assert_eq!(theme.scopes.len(), 3);
        assert_eq!(theme.scopes[0].style.font_style, Some(FontStyle::ITALIC));
        assert_eq!(theme.scopes[1].style.font_style, None);
        assert_eq!(theme.scopes[2].style.font_style, Some(FontStyle::empty()));
    }

    #[test]
    fn cyclic_includes_and_malformed_rules_fail_with_a_source_path() {
        let directory = tempfile::tempdir().unwrap();
        let path = directory.path().join("theme.json");
        for text in [
            r#"{"include":"theme.json"}"#,
            r#"{"include":"missing.json"}"#,
            r#"{"tokenColors":[{"scope":1,"settings":{}}]}"#,
            r#"{"colors":{"editor.foreground":false}}"#,
        ] {
            fs::write(&path, text).unwrap();
            assert!(load(&path, &mut Vec::new())
                .unwrap_err()
                .to_string()
                .contains("theme.json"));
        }
    }
}

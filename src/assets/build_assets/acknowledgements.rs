use std::fmt::Write;
use std::fs::read_to_string;
use std::path::{Path, PathBuf};

use walkdir::DirEntry;

use crate::error::*;

struct PathAndStem {
    path: PathBuf,
    stem: String,
    relative_path: String,
}

/// Looks for LICENSE and NOTICE files in `source_dir`, does some rudimentary
/// analysis, and compiles them together in a single string that is meant to be
/// used in the output to `--acknowledgements`
pub fn build_acknowledgements(
    source_dir: &Path,
    include_acknowledgements: bool,
) -> Result<Option<String>> {
    if !include_acknowledgements {
        return Ok(None);
    }

    let mut acknowledgements = format!("{}\n\n", include_str!("../../../NOTICE"));

    // Sort entries so the order is stable over time
    let entries = walkdir::WalkDir::new(source_dir).sort_by(|a, b| a.path().cmp(b.path()));
    for path_and_stem in entries
        .into_iter()
        .flatten()
        .flat_map(|entry| to_path_and_stem(source_dir, entry))
    {
        if let Some(license_text) = handle_file(&path_and_stem)? {
            append_to_acknowledgements(
                &mut acknowledgements,
                &path_and_stem.relative_path,
                &license_text,
            )
        }
    }

    Ok(Some(acknowledgements))
}

fn to_path_and_stem(source_dir: &Path, entry: DirEntry) -> Option<PathAndStem> {
    let path = entry.path();

    Some(PathAndStem {
        path: path.to_owned(),
        stem: path.file_stem().map(|s| s.to_string_lossy().to_string())?,
        relative_path: path
            .strip_prefix(source_dir)
            .map(|p| p.to_string_lossy().to_string())
            .ok()?,
    })
}

fn handle_file(path_and_stem: &PathAndStem) -> Result<Option<String>> {
    if path_and_stem.stem == "NOTICE" {
        handle_notice(&path_and_stem.path)
    } else if path_and_stem.stem.eq_ignore_ascii_case("LICENSE") {
        handle_license(&path_and_stem.path)
    } else {
        Ok(None)
    }
}

fn handle_notice(path: &Path) -> Result<Option<String>> {
    // Assume NOTICE as defined by Apache License 2.0. These must be part of acknowledgements.
    Ok(Some(read_to_string(path)?))
}

fn handle_license(path: &Path) -> Result<Option<String>> {
    let license_text = read_to_string(path)?;

    if include_license_in_acknowledgments(&license_text) {
        Ok(Some(license_text))
    } else if license_not_needed_in_acknowledgements(&license_text) {
        Ok(None)
    } else {
        Err(format!("ERROR: License is of unknown type: {path:?}").into())
    }
}

fn include_license_in_acknowledgments(license_text: &str) -> bool {
    let markers = vec![
        // MIT
        "The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.",

        // BSD
        "Redistributions in binary form must reproduce the above copyright notice,",

        // Apache 2.0
        "Apache License Version 2.0, January 2004 http://www.apache.org/licenses/",
        "Licensed under the Apache License, Version 2.0 (the \"License\");",

        // CC BY 4.0
        "Creative Commons Attribution 4.0 International Public License",
    ];

    license_contains_marker(license_text, &markers)
}

fn license_not_needed_in_acknowledgements(license_text: &str) -> bool {
    let markers = vec![
        // Public domain
        "This is free and unencumbered software released into the public domain.",

        // Public domain with stronger wording than above
        "DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE",

        // Special license of assets/syntaxes/01_Packages/LICENSE
        "Permission to copy, use, modify, sell and distribute this software is granted. This software is provided \"as is\" without express or implied warranty, and with no claim as to its suitability for any purpose."
    ];

    license_contains_marker(license_text, &markers)
}

fn license_contains_marker(license_text: &str, markers: &[&str]) -> bool {
    let normalized_license_text = normalize_license_text(license_text);
    markers.iter().any(|m| normalized_license_text.contains(m))
}

fn append_to_acknowledgements(
    acknowledgements: &mut String,
    relative_path: &str,
    license_text: &str,
) {
    write!(acknowledgements, "## {relative_path}\n\n{license_text}").ok();

    // Make sure the last char is a newline to not mess up formatting later
    if acknowledgements
        .chars()
        .last()
        .expect("acknowledgements is not the empty string")
        != '\n'
    {
        acknowledgements.push('\n');
    }

    // Add two more newlines to make it easy to distinguish where this text ends
    // and the next starts
    acknowledgements.push_str("\n\n");
}

/// Replaces newlines with a space character, and replaces multiple spaces with one space.
/// This makes the text easier to analyze.
fn normalize_license_text(license_text: &str) -> String {
    use regex::Regex;

    let whitespace_and_newlines = Regex::new(r"\s").unwrap();
    let as_single_line = whitespace_and_newlines.replace_all(license_text, " ");

    let many_spaces = Regex::new(" +").unwrap();
    many_spaces.replace_all(&as_single_line, " ").to_string()
}

#[cfg(test)]
mod tests {
    #[cfg(test)]
    use super::*;

    #[test]
    fn test_normalize_license_text() {
        let license_text = "This is a license text with these terms:
 * Complicated multi-line
   term with indentation";

        assert_eq!(
            "This is a license text with these terms: * Complicated multi-line term with indentation".to_owned(),
            normalize_license_text(license_text),
        );
    }

    #[test]
    fn test_normalize_license_text_with_windows_line_endings() {
        let license_text = "This license text includes windows line endings\r
and we need to handle that.";

        assert_eq!(
            "This license text includes windows line endings and we need to handle that."
                .to_owned(),
            normalize_license_text(license_text),
        );
    }

    #[test]
    fn test_append_to_acknowledgements_adds_newline_if_missing() {
        let mut acknowledgements = "preamble\n\n\n".to_owned();

        append_to_acknowledgements(&mut acknowledgements, "some/path", "line without newline");
        assert_eq!(
            "preamble


## some/path

line without newline


",
            acknowledgements
        );

        append_to_acknowledgements(&mut acknowledgements, "another/path", "line with newline\n");
        assert_eq!(
            "preamble


## some/path

line without newline


## another/path

line with newline


",
            acknowledgements
        );
    }
}

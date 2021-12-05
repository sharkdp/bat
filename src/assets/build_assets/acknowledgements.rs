use std::fs::read_to_string;
use std::path::Path;

use crate::error::*;

// Sourced from the License section in the README.md
const PREAMBLE: &str = "Copyright (c) 2018-2021 bat-developers (https://github.com/sharkdp/bat).

bat is made available under the terms of either the MIT License or the Apache
License 2.0, at your option.

See the LICENSE-APACHE and LICENSE-MIT files for license details.
";

/// Looks for LICENSE and NOTICE files in `source_dir`, does some rudimentary
/// analysis, and compiles them together in a single string that is meant to be
/// used in the output to `--acknowledgements`
pub fn build_acknowledgements(
    source_dir: &Path,
    include_integrated_assets: bool,
) -> Result<Option<String>> {
    if include_integrated_assets {
        return Ok(None);
    }

    let mut acknowledgements = String::new();
    acknowledgements.push_str(PREAMBLE);

    // Sort entries so the order is stable over time
    let entries = walkdir::WalkDir::new(source_dir).sort_by(|a, b| a.path().cmp(b.path()));
    for entry in entries {
        let entry = match entry {
            Ok(entry) => entry,
            Err(_) => continue,
        };

        let path = entry.path();
        let stem = match path.file_stem().and_then(|s| s.to_str()) {
            Some(stem) => stem,
            None => continue,
        };

        handle_file(&mut acknowledgements, path, stem)?
    }

    Ok(Some(acknowledgements))
}

fn handle_file(acknowledgements: &mut String, path: &Path, stem: &str) -> Result<()> {
    if stem == "NOTICE" {
        handle_notice(acknowledgements, path)?;
    } else if stem.to_ascii_uppercase() == "LICENSE" {
        handle_license(acknowledgements, path)?;
    }

    Ok(())
}

fn handle_notice(acknowledgements: &mut String, path: &Path) -> Result<()> {
    // Assume NOTICE as defined by Apache License 2.0. These must be part of acknowledgements.
    let license_text = read_to_string(path)?;
    append_to_acknowledgements(acknowledgements, &license_text)
}

fn handle_license(acknowledgements: &mut String, path: &Path) -> Result<()> {
    let license_text = read_to_string(path)?;

    if include_license_in_acknowledgments(&license_text) {
        append_to_acknowledgements(acknowledgements, &license_text)
    } else if license_not_needed_in_acknowledgements(&license_text) {
        Ok(())
    } else {
        Err(format!("ERROR: License is of unknown type: {:?}", path).into())
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
    ];

    license_contains_marker(license_text, &markers)
}

fn license_not_needed_in_acknowledgements(license_text: &str) -> bool {
    let markers = vec![
        // Public domain
        "This is free and unencumbered software released into the public domain.",

        // Special license of assets/syntaxes/01_Packages/LICENSE
        "Permission to copy, use, modify, sell and distribute this software is granted. This software is provided \"as is\" without express or implied warranty, and with no claim as to its suitability for any purpose."
    ];

    license_contains_marker(license_text, &markers)
}

fn license_contains_marker(license_text: &str, markers: &[&str]) -> bool {
    let normalized_license_text = normalize_license_text(license_text);
    for marker in markers {
        if normalized_license_text.contains(marker) {
            return true;
        }
    }
    false
}

fn append_to_acknowledgements(acknowledgements: &mut String, license_text: &str) -> Result<()> {
    // Most license texts wrap at 80 chars so our horizontal divider is 80 chars
    acknowledgements.push_str(
        "――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――\n",
    );

    // Now add the license text itself
    acknowledgements.push_str(license_text);

    // Make sure the last char is a newline to not mess up formatting later
    if acknowledgements
        .chars()
        .last()
        .expect("acknowledgements is not the empty string")
        != '\n'
    {
        acknowledgements.push('\n');
    }

    Ok(())
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
        let mut acknowledgements = "preamble\n".to_owned();

        append_to_acknowledgements(&mut acknowledgements, "line without newline").unwrap();
        assert_eq!(
            "preamble
――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――
line without newline
",
            acknowledgements
        );

        append_to_acknowledgements(&mut acknowledgements, "line with newline\n").unwrap();
        assert_eq!(
            "preamble
――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――
line without newline
――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――
line with newline
",
            acknowledgements
        );
    }
}

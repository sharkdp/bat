use std::ffi::OsStr;
use std::path::Path;

use crate::error::*;

const IGNORED_SUFFIXES: [&str; 13] = [
    // Editor etc backups
    "~",
    ".bak",
    ".old",
    ".orig",
    // Debian and derivatives apt/dpkg/ucf backups
    ".dpkg-dist",
    ".dpkg-old",
    ".ucf-dist",
    ".ucf-new",
    ".ucf-old",
    // Red Hat and derivatives rpm backups
    ".rpmnew",
    ".rpmorig",
    ".rpmsave",
    // Build system input/template files
    ".in",
];

#[derive(Debug, Clone, Default)]
pub struct IgnoredSuffixes {
    values: Vec<String>,
}

impl IgnoredSuffixes {
    pub fn new(values: Vec<String>) -> Self {
        IgnoredSuffixes { values }
    }

    /// If we find an ignored suffix on the file name, e.g. '~', we strip it and
    /// then try again without it.
    pub fn try_with_stripped_suffix<'a, T, F>(
        &self,
        file_name: &'a OsStr,
        func: F,
    ) -> Result<Option<T>>
    where
        F: Fn(&'a OsStr) -> Result<Option<T>>,
    {
        let mut from_stripped = None;
        if let Some(file_str) = Path::new(file_name).to_str() {
            for suffix in self
                .values
                .iter()
                .map(|v| v.as_str())
                .chain(IGNORED_SUFFIXES.iter().copied())
            {
                if let Some(stripped_filename) = file_str.strip_suffix(suffix) {
                    from_stripped = func(OsStr::new(stripped_filename))?;
                    break;
                }
            }
        }
        Ok(from_stripped)
    }
}

#[test]
fn internal_suffixes() {
    let ignored_suffixes = IgnoredSuffixes::new(vec![]);

    let file_names = IGNORED_SUFFIXES
        .iter()
        .map(|suffix| format!("test.json{}", suffix));
    for file_name_str in file_names {
        let file_name = OsStr::new(&file_name_str);
        let expected_stripped_file_name = OsStr::new("test.json");
        let stripped_file_name = ignored_suffixes
            .try_with_stripped_suffix(file_name, |stripped_file_name| Ok(Some(stripped_file_name)));
        assert_eq!(
            expected_stripped_file_name,
            stripped_file_name.unwrap().unwrap()
        );
    }
}

#[test]
fn external_suffixes() {
    let ignored_suffixes = IgnoredSuffixes::new(vec![
        String::from(".development"),
        String::from(".production"),
    ]);

    let file_names = ignored_suffixes
        .values
        .iter()
        .map(|suffix| format!("test.json{}", suffix));
    for file_name_str in file_names {
        let file_name = OsStr::new(&file_name_str);
        let expected_stripped_file_name = OsStr::new("test.json");
        let stripped_file_name = ignored_suffixes
            .try_with_stripped_suffix(file_name, |stripped_file_name| Ok(Some(stripped_file_name)));
        assert_eq!(
            expected_stripped_file_name,
            stripped_file_name.unwrap().unwrap()
        );
    }
}

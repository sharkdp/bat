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

/// If we find an ignored suffix on the file name, e.g. '~', we strip it and
/// then try again without it. Note that we do this recursively.
pub fn try_with_stripped_suffix<T, F>(file_name: &OsStr, func: F) -> Result<Option<T>>
where
    F: Fn(&OsStr) -> Result<Option<T>>,
{
    let file_path = Path::new(file_name);
    let mut syntax = None;
    if let Some(file_str) = file_path.to_str() {
        for suffix in &IGNORED_SUFFIXES {
            if let Some(stripped_filename) = file_str.strip_suffix(suffix) {
                syntax = func(OsStr::new(stripped_filename))?;
                break;
            }
        }
    }
    Ok(syntax)
}

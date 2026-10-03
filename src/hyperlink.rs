//! Opt-in OSC 8 links. Paths are percent-encoded before entering a terminal sequence.

use std::path::Path;

use crate::error::{Error, Result};

#[derive(Debug, Clone)]
pub struct Hyperlink {
    format: String,
    pub highlighted_only: bool,
}

impl Hyperlink {
    /// Construct a URI template using `{path}` and optionally `{line}`.
    /// `file://{path}` opens the file; editor-specific templates can include line numbers.
    pub fn new(format: &str, highlighted_only: bool) -> Result<Self> {
        let invalid = || {
            Error::Msg(
                "Invalid hyperlink format: expected an ASCII URI with {path} and optional {line}"
                    .into(),
            )
        };
        if !format.is_ascii()
            || format
                .bytes()
                .any(|b| b.is_ascii_control() || b.is_ascii_whitespace())
        {
            return Err(invalid());
        }
        let (scheme, _) = format.split_once(':').ok_or_else(invalid)?;
        if !scheme.starts_with(|c: char| c.is_ascii_alphabetic())
            || !scheme
                .bytes()
                .all(|b| b.is_ascii_alphanumeric() || matches!(b, b'+' | b'-' | b'.'))
            || !format.contains("{path}")
        {
            return Err(invalid());
        }
        let remainder = format.replace("{path}", "").replace("{line}", "");
        if remainder.contains(['{', '}']) {
            return Err(invalid());
        }
        Ok(Self {
            format: format.into(),
            highlighted_only,
        })
    }

    pub(crate) fn uri(&self, encoded_path: &str, line: usize) -> String {
        // A UNC path supplies the authority in a file URI.
        let encoded_path = if self.format.starts_with("file://{path}") {
            encoded_path.strip_prefix("//").unwrap_or(encoded_path)
        } else {
            encoded_path
        };
        self.format
            .replace("{path}", encoded_path)
            .replace("{line}", &line.to_string())
    }
}

pub(crate) fn encode_path(path: &Path) -> Option<String> {
    let absolute = path_abs::PathAbs::new(path).ok()?;
    let path = absolute.as_path();
    #[cfg(unix)]
    let bytes = {
        use std::os::unix::ffi::OsStrExt;
        path.as_os_str().as_bytes().to_vec()
    };
    #[cfg(not(unix))]
    let bytes = {
        // Windows file URIs use /C:/...; forward slashes also preserve UNC paths.
        let path = path.to_str()?.replace('\\', "/");
        let path = if let Some(unc) = path.strip_prefix("//?/UNC/") {
            format!("//{unc}")
        } else {
            path.strip_prefix("//?/").unwrap_or(&path).to_owned()
        };
        if path.starts_with('/') {
            path.as_bytes().to_vec()
        } else {
            format!("/{path}").into_bytes()
        }
    };
    let mut encoded = String::new();
    use std::fmt::Write;
    for byte in bytes {
        if byte.is_ascii_alphanumeric() || matches!(byte, b'-' | b'.' | b'_' | b'~' | b'/' | b':') {
            encoded.push(byte as char);
        } else {
            write!(&mut encoded, "%{byte:02X}").ok()?;
        }
    }
    Some(encoded)
}

pub(crate) fn link(uri: &str, text: &str) -> String {
    format!("\x1b]8;;{uri}\x1b\\{text}\x1b]8;;\x1b\\")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[cfg(unix)]
    fn paths_encode_control_and_non_utf8_bytes() {
        use std::ffi::OsStr;
        use std::os::unix::ffi::OsStrExt;
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join(OsStr::from_bytes(b"name\x1b\xff\n"));
        let encoded = encode_path(&path).unwrap();
        assert!(encoded.ends_with("name%1B%FF%0A"));
        assert!(!encoded.bytes().any(|b| b.is_ascii_control()));
    }

    #[test]
    fn reject_control_bytes_and_invalid_templates() {
        for template in [
            "file://{path}\x1b\\",
            "file://{path}\n",
            "file://a b/{path}",
            "file://{unknown}",
            "{path}",
            "1bad:{path}",
            "file://{{path}",
        ] {
            assert!(Hyperlink::new(template, false).is_err(), "{template:?}");
        }
        assert_eq!(
            Hyperlink::new("vscode://file{path}:{line}", false)
                .unwrap()
                .uri("/tmp/a%20b", 42),
            "vscode://file/tmp/a%20b:42"
        );
        assert_eq!(
            Hyperlink::new("file://{path}", false)
                .unwrap()
                .uri("//server/share/file", 1),
            "file://server/share/file"
        );
    }
}

use std::{
    path::{Path, PathBuf},
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc,
    },
    thread,
};

use globset::{Candidate, GlobBuilder, GlobMatcher};
use once_cell::sync::Lazy;

use crate::error::Result;
use builtin::BUILTIN_MAPPINGS;
use ignored_suffixes::IgnoredSuffixes;

mod builtin;
pub mod ignored_suffixes;

/// Whether a glob pattern should be matched case-sensitively or case-insensitively.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Case {
    Sensitive,
    Insensitive,
}

/// On Windows, `PathAbs` and `fs::canonicalize` return verbatim paths (`\\?\C:\dir\file`,
/// `\\?\UNC\server\share\file`), which a pattern such as `C:/dir/*` would not match.
fn without_verbatim_prefix(path: &Path) -> Option<PathBuf> {
    if !cfg!(windows) {
        return None;
    }
    let path = path.to_str()?;
    match path.strip_prefix(r"\\?\UNC\") {
        Some(rest) => Some(format!(r"\\{rest}").into()),
        None => path.strip_prefix(r"\\?\").map(PathBuf::from),
    }
}

fn make_glob_matcher(from: &str, case: Case) -> Result<GlobMatcher> {
    let matcher = GlobBuilder::new(from)
        .case_insensitive(matches!(case, Case::Insensitive))
        .literal_separator(true)
        .build()?
        .compile_matcher();
    Ok(matcher)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[non_exhaustive]
pub enum MappingTarget<'a> {
    /// For mapping a path to a specific syntax.
    MapTo(&'a str),

    /// For mapping a path (typically an extension-less file name) to an unknown
    /// syntax. This typically means later using the contents of the first line
    /// of the file to determine what syntax to use.
    MapToUnknown,

    /// For mapping a file extension (e.g. `*.conf`) to an unknown syntax. This
    /// typically means later using the contents of the first line of the file
    /// to determine what syntax to use. However, if a syntax handles a file
    /// name that happens to have the given file extension (e.g. `resolv.conf`),
    /// then that association will have higher precedence, and the mapping will
    /// be ignored.
    MapExtensionToUnknown,
}

#[derive(Debug, Clone, Default)]
pub struct SyntaxMapping<'a> {
    /// User-defined mappings at run time.
    ///
    /// Rules in front have precedence.
    custom_mappings: Vec<(GlobMatcher, MappingTarget<'a>)>,

    pub(crate) ignored_suffixes: IgnoredSuffixes<'a>,

    /// A flag to halt glob matcher building, which is offloaded to another thread.
    ///
    /// We have this so that we can signal the thread to halt early when appropriate.
    halt_glob_build: Arc<AtomicBool>,
}

impl Drop for SyntaxMapping<'_> {
    fn drop(&mut self) {
        // signal the offload thread to halt early
        self.halt_glob_build.store(true, Ordering::Relaxed);
    }
}

impl<'a> SyntaxMapping<'a> {
    pub fn new() -> SyntaxMapping<'a> {
        Default::default()
    }

    /// Start a thread to build the glob matchers for all builtin mappings.
    ///
    /// The use of this function while not necessary, is useful to speed up startup
    /// times by starting this work early in parallel.
    ///
    /// The thread halts if/when `halt_glob_build` is set to true.
    pub fn start_offload_build_all(&self) {
        let halt = Arc::clone(&self.halt_glob_build);
        thread::spawn(move || {
            for (matcher, _) in BUILTIN_MAPPINGS.iter() {
                if halt.load(Ordering::Relaxed) {
                    break;
                }
                Lazy::force(matcher);
            }
        });
        // Note that this thread is not joined upon completion because there's
        // no shared resources that need synchronization to be safely dropped.
        // If we later add code into this thread that requires interesting
        // resources (e.g. IO), it would be a good idea to store the handle
        // and join it on drop.
    }

    pub fn insert(&mut self, from: &str, to: MappingTarget<'a>) -> Result<()> {
        let matcher = make_glob_matcher(from, Case::Insensitive)?;
        self.custom_mappings.push((matcher, to));
        Ok(())
    }

    /// Like [`Self::insert`], but the glob pattern is matched case-sensitively.
    pub fn insert_case_sensitive(&mut self, from: &str, to: MappingTarget<'a>) -> Result<()> {
        let matcher = make_glob_matcher(from, Case::Sensitive)?;
        self.custom_mappings.push((matcher, to));
        Ok(())
    }

    /// Returns an iterator over all mappings. User-defined mappings are listed
    /// before builtin mappings; mappings in front have higher precedence.
    ///
    /// Builtin mappings' `GlobMatcher`s are lazily compiled.
    ///
    /// Note that this function only returns mappings that are valid under the
    /// current environment. For details see [`Self::builtin_mappings`].
    pub fn all_mappings(&self) -> impl Iterator<Item = (&GlobMatcher, &MappingTarget<'a>)> {
        self.custom_mappings()
            .iter()
            .map(|(matcher, target)| (matcher, target)) // as_ref
            .chain(
                // we need a map with a closure to "do" the lifetime variance
                // see: https://discord.com/channels/273534239310479360/1120124565591425034/1170543402870382653
                // also, clippy false positive:
                // see: https://github.com/rust-lang/rust-clippy/issues/9280
                #[allow(clippy::map_identity)]
                self.builtin_mappings().map(|rule| rule),
            )
    }

    /// Returns an iterator over all valid builtin mappings. Mappings in front
    /// have higher precedence.
    ///
    /// The `GlabMatcher`s are lazily compiled.
    ///
    /// Mappings that are invalid under the current environment (i.e. rule
    /// requires environment variable(s) that is unset, or the joined string
    /// after variable(s) replacement is not a valid glob expression) are
    /// ignored.
    pub fn builtin_mappings(
        &self,
    ) -> impl Iterator<Item = (&'static GlobMatcher, &'static MappingTarget<'static>)> {
        BUILTIN_MAPPINGS
            .iter()
            .filter_map(|(matcher, target)| matcher.as_ref().map(|glob| (glob, target)))
    }

    /// Returns all user-defined mappings.
    pub fn custom_mappings(&self) -> &[(GlobMatcher, MappingTarget<'a>)] {
        &self.custom_mappings
    }

    pub fn get_syntax_for(&self, path: impl AsRef<Path>) -> Option<MappingTarget<'a>> {
        // Try matching on the file name as-is.
        let candidate = Candidate::new(&path);
        let simplified_path = without_verbatim_prefix(path.as_ref());
        let candidate_simplified = simplified_path.as_ref().map(Candidate::new);
        let candidate_filename = path.as_ref().file_name().map(Candidate::new);
        for (glob, syntax) in self.all_mappings() {
            if glob.is_match_candidate(&candidate)
                || candidate_simplified
                    .as_ref()
                    .is_some_and(|simplified| glob.is_match_candidate(simplified))
                || candidate_filename
                    .as_ref()
                    .is_some_and(|filename| glob.is_match_candidate(filename))
            {
                return Some(*syntax);
            }
        }
        // Try matching on the file name after removing an ignored suffix.
        let file_name = path.as_ref().file_name()?;
        self.ignored_suffixes
            .try_with_stripped_suffix(file_name, |stripped_file_name| {
                if Path::new(stripped_file_name).file_name().is_none() {
                    return Ok(self.get_syntax_for(stripped_file_name));
                }
                Ok(self.get_syntax_for(path.as_ref().with_file_name(stripped_file_name)))
            })
            .ok()?
    }

    pub fn insert_ignored_suffix(&mut self, suffix: &'a str) {
        self.ignored_suffixes.add_suffix(suffix);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn builtin_mappings_work() {
        let map = SyntaxMapping::new();

        assert_eq!(
            map.get_syntax_for("/path/to/build"),
            Some(MappingTarget::MapToUnknown)
        );
    }

    #[test]
    fn all_fixed_builtin_mappings_can_compile() {
        let map = SyntaxMapping::new();

        // collect call evaluates all lazy closures
        // fixed builtin mappings will panic if they fail to compile
        let _mappings = map.builtin_mappings().collect::<Vec<_>>();
    }

    #[test]
    fn builtin_mappings_matcher_only_compile_once() {
        let map = SyntaxMapping::new();

        let two_iterations: Vec<_> = (0..2)
            .map(|_| {
                // addresses of every matcher
                map.builtin_mappings()
                    .map(|(matcher, _)| matcher as *const _ as usize)
                    .collect::<Vec<_>>()
            })
            .collect();

        // if the matchers are only compiled once, their address should remain the same
        assert_eq!(two_iterations[0], two_iterations[1]);
    }

    #[test]
    fn custom_mappings_work() {
        let mut map = SyntaxMapping::new();
        map.insert("/path/to/Cargo.lock", MappingTarget::MapTo("TOML"))
            .ok();
        map.insert("/path/to/.ignore", MappingTarget::MapTo("Git Ignore"))
            .ok();

        assert_eq!(
            map.get_syntax_for("/path/to/Cargo.lock"),
            Some(MappingTarget::MapTo("TOML"))
        );
        assert_eq!(map.get_syntax_for("/path/to/other.lock"), None);

        assert_eq!(
            map.get_syntax_for("/path/to/.ignore"),
            Some(MappingTarget::MapTo("Git Ignore"))
        );
    }

    #[cfg(windows)]
    #[test]
    fn custom_mappings_match_verbatim_paths() {
        let mut map = SyntaxMapping::new();
        map.insert("C:/dir/*.myconf", MappingTarget::MapTo("INI"))
            .unwrap();
        map.insert(r"\\server\share\*.myconf", MappingTarget::MapTo("TOML"))
            .unwrap();

        for (path, expected) in [
            (r"C:\dir\file.myconf", Some(MappingTarget::MapTo("INI"))),
            (r"\\?\C:\dir\file.myconf", Some(MappingTarget::MapTo("INI"))),
            (r"\\?\D:\dir\file.myconf", None),
            (
                r"\\?\UNC\server\share\file.myconf",
                Some(MappingTarget::MapTo("TOML")),
            ),
        ] {
            assert_eq!(map.get_syntax_for(path), expected, "{path}");
        }
    }

    #[test]
    fn custom_mappings_with_ignored_suffix_preserve_paths() {
        for (pattern, parent) in [
            ("/path/to/file.demo", "/path/to"),
            ("/path/to/*.demo", "/path/to"),
            ("path/to/*.demo", "path/to"),
        ] {
            let mut map = SyntaxMapping::new();
            map.insert(pattern, MappingTarget::MapTo("JSON")).unwrap();
            map.insert_ignored_suffix(".custom");

            for suffix in ["", ".bak", ".bak.old", ".custom.custom"] {
                let path = Path::new(parent).join(format!("file.demo{suffix}"));
                assert_eq!(
                    map.get_syntax_for(&path),
                    Some(MappingTarget::MapTo("JSON")),
                    "{pattern}: {}",
                    path.display()
                );
            }
            assert_eq!(map.get_syntax_for("/path/elsewhere/file.demo.bak"), None);
        }
    }

    #[test]
    fn custom_mappings_with_ignored_suffix_preserve_precedence() {
        for (first, second) in [("/path/to/*.demo", "*.demo"), ("*.demo", "/path/to/*.demo")] {
            let mut map = SyntaxMapping::new();
            map.insert(first, MappingTarget::MapTo("first")).unwrap();
            map.insert(second, MappingTarget::MapTo("second")).unwrap();
            assert_eq!(
                map.get_syntax_for("/path/to/file.demo.bak"),
                Some(MappingTarget::MapTo("first"))
            );

            map.insert("*.bak", MappingTarget::MapTo("backup")).unwrap();
            assert_eq!(
                map.get_syntax_for("/path/to/file.demo.bak"),
                Some(MappingTarget::MapTo("backup"))
            );
        }
    }

    #[test]
    fn custom_mappings_with_empty_stripped_filename() {
        let mut map = SyntaxMapping::new();
        map.insert("to", MappingTarget::MapTo("parent")).unwrap();
        map.insert_ignored_suffix(".custom");

        for name in [
            "~", ".bak", ".custom", "~.bak", ".~", "..bak", "..custom", "...bak",
        ] {
            assert_eq!(map.get_syntax_for(Path::new("/path/to").join(name)), None);
        }

        map.insert("", MappingTarget::MapTo("empty")).unwrap();
        assert_eq!(
            map.get_syntax_for("/path/to/~.bak"),
            Some(MappingTarget::MapTo("empty"))
        );

        for (pattern, name) in [(".", ".~"), (".", "..bak"), ("..", "...bak")] {
            let mut map = SyntaxMapping::new();
            map.insert(pattern, MappingTarget::MapTo("dot")).unwrap();
            assert_eq!(
                map.get_syntax_for(Path::new("/path/to").join(name)),
                Some(MappingTarget::MapTo("dot"))
            );
        }
    }

    #[test]
    fn custom_mappings_override_builtin() {
        let mut map = SyntaxMapping::new();

        assert_eq!(
            map.get_syntax_for("/path/to/httpd.conf"),
            Some(MappingTarget::MapTo("Apache Conf"))
        );
        map.insert("httpd.conf", MappingTarget::MapTo("My Syntax"))
            .ok();
        assert_eq!(
            map.get_syntax_for("/path/to/httpd.conf"),
            Some(MappingTarget::MapTo("My Syntax"))
        );
    }

    #[test]
    fn custom_mappings_precedence() {
        let mut map = SyntaxMapping::new();

        map.insert("/path/to/foo", MappingTarget::MapTo("alpha"))
            .ok();
        map.insert("/path/to/foo", MappingTarget::MapTo("bravo"))
            .ok();
        assert_eq!(
            map.get_syntax_for("/path/to/foo"),
            Some(MappingTarget::MapTo("alpha"))
        );
    }

    #[test]
    fn case_sensitive_custom_mappings_work() {
        let mut map = SyntaxMapping::new();
        map.insert_case_sensitive("MY_SPECIAL_FILE", MappingTarget::MapTo("Python"))
            .ok();

        // Exact case matches
        assert_eq!(
            map.get_syntax_for("/path/to/MY_SPECIAL_FILE"),
            Some(MappingTarget::MapTo("Python"))
        );
        // Different case should NOT match the case-sensitive rule
        assert_eq!(map.get_syntax_for("/path/to/my_special_file"), None);
        assert_eq!(map.get_syntax_for("/path/to/My_Special_File"), None);
    }

    #[test]
    fn builtin_mappings_build_is_case_sensitive() {
        let map = SyntaxMapping::new();

        // "BUILD" (uppercase) should map to Python via case-sensitive builtin
        assert_eq!(
            map.get_syntax_for("/path/to/BUILD"),
            Some(MappingTarget::MapTo("Python"))
        );
        // "build" (lowercase) should still map to MapToUnknown
        assert_eq!(
            map.get_syntax_for("/path/to/build"),
            Some(MappingTarget::MapToUnknown)
        );
        // Mixed case should NOT match the Python rule
        assert_eq!(
            map.get_syntax_for("/path/to/Build"),
            Some(MappingTarget::MapToUnknown)
        );
    }
}

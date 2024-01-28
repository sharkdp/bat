use std::path::Path;

use globset::{Candidate, GlobBuilder, GlobMatcher};

use crate::error::Result;
use builtin::BUILTIN_MAPPINGS;
use ignored_suffixes::IgnoredSuffixes;

mod builtin;
pub mod ignored_suffixes;

fn make_glob_matcher(from: &str) -> Result<GlobMatcher> {
    let matcher = GlobBuilder::new(from)
        .case_insensitive(true)
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
}

impl<'a> SyntaxMapping<'a> {
    pub fn new() -> SyntaxMapping<'a> {
        Default::default()
    }

    pub fn insert(&mut self, from: &str, to: MappingTarget<'a>) -> Result<()> {
        let matcher = make_glob_matcher(from)?;
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
        let candidate_filename = path.as_ref().file_name().map(Candidate::new);
        for (glob, syntax) in self.all_mappings() {
            if glob.is_match_candidate(&candidate)
                || candidate_filename
                    .as_ref()
                    .map_or(false, |filename| glob.is_match_candidate(filename))
            {
                return Some(*syntax);
            }
        }
        // Try matching on the file name after removing an ignored suffix.
        let file_name = path.as_ref().file_name()?;
        self.ignored_suffixes
            .try_with_stripped_suffix(file_name, |stripped_file_name| {
                Ok(self.get_syntax_for(stripped_file_name))
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
}

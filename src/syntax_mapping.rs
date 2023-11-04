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

    /// Returns all mappings. User-defined mappings are listed before builtin
    /// mappings; mappings in front have higher precedence.
    ///
    /// Note that this function ignores builtin mappings that are invalid under
    /// the current environment (i.e. their rules require an environment
    /// variable that is unset).
    pub fn all_mappings(&self) -> Vec<(&GlobMatcher, &MappingTarget<'a>)> {
        self.custom_mappings()
            .iter()
            .map(|(matcher, target)| (matcher, target)) // as_ref
            .chain(self.builtin_mappings())
            .collect()
    }

    /// Returns all valid builtin mappings. Mappings in front have higher
    /// precedence.
    ///
    /// If a mapping rule requires an environment variable that is unset, it
    /// will be ignored.
    pub fn builtin_mappings(&self) -> Vec<(&'static GlobMatcher, &'static MappingTarget<'static>)> {
        BUILTIN_MAPPINGS
            .iter()
            .filter_map(|(matcher, target)| matcher.as_ref().map(|glob| (glob, target)))
            .collect()
    }

    /// Returns all user-defined mappings.
    pub fn custom_mappings(&self) -> &[(GlobMatcher, MappingTarget<'a>)] {
        &self.custom_mappings
    }

    pub fn get_syntax_for(&self, path: impl AsRef<Path>) -> Option<MappingTarget<'a>> {
        // Try matching on the file name as-is.
        let candidate = Candidate::new(&path);
        let candidate_filename = path.as_ref().file_name().map(Candidate::new);
        for (glob, syntax) in self.all_mappings().into_iter() {
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
    fn basic() {
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
    fn user_can_override_builtin_mappings() {
        let mut map = SyntaxMapping::new();

        assert_eq!(
            map.get_syntax_for("/etc/profile"),
            Some(MappingTarget::MapTo("Bourne Again Shell (bash)"))
        );
        map.insert("/etc/profile", MappingTarget::MapTo("My Syntax"))
            .ok();
        assert_eq!(
            map.get_syntax_for("/etc/profile"),
            Some(MappingTarget::MapTo("My Syntax"))
        );
    }

    #[test]
    fn builtin_mappings() {
        let map = SyntaxMapping::new();

        assert_eq!(
            map.get_syntax_for("/path/to/build"),
            Some(MappingTarget::MapToUnknown)
        );
    }
}

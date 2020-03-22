use std::path::Path;

use crate::errors::Result;

use globset::{Candidate, GlobBuilder, GlobMatcher};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum MappingTarget<'a> {
    MapTo(&'a str),
    MapToUnknown,
}

#[derive(Debug, Clone, Default)]
pub struct SyntaxMapping<'a> {
    mappings: Vec<(GlobMatcher, MappingTarget<'a>)>,
}

impl<'a> SyntaxMapping<'a> {
    pub fn empty() -> SyntaxMapping<'a> {
        Default::default()
    }

    pub fn builtin() -> SyntaxMapping<'a> {
        let mut mapping = Self::empty();
        mapping
            .insert("build", MappingTarget::MapToUnknown)
            .unwrap();

        mapping
    }

    pub fn insert(&mut self, from: &str, to: MappingTarget<'a>) -> Result<()> {
        let glob = GlobBuilder::new(from)
            .case_insensitive(false)
            .literal_separator(true)
            .build()?;
        self.mappings.push((glob.compile_matcher(), to));
        Ok(())
    }

    pub(crate) fn get_syntax_for(&self, path: impl AsRef<Path>) -> Option<MappingTarget<'a>> {
        let candidate = Candidate::new(path.as_ref());
        let canddidate_filename = path.as_ref().file_name().map(Candidate::new);
        for (ref glob, ref syntax) in &self.mappings {
            if glob.is_match_candidate(&candidate)
                || canddidate_filename
                    .as_ref()
                    .map_or(false, |filename| glob.is_match_candidate(filename))
            {
                return Some(*syntax);
            }
        }
        None
    }
}

#[test]
fn basic() {
    let mut map = SyntaxMapping::empty();
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
fn builtin_mappings() {
    let map = SyntaxMapping::builtin();

    assert_eq!(
        map.get_syntax_for("/path/to/build"),
        Some(MappingTarget::MapToUnknown)
    );
}

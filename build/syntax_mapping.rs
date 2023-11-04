use std::{convert::Infallible, env, fs, path::Path, str::FromStr};

use anyhow::{anyhow, bail};
use indexmap::IndexMap;
use itertools::Itertools;
use once_cell::sync::Lazy;
use regex::Regex;
use serde::Deserialize;
use serde_with::DeserializeFromStr;
use walkdir::WalkDir;

/// Known mapping targets.
///
/// Corresponds to `syntax_mapping::MappingTarget`.
#[derive(Clone, Debug, Eq, PartialEq, Hash, DeserializeFromStr)]
pub enum MappingTarget {
    MapTo(String),
    MapToUnknown,
    MapExtensionToUnknown,
}
impl FromStr for MappingTarget {
    type Err = Infallible;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "MappingTarget::MapToUnknown" => Ok(Self::MapToUnknown),
            "MappingTarget::MapExtensionToUnknown" => Ok(Self::MapExtensionToUnknown),
            syntax => Ok(Self::MapTo(syntax.into())),
        }
    }
}
impl MappingTarget {
    fn codegen(&self) -> String {
        match self {
            Self::MapTo(syntax) => format!(r###"MappingTarget::MapTo(r#"{syntax}"#)"###),
            Self::MapToUnknown => "MappingTarget::MapToUnknown".into(),
            Self::MapExtensionToUnknown => "MappingTarget::MapExtensionToUnknown".into(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, DeserializeFromStr)]
/// A single matcher.
///
/// Codegen converts this into a `Lazy<GlobMatcher>`.
struct Matcher(Vec<MatcherSegment>);
/// Parse a matcher.
///
/// Note that this implementation is rather strict: it will greedily interpret
/// every valid environment variable replacement as such, then immediately
/// hard-error if it finds a '$', '{', or '}' anywhere in the remaining text
/// segments.
///
/// The reason for this strictness is I currently cannot think of a valid reason
/// why you would ever need '$', '{', or '}' as plaintext in a glob pattern.
/// Therefore any such occurrences are likely human errors.
///
/// If we later discover some edge cases, it's okay to make it more permissive.
impl FromStr for Matcher {
    type Err = anyhow::Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use MatcherSegment as Seg;
        static VAR_REGEX: Lazy<Regex> = Lazy::new(|| Regex::new(r"\$\{([\w\d_\-]+)\}").unwrap());

        let mut segments = vec![];
        let mut text_start = 0;
        for capture in VAR_REGEX.captures_iter(s) {
            let match_0 = capture.get(0).unwrap();

            // text before this var
            let text_end = match_0.start();
            segments.push(Seg::Text(s[text_start..text_end].into()));
            text_start = match_0.end();

            // this var
            segments.push(Seg::Env(capture.get(1).unwrap().as_str().into()));
        }
        // possible trailing text
        segments.push(Seg::Text(s[text_start..].into()));

        // cleanup empty text segments
        let non_empty_segments = segments
            .into_iter()
            .filter(|seg| match seg {
                Seg::Text(t) => !t.is_empty(),
                Seg::Env(_) => true,
            })
            .collect_vec();

        if non_empty_segments.is_empty() {
            bail!(r#"Parsed an empty matcher: "{s}""#);
        }

        if non_empty_segments.iter().any(|seg| match seg {
            Seg::Text(t) => t.contains(['$', '{', '}']),
            Seg::Env(_) => false,
        }) {
            bail!(r#"Invalid matcher: "{s}""#);
        }

        Ok(Self(non_empty_segments))
    }
}
impl Matcher {
    fn codegen(&self) -> String {
        match self.0.len() {
            0 => unreachable!("0-length matcher should never be created"),
            // if-let guard would be ideal here
            // see: https://github.com/rust-lang/rust/issues/51114
            1 if matches!(self.0[0], MatcherSegment::Text(_)) => {
                let MatcherSegment::Text(ref s) = self.0[0] else {
                    unreachable!()
                };
                format!(r###"Lazy::new(|| Some(build_matcher_fixed(r#"{s}"#)))"###)
            }
            // parser logic ensures that this case can only happen when there are dynamic segments
            _ => {
                let segs = self.0.iter().map(MatcherSegment::codegen).join(", ");
                format!(r###"Lazy::new(|| build_matcher_dynamic(&[{segs}]))"###)
            }
        }
    }
}

/// A segment in a matcher.
///
/// Corresponds to `syntax_mapping::MatcherSegment`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum MatcherSegment {
    Text(String),
    Env(String),
}
impl MatcherSegment {
    fn codegen(&self) -> String {
        match self {
            Self::Text(s) => format!(r###"MatcherSegment::Text(r#"{s}"#)"###),
            Self::Env(s) => format!(r###"MatcherSegment::Env(r#"{s}"#)"###),
        }
    }
}

/// A struct that models a single .toml file in /src/syntax_mapping/builtins/.
#[derive(Clone, Debug, Deserialize)]
struct MappingDefModel {
    mappings: IndexMap<MappingTarget, Vec<Matcher>>,
}
impl MappingDefModel {
    fn into_mapping_list(self) -> MappingList {
        let list = self
            .mappings
            .into_iter()
            .flat_map(|(target, matchers)| {
                matchers
                    .into_iter()
                    .map(|matcher| (matcher, target.clone()))
                    .collect::<Vec<_>>()
            })
            .collect();
        MappingList(list)
    }
}

#[derive(Clone, Debug)]
struct MappingList(Vec<(Matcher, MappingTarget)>);
impl MappingList {
    fn codegen(&self) -> String {
        let array_items: Vec<_> = self
            .0
            .iter()
            .map(|(matcher, target)| {
                format!("({m}, {t})", m = matcher.codegen(), t = target.codegen())
            })
            .collect();
        let len = array_items.len();

        format!(
            "pub(crate) static BUILTIN_MAPPINGS: [(Lazy<Option<GlobMatcher>>, MappingTarget); {len}] = [\n{items}\n];",
            items = array_items.join(",\n")
        )
    }
}

fn read_all_mappings() -> anyhow::Result<MappingList> {
    let mut all_mappings = vec![];

    for entry in WalkDir::new("src/syntax_mapping/builtins")
        .sort_by_file_name()
        .into_iter()
        .map(|entry| entry.unwrap_or_else(|err| panic!("failed to visit a file: {err}")))
        .filter(|entry| {
            let path = entry.path();
            path.is_file() && path.extension().map(|ext| ext == "toml").unwrap_or(false)
        })
    {
        let toml_string = fs::read_to_string(entry.path())?;
        let mappings = toml::from_str::<MappingDefModel>(&toml_string)?.into_mapping_list();
        all_mappings.extend(mappings.0);
    }

    let duplicates = all_mappings
        .iter()
        .duplicates_by(|(matcher, _)| matcher)
        .collect_vec();
    if !duplicates.is_empty() {
        bail!("Rules with duplicate matchers found: {duplicates:?}");
    }

    Ok(MappingList(all_mappings))
}

/// Build the static syntax mappings defined in /src/syntax_mapping/builtins/
/// into a .rs source file, which is to be inserted with `include!`.
pub fn build_static_mappings() -> anyhow::Result<()> {
    println!("cargo:rerun-if-changed=src/syntax_mapping/builtins/");

    let mappings = read_all_mappings()?;

    let codegen_path = Path::new(&env::var_os("OUT_DIR").ok_or(anyhow!("OUT_DIR is unset"))?)
        .join("codegen_static_syntax_mappings.rs");

    fs::write(codegen_path, mappings.codegen())?;

    Ok(())
}
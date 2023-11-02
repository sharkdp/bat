use std::{convert::Infallible, env, fs, path::Path, str::FromStr};

use anyhow::{anyhow, bail};
use indexmap::IndexMap;
use itertools::Itertools;
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

#[derive(Clone, Debug, DeserializeFromStr)]
/// A single matcher.
///
/// Corresponds to `syntax_mapping::BuiltinMatcher`.
struct Matcher(Vec<MatcherSegment>);
/// Parse a matcher.
///
/// Note that this implementation is rather strict: when it sees a '$', '{', or
/// '}' where it does not make sense, it will immediately hard-error.
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

        if s.is_empty() {
            bail!("Empty string is not a valid glob matcher");
        }

        let mut segments = Vec::new();
        let mut buf = String::new();
        let mut is_in_var = false;

        let mut char_it = s.chars();
        loop {
            match char_it.next() {
                Some('$') => {
                    if is_in_var {
                        bail!(r#"Saw a '$' when already in a variable: "{s}""#);
                    }
                    match char_it.next() {
                        Some('{') => {
                            // push text unless empty
                            if !buf.is_empty() {
                                segments.push(Seg::Text(buf.clone()));
                                buf.clear();
                            }
                            // open var
                            is_in_var = true;
                        }
                        Some(_) | None => {
                            bail!(r#"Expected a '{{' after '$': "{s}""#);
                        }
                    }
                }
                Some('{') => {
                    bail!(r#"Saw a hanging '{{': "{s}""#);
                }
                Some('}') => {
                    if !is_in_var {
                        bail!(r#"Saw a '}}' when not in a variable: "{s}""#);
                    }
                    if buf.is_empty() {
                        // `${}`
                        bail!(r#"Variable name cannot be empty: "{s}""#);
                    }
                    // push variable
                    segments.push(Seg::Env(buf.clone()));
                    buf.clear();
                    // close var
                    is_in_var = false;
                }
                Some(' ') if is_in_var => {
                    bail!(r#"' ' Cannot be part of a variable's name: "{s}""#);
                }
                Some(c) => {
                    // either plaintext or variable name
                    buf.push(c);
                }
                None => {
                    if is_in_var {
                        bail!(r#"Variable unclosed: "{s}""#);
                    }
                    segments.push(Seg::Text(buf.clone()));
                    break;
                }
            }
        }

        Ok(Self(segments))
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
                format!(r###"BuiltinMatcher::Fixed(r#"{s}"#)"###)
            }
            // parser logic ensures that this case can only happen when there are dynamic segments
            _ => {
                let segments_codegen = self.0.iter().map(MatcherSegment::codegen).join(", ");
                let closure = format!("|| build_glob_string(&[{segments_codegen}])");
                format!("BuiltinMatcher::Dynamic(Lazy::new({closure}))")
            }
        }
    }
}

/// A segment in a matcher.
///
/// Corresponds to `syntax_mapping::MatcherSegment`.
#[derive(Debug, Clone)]
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
            "static STATIC_RULES: [(BuiltinMatcher, MappingTarget); {len}] = [\n{items}\n];",
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

    Ok(MappingList(all_mappings))
}

/// Build the static syntax mappings defined in /src/syntax_mapping/builtins/
/// into a .rs source file, which is to be inserted with `include!`.
pub fn build_static_mappings() -> anyhow::Result<()> {
    let mappings = read_all_mappings()?;

    let codegen_path = Path::new(&env::var_os("OUT_DIR").ok_or(anyhow!("OUT_DIR is unset"))?)
        .join("codegen_static_syntax_mappings.rs");

    fs::write(codegen_path, mappings.codegen())?;

    Ok(())
}

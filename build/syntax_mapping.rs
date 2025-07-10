use std::{
    convert::Infallible,
    env, fs,
    path::{Path, PathBuf},
    str::FromStr,
};

use anyhow::{anyhow, bail};
use indexmap::IndexMap;
use itertools::Itertools;
use once_cell::sync::Lazy;
use proc_macro2::TokenStream;
use quote::{quote, ToTokens, TokenStreamExt};
use regex::Regex;
use serde_derive::Deserialize;
use serde_with::DeserializeFromStr;
use walkdir::WalkDir;

/// Known mapping targets.
///
/// Corresponds to `syntax_mapping::MappingTarget`.
#[allow(clippy::enum_variant_names)]
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
impl ToTokens for MappingTarget {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let t = match self {
            Self::MapTo(syntax) => quote! { MappingTarget::MapTo(#syntax) },
            Self::MapToUnknown => quote! { MappingTarget::MapToUnknown },
            Self::MapExtensionToUnknown => quote! { MappingTarget::MapExtensionToUnknown },
        };
        tokens.append_all(t);
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, DeserializeFromStr)]
/// A single matcher.
///
/// Codegen converts this into a `Lazy<Option<GlobMatcher>>`.
struct Matcher(Vec<MatcherSegment>);
/// Parse a matcher.
///
/// Note that this implementation is rather strict: it will greedily interpret
/// every valid environment variable replacement as such, then immediately
/// hard-error if it finds a '$' anywhere in the remaining text segments.
///
/// The reason for this strictness is I currently cannot think of a valid reason
/// why you would ever need '$' as plaintext in a glob pattern. Therefore any
/// such occurrences are likely human errors.
///
/// If we later discover some edge cases, it's okay to make it more permissive.
///
/// Revision history:
/// - 2024-02-20: allow `{` and `}` (glob brace expansion)
impl FromStr for Matcher {
    type Err = anyhow::Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use MatcherSegment as Seg;
        static VAR_REGEX: Lazy<Regex> = Lazy::new(|| Regex::new(r"\$\{([\w\d_]+)\}").unwrap());

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
            .filter(|seg| seg.text().map(|t| !t.is_empty()).unwrap_or(true))
            .collect_vec();

        // sanity check
        if non_empty_segments
            .windows(2)
            .any(|segs| segs[0].is_text() && segs[1].is_text())
        {
            unreachable!("Parsed into consecutive text segments: {non_empty_segments:?}");
        }

        // guard empty case
        if non_empty_segments.is_empty() {
            bail!(r#"Parsed an empty matcher: "{s}""#);
        }

        // guard variable syntax leftover fragments
        if non_empty_segments
            .iter()
            .filter_map(Seg::text)
            .any(|t| t.contains('$'))
        {
            bail!(r#"Invalid matcher: "{s}""#);
        }

        Ok(Self(non_empty_segments))
    }
}
impl ToTokens for Matcher {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let t = match self.0.as_slice() {
            [] => unreachable!("0-length matcher should never be created"),
            [MatcherSegment::Text(text)] => {
                quote! { Lazy::new(|| Some(build_matcher_fixed(#text))) }
            }
            // parser logic ensures that this case can only happen when there are dynamic segments
            segs @ [_, ..] => quote! { Lazy::new(|| build_matcher_dynamic(&[ #(#segs),* ])) },
        };
        tokens.append_all(t);
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
impl ToTokens for MatcherSegment {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let t = match self {
            Self::Text(text) => quote! { MatcherSegment::Text(#text) },
            Self::Env(env) => quote! { MatcherSegment::Env(#env) },
        };
        tokens.append_all(t);
    }
}
#[allow(dead_code)]
impl MatcherSegment {
    fn is_text(&self) -> bool {
        matches!(self, Self::Text(_))
    }
    fn is_env(&self) -> bool {
        matches!(self, Self::Env(_))
    }
    fn text(&self) -> Option<&str> {
        match self {
            Self::Text(t) => Some(t),
            Self::Env(_) => None,
        }
    }
    fn env(&self) -> Option<&str> {
        match self {
            Self::Text(_) => None,
            Self::Env(t) => Some(t),
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
impl ToTokens for MappingList {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let len = self.0.len();
        let array_items = self
            .0
            .iter()
            .map(|(matcher, target)| quote! { (#matcher, #target) });

        let t = quote! {
            /// Generated by build script from /src/syntax_mapping/builtins/.
            pub(crate) static BUILTIN_MAPPINGS: [(Lazy<Option<GlobMatcher>>, MappingTarget); #len] = [#(#array_items),*];
        };
        tokens.append_all(t);
    }
}

/// Get the list of paths to all mapping definition files that should be
/// included for the current target platform.
fn get_def_paths() -> anyhow::Result<Vec<PathBuf>> {
    let source_subdirs = [
        "common",
        #[cfg(target_family = "unix")]
        "unix-family",
        #[cfg(any(
            target_os = "freebsd",
            target_os = "netbsd",
            target_os = "openbsd",
            target_os = "macos"
        ))]
        "bsd-family",
        #[cfg(target_os = "linux")]
        "linux",
        #[cfg(target_os = "macos")]
        "macos",
        #[cfg(target_os = "windows")]
        "windows",
    ];

    let mut toml_paths = vec![];
    for subdir_name in source_subdirs {
        let subdir = Path::new("src/syntax_mapping/builtins").join(subdir_name);
        if !subdir.try_exists()? {
            // Directory might not exist due to this `cargo vendor` bug:
            // https://github.com/rust-lang/cargo/issues/15080
            continue;
        }
        let wd = WalkDir::new(subdir);
        let paths = wd
            .into_iter()
            .filter_map_ok(|entry| {
                let path = entry.path();
                (path.is_file() && path.extension().map(|ext| ext == "toml").unwrap_or(false))
                    .then(|| path.to_owned())
            })
            .collect::<Result<Vec<_>, _>>()?;
        toml_paths.extend(paths);
    }

    toml_paths.sort_by_key(|path| {
        path.file_name()
            .expect("file name should not terminate in ..")
            .to_owned()
    });

    Ok(toml_paths)
}

fn read_all_mappings() -> anyhow::Result<MappingList> {
    let mut all_mappings = vec![];

    for path in get_def_paths()? {
        let toml_string = fs::read_to_string(path)?;
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

    // IMPRV: parse + unparse is a bit cringe, but there seems to be no better
    // option given the limited APIs of `prettyplease`
    let rs_src = syn::parse_file(&mappings.to_token_stream().to_string())?;
    let rs_src_pretty = prettyplease::unparse(&rs_src);

    let codegen_path = Path::new(&env::var_os("OUT_DIR").ok_or(anyhow!("OUT_DIR is unset"))?)
        .join("codegen_static_syntax_mappings.rs");

    fs::write(codegen_path, rs_src_pretty)?;

    Ok(())
}

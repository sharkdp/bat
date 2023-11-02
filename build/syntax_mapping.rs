use std::{convert::Infallible, env, fs, path::Path, str::FromStr};

use anyhow::anyhow;
use indexmap::IndexMap;
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

/// A struct that models a single .toml file in /src/syntax_mapping/builtins/.
#[derive(Clone, Debug, Deserialize)]
struct MappingDefModel {
    mappings: IndexMap<MappingTarget, Vec<String>>,
}
impl MappingDefModel {
    fn into_mapping_list(self) -> MappingList {
        let list = self
            .mappings
            .into_iter()
            .flat_map(|(target, matcher)| {
                matcher
                    .into_iter()
                    .map(|rule| (rule, target.clone()))
                    .collect::<Vec<_>>()
            })
            .collect();
        MappingList(list)
    }
}

#[derive(Clone, Debug)]
struct MappingList(Vec<(String, MappingTarget)>);
impl MappingList {
    fn codegen(&self) -> String {
        let array_items: Vec<_> = self
            .0
            .iter()
            .map(|(matcher, target)| format!(r###"(r#"{matcher}"#, {t})"###, t = target.codegen()))
            .collect();
        let len = array_items.len();

        format!(
            "static STATIC_RULES: [(&'static str, &'static str); {len}] = [\n{items}\n];",
            items = array_items.join(",\n")
        )
    }
}

fn read_all_mappings() -> anyhow::Result<MappingList> {
    let mut all_mappings = vec![];

    for entry in WalkDir::new("src/syntax_mapping/builtins")
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

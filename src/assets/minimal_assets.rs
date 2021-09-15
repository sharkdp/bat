use std::collections::HashMap;

use lazycell::LazyCell;

use syntect::parsing::SyntaxSet;

use super::*;

#[derive(Debug)]
pub(crate) struct MinimalAssets {
    minimal_syntaxes: MinimalSyntaxes,

    /// Lazily load serialized [SyntaxSet]s from [Self.minimal_syntaxes]. The
    /// index in this vec matches the index in
    /// [Self.minimal_syntaxes.serialized_syntax_sets]
    deserialized_minimal_syntaxes: Vec<LazyCell<SyntaxSet>>,
}

/// Stores and allows lookup of minimal [SyntaxSet]s. The [SyntaxSet]s are
/// stored in serialized form, and are deserialized on-demand. This gives good
/// startup performance since only the necessary [SyntaxReference]s needs to be
/// deserialized.
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub(crate) struct MinimalSyntaxes {
    /// Lookup the index into `serialized_syntax_sets` of a [SyntaxSet] by the
    /// name of any [SyntaxReference] inside the [SyntaxSet]
    /// (We will later add `by_extension`, `by_first_line`, etc.)
    pub(crate) by_name: HashMap<String, usize>,

    /// Serialized [SyntaxSet]s. Whether or not this data is compressed is
    /// decided by [COMPRESS_SERIALIZED_MINIMAL_SYNTAXES]
    pub(crate) serialized_syntax_sets: Vec<Vec<u8>>,
}

impl MinimalAssets {
    pub(crate) fn new(minimal_syntaxes: MinimalSyntaxes) -> Self {
        // Prepare so we can lazily load minimal syntaxes without a mut reference
        let deserialized_minimal_syntaxes =
            vec![LazyCell::new(); minimal_syntaxes.serialized_syntax_sets.len()];

        Self {
            minimal_syntaxes,
            deserialized_minimal_syntaxes,
        }
    }

    pub fn get_syntax_set_by_name(&self, name: &str) -> Option<&SyntaxSet> {
        self.minimal_syntaxes
            .by_name
            .get(&name.to_ascii_lowercase())
            .and_then(|index| self.get_minimal_syntax_set_with_index(*index))
    }

    fn load_minimal_syntax_set_with_index(&self, index: usize) -> Result<SyntaxSet> {
        let serialized_syntax_set = &self.minimal_syntaxes.serialized_syntax_sets[index];
        asset_from_contents(
            &serialized_syntax_set[..],
            &format!("minimal syntax set {}", index),
            COMPRESS_SERIALIZED_MINIMAL_SYNTAXES,
        )
        .map_err(|_| format!("Could not parse minimal syntax set {}", index).into())
    }

    fn get_minimal_syntax_set_with_index(&self, index: usize) -> Option<&SyntaxSet> {
        self.deserialized_minimal_syntaxes
            .get(index)
            .and_then(|cell| {
                cell.try_borrow_with(|| self.load_minimal_syntax_set_with_index(index))
                    .ok()
            })
    }
}

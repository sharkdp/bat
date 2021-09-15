use std::path::PathBuf;

use syntect::parsing::SyntaxSet;

use super::*;

/// A SyntaxSet in serialized form, i.e. bincoded and flate2 compressed.
/// We keep it in this format since we want to load it lazily.
#[derive(Debug)]
pub enum SerializedSyntaxSet {
    /// The data comes from a user-generated cache file.
    FromFile(PathBuf),

    /// The data to use is embedded into the bat binary.
    FromBinary(&'static [u8]),
}

impl SerializedSyntaxSet {
    pub fn deserialize(&self) -> Result<SyntaxSet> {
        match self {
            SerializedSyntaxSet::FromBinary(data) => Ok(from_binary(data, COMPRESS_SYNTAXES)),
            SerializedSyntaxSet::FromFile(ref path) => {
                asset_from_cache(path, "syntax set", COMPRESS_SYNTAXES)
            }
        }
    }
}

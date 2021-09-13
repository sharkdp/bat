use std::collections::HashMap;
use std::ffi::OsStr;
use std::fs;
use std::path::{Path, PathBuf};

use lazycell::LazyCell;

use syntect::highlighting::{Theme, ThemeSet};
use syntect::parsing::{SyntaxReference, SyntaxSet};

use path_abs::PathAbs;

use crate::bat_warning;
use crate::error::*;
use crate::input::{InputReader, OpenedInput, OpenedInputKind};
use crate::syntax_mapping::{MappingTarget, SyntaxMapping};

#[derive(Debug)]
pub struct HighlightingAssets {
    syntax_set_cell: LazyCell<SyntaxSet>,
    serialized_syntax_set: SerializedSyntaxSet,

    minimal_syntaxes: MinimalSyntaxes,

    /// Lazily load serialized [SyntaxSet]s from [Self.minimal_syntaxes]. The
    /// index in this vec matches the index in
    /// [Self.minimal_syntaxes.serialized_syntax_sets]
    deserialized_minimal_syntaxes: Vec<LazyCell<SyntaxSet>>,

    theme_set: ThemeSet,
    fallback_theme: Option<&'static str>,
}

#[derive(Debug)]
pub struct SyntaxReferenceInSet<'a> {
    pub syntax: &'a SyntaxReference,
    pub syntax_set: &'a SyntaxSet,
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

// Compress for size of ~700 kB instead of ~4600 kB at the cost of ~30% longer deserialization time
pub(crate) const COMPRESS_SYNTAXES: bool = true;

// Compress for size of ~20 kB instead of ~200 kB at the cost of ~30% longer deserialization time
pub(crate) const COMPRESS_THEMES: bool = true;

// Compress for size of ~400 kB instead of ~2100 kB at the cost of ~30% longer deserialization time
pub(crate) const COMPRESS_SERIALIZED_MINIMAL_SYNTAXES: bool = true;

// Whether or not to compress the serialized form of [MinimalSyntaxes]. Shall
// always be `false`, because the data in
// [MinimalSyntaxes.serialized_syntax_sets] has already been compressed
// (assuming [COMPRESS_SERIALIZED_MINIMAL_SYNTAXES] is `true`). The "outer" data
// structures like `by_name` are tiny. If we compress, deserialization can't do
// efficient byte-by-byte copy of `serialized_syntax_sets`.
pub(crate) const COMPRESS_MINIMAL_SYNTAXES: bool = false;

const IGNORED_SUFFIXES: [&str; 13] = [
    // Editor etc backups
    "~",
    ".bak",
    ".old",
    ".orig",
    // Debian and derivatives apt/dpkg/ucf backups
    ".dpkg-dist",
    ".dpkg-old",
    ".ucf-dist",
    ".ucf-new",
    ".ucf-old",
    // Red Hat and derivatives rpm backups
    ".rpmnew",
    ".rpmorig",
    ".rpmsave",
    // Build system input/template files
    ".in",
];

impl HighlightingAssets {
    fn new(
        serialized_syntax_set: SerializedSyntaxSet,
        minimal_syntaxes: MinimalSyntaxes,
        theme_set: ThemeSet,
    ) -> Self {
        // Prepare so we can lazily load minimal syntaxes without a mut reference
        let deserialized_minimal_syntaxes =
            vec![LazyCell::new(); minimal_syntaxes.serialized_syntax_sets.len()];

        HighlightingAssets {
            syntax_set_cell: LazyCell::new(),
            serialized_syntax_set,
            deserialized_minimal_syntaxes,
            minimal_syntaxes,
            theme_set,
            fallback_theme: None,
        }
    }

    pub fn default_theme() -> &'static str {
        "Monokai Extended"
    }

    pub fn from_cache(cache_path: &Path) -> Result<Self> {
        Ok(HighlightingAssets::new(
            SerializedSyntaxSet::FromFile(cache_path.join("syntaxes.bin")),
            asset_from_cache(
                &cache_path.join("minimal_syntaxes.bin"),
                "minimal syntax sets",
                COMPRESS_MINIMAL_SYNTAXES,
            )?,
            asset_from_cache(&cache_path.join("themes.bin"), "theme set", COMPRESS_THEMES)?,
        ))
    }

    pub fn from_binary() -> Self {
        HighlightingAssets::new(
            SerializedSyntaxSet::FromBinary(get_serialized_integrated_syntaxset()),
            get_integrated_minimal_syntaxes(),
            get_integrated_themeset(),
        )
    }

    pub fn set_fallback_theme(&mut self, theme: &'static str) {
        self.fallback_theme = Some(theme);
    }

    pub(crate) fn get_syntax_set(&self) -> Result<&SyntaxSet> {
        self.syntax_set_cell
            .try_borrow_with(|| self.serialized_syntax_set.deserialize())
    }

    /// Use [Self::get_syntaxes] instead
    #[deprecated]
    pub fn syntaxes(&self) -> &[SyntaxReference] {
        self.get_syntax_set()
            .expect(".syntaxes() is deprecated, use .get_syntaxes() instead")
            .syntaxes()
    }

    pub fn get_syntaxes(&self) -> Result<&[SyntaxReference]> {
        Ok(self.get_syntax_set()?.syntaxes())
    }

    fn get_theme_set(&self) -> &ThemeSet {
        &self.theme_set
    }

    pub fn themes(&self) -> impl Iterator<Item = &str> {
        self.get_theme_set().themes.keys().map(|s| s.as_ref())
    }

    /// Finds a [SyntaxSet] that contains a [SyntaxReference] by its name. First
    /// tries to find a minimal [SyntaxSet]. If none is found, returns the
    /// [SyntaxSet] that contains all syntaxes.
    fn get_syntax_set_by_name(&self, name: &str) -> Result<&SyntaxSet> {
        let minimal_syntax_set = self
            .minimal_syntaxes
            .by_name
            .get(&name.to_ascii_lowercase())
            .and_then(|index| self.get_minimal_syntax_set_with_index(*index));

        match minimal_syntax_set {
            Some(syntax_set) => Ok(syntax_set),
            None => self.get_syntax_set(),
        }
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

    /// Use [Self::get_syntax_for_file_name] instead
    #[deprecated]
    pub fn syntax_for_file_name(
        &self,
        file_name: impl AsRef<Path>,
        mapping: &SyntaxMapping,
    ) -> Option<&SyntaxReference> {
        self.get_syntax_for_file_name(file_name, mapping)
            .expect(
                ".syntax_for_file_name() is deprecated, use .get_syntax_for_file_name() instead",
            )
            .map(|syntax_in_set| syntax_in_set.syntax)
    }

    pub fn get_syntax_for_file_name(
        &self,
        file_name: impl AsRef<Path>,
        mapping: &SyntaxMapping,
    ) -> Result<Option<SyntaxReferenceInSet>> {
        let file_name = file_name.as_ref();
        Ok(match mapping.get_syntax_for(file_name) {
            Some(MappingTarget::MapToUnknown) => None,
            Some(MappingTarget::MapTo(syntax_name)) => self.find_syntax_by_name(syntax_name)?,
            None => self.get_extension_syntax(file_name.as_os_str())?,
        })
    }

    pub(crate) fn get_theme(&self, theme: &str) -> &Theme {
        match self.get_theme_set().themes.get(theme) {
            Some(theme) => theme,
            None => {
                if theme == "ansi-light" || theme == "ansi-dark" {
                    bat_warning!("Theme '{}' is deprecated, using 'ansi' instead.", theme);
                    return self.get_theme("ansi");
                }
                if !theme.is_empty() {
                    bat_warning!("Unknown theme '{}', using default.", theme)
                }
                &self.get_theme_set().themes
                    [self.fallback_theme.unwrap_or_else(|| Self::default_theme())]
            }
        }
    }

    pub(crate) fn get_syntax(
        &self,
        language: Option<&str>,
        input: &mut OpenedInput,
        mapping: &SyntaxMapping,
    ) -> Result<SyntaxReferenceInSet> {
        if let Some(language) = language {
            let syntax_set = self.get_syntax_set_by_name(language)?;
            return syntax_set
                .find_syntax_by_token(language)
                .map(|syntax| SyntaxReferenceInSet { syntax, syntax_set })
                .ok_or_else(|| Error::UnknownSyntax(language.to_owned()));
        }

        let line_syntax = self.get_first_line_syntax(&mut input.reader)?;

        // Get the path of the file:
        // If this was set by the metadata, that will take priority.
        // If it wasn't, it will use the real file path (if available).
        let path_str = input
            .metadata
            .user_provided_name
            .as_ref()
            .or_else(|| match input.kind {
                OpenedInputKind::OrdinaryFile(ref path) => Some(path),
                _ => None,
            });

        if let Some(path_str) = path_str {
            // If a path was provided, we try and detect the syntax based on extension mappings.
            let path = Path::new(path_str);
            let absolute_path = PathAbs::new(path)
                .ok()
                .map(|p| p.as_path().to_path_buf())
                .unwrap_or_else(|| path.to_owned());

            match mapping.get_syntax_for(absolute_path) {
                Some(MappingTarget::MapToUnknown) => line_syntax
                    .ok_or_else(|| Error::UndetectedSyntax(path.to_string_lossy().into())),

                Some(MappingTarget::MapTo(syntax_name)) => self
                    .find_syntax_by_name(syntax_name)?
                    .ok_or_else(|| Error::UnknownSyntax(syntax_name.to_owned())),

                None => {
                    let file_name = path.file_name().unwrap_or_default();
                    self.get_extension_syntax(file_name)?
                        .or(line_syntax)
                        .ok_or_else(|| Error::UndetectedSyntax(path.to_string_lossy().into()))
                }
            }
        } else {
            // If a path wasn't provided, we fall back to the detect first-line syntax.
            line_syntax.ok_or_else(|| Error::UndetectedSyntax("[unknown]".into()))
        }
    }

    fn find_syntax_by_name(&self, syntax_name: &str) -> Result<Option<SyntaxReferenceInSet>> {
        let syntax_set = self.get_syntax_set()?;
        Ok(syntax_set
            .find_syntax_by_name(syntax_name)
            .map(|syntax| SyntaxReferenceInSet { syntax, syntax_set }))
    }

    fn get_extension_syntax(&self, file_name: &OsStr) -> Result<Option<SyntaxReferenceInSet>> {
        let mut syntax = self.find_syntax_by_file_name(file_name)?;
        if syntax.is_none() {
            syntax = self.find_syntax_by_file_name_extension(file_name)?;
        }
        if syntax.is_none() {
            syntax = self.get_extension_syntax_with_stripped_suffix(file_name)?;
        }
        Ok(syntax)
    }

    fn find_syntax_by_file_name(&self, file_name: &OsStr) -> Result<Option<SyntaxReferenceInSet>> {
        let syntax_set = self.get_syntax_set()?;
        Ok(syntax_set
            .find_syntax_by_extension(file_name.to_str().unwrap_or_default())
            .map(|syntax| SyntaxReferenceInSet { syntax, syntax_set }))
    }

    fn find_syntax_by_file_name_extension(
        &self,
        file_name: &OsStr,
    ) -> Result<Option<SyntaxReferenceInSet>> {
        let file_path = Path::new(file_name);
        let syntax_set = self.get_syntax_set()?;
        Ok(syntax_set
            .find_syntax_by_extension(
                file_path
                    .extension()
                    .and_then(|x| x.to_str())
                    .unwrap_or_default(),
            )
            .map(|syntax| SyntaxReferenceInSet { syntax, syntax_set }))
    }

    /// If we find an ignored suffix on the file name, e.g. '~', we strip it and
    /// then try again to find a syntax without it. Note that we do this recursively.
    fn get_extension_syntax_with_stripped_suffix(
        &self,
        file_name: &OsStr,
    ) -> Result<Option<SyntaxReferenceInSet>> {
        let file_path = Path::new(file_name);
        let mut syntax = None;
        if let Some(file_str) = file_path.to_str() {
            for suffix in &IGNORED_SUFFIXES {
                if let Some(stripped_filename) = file_str.strip_suffix(suffix) {
                    syntax = self.get_extension_syntax(OsStr::new(stripped_filename))?;
                    break;
                }
            }
        }
        Ok(syntax)
    }

    fn get_first_line_syntax(
        &self,
        reader: &mut InputReader,
    ) -> Result<Option<SyntaxReferenceInSet>> {
        let syntax_set = self.get_syntax_set()?;
        Ok(String::from_utf8(reader.first_line.clone())
            .ok()
            .and_then(|l| syntax_set.find_syntax_by_first_line(&l))
            .map(|syntax| SyntaxReferenceInSet { syntax, syntax_set }))
    }
}

#[cfg(feature = "build-assets")]
pub use crate::build_assets::build_assets as build;

/// A SyntaxSet in serialized form, i.e. bincoded and flate2 compressed.
/// We keep it in this format since we want to load it lazily.
#[derive(Debug)]
enum SerializedSyntaxSet {
    /// The data comes from a user-generated cache file.
    FromFile(PathBuf),

    /// The data to use is embedded into the bat binary.
    FromBinary(&'static [u8]),
}

impl SerializedSyntaxSet {
    fn deserialize(&self) -> Result<SyntaxSet> {
        match self {
            SerializedSyntaxSet::FromBinary(data) => Ok(from_binary(data, COMPRESS_SYNTAXES)),
            SerializedSyntaxSet::FromFile(ref path) => {
                asset_from_cache(path, "syntax set", COMPRESS_SYNTAXES)
            }
        }
    }
}

pub(crate) fn get_serialized_integrated_syntaxset() -> &'static [u8] {
    include_bytes!("../assets/syntaxes.bin")
}

pub(crate) fn get_integrated_themeset() -> ThemeSet {
    from_binary(include_bytes!("../assets/themes.bin"), COMPRESS_THEMES)
}

fn get_integrated_minimal_syntaxes() -> MinimalSyntaxes {
    from_binary(
        include_bytes!("../assets/minimal_syntaxes.bin"),
        COMPRESS_MINIMAL_SYNTAXES,
    )
}

pub(crate) fn from_binary<T: serde::de::DeserializeOwned>(v: &[u8], compressed: bool) -> T {
    asset_from_contents(v, "n/a", compressed)
        .expect("data integrated in binary is never faulty, but make sure `compressed` is in sync!")
}

fn asset_from_contents<T: serde::de::DeserializeOwned>(
    contents: &[u8],
    description: &str,
    compressed: bool,
) -> Result<T> {
    if compressed {
        bincode::deserialize_from(flate2::read::ZlibDecoder::new(contents))
    } else {
        bincode::deserialize_from(contents)
    }
    .map_err(|_| format!("Could not parse {}", description).into())
}

fn asset_from_cache<T: serde::de::DeserializeOwned>(
    path: &Path,
    description: &str,
    compressed: bool,
) -> Result<T> {
    let contents = fs::read(path).map_err(|_| {
        format!(
            "Could not load cached {} '{}'",
            description,
            path.to_string_lossy()
        )
    })?;
    asset_from_contents(&contents[..], description, compressed)
        .map_err(|_| format!("Could not parse cached {}", description).into())
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::ffi::OsStr;

    use std::fs::File;
    use std::io::{BufReader, Write};
    use tempfile::TempDir;

    use crate::input::Input;

    struct SyntaxDetectionTest<'a> {
        assets: HighlightingAssets,
        pub syntax_mapping: SyntaxMapping<'a>,
        pub temp_dir: TempDir,
    }

    impl<'a> SyntaxDetectionTest<'a> {
        fn new() -> Self {
            SyntaxDetectionTest {
                assets: HighlightingAssets::from_binary(),
                syntax_mapping: SyntaxMapping::builtin(),
                temp_dir: TempDir::new().expect("creation of temporary directory"),
            }
        }

        fn get_syntax_name(
            &self,
            language: Option<&str>,
            input: &mut OpenedInput,
            mapping: &SyntaxMapping,
        ) -> String {
            self.assets
                .get_syntax(language, input, mapping)
                .map(|syntax_in_set| syntax_in_set.syntax.name.clone())
                .unwrap_or_else(|_| "!no syntax!".to_owned())
        }

        fn syntax_for_real_file_with_content_os(
            &self,
            file_name: &OsStr,
            first_line: &str,
        ) -> String {
            let file_path = self.temp_dir.path().join(file_name);
            {
                let mut temp_file = File::create(&file_path).unwrap();
                writeln!(temp_file, "{}", first_line).unwrap();
            }

            let input = Input::ordinary_file(&file_path);
            let dummy_stdin: &[u8] = &[];
            let mut opened_input = input.open(dummy_stdin, None).unwrap();

            self.get_syntax_name(None, &mut opened_input, &self.syntax_mapping)
        }

        fn syntax_for_file_with_content_os(&self, file_name: &OsStr, first_line: &str) -> String {
            let file_path = self.temp_dir.path().join(file_name);
            let input = Input::from_reader(Box::new(BufReader::new(first_line.as_bytes())))
                .with_name(Some(&file_path));
            let dummy_stdin: &[u8] = &[];
            let mut opened_input = input.open(dummy_stdin, None).unwrap();

            self.get_syntax_name(None, &mut opened_input, &self.syntax_mapping)
        }

        #[cfg(unix)]
        fn syntax_for_file_os(&self, file_name: &OsStr) -> String {
            self.syntax_for_file_with_content_os(file_name, "")
        }

        fn syntax_for_file_with_content(&self, file_name: &str, first_line: &str) -> String {
            self.syntax_for_file_with_content_os(OsStr::new(file_name), first_line)
        }

        fn syntax_for_file(&self, file_name: &str) -> String {
            self.syntax_for_file_with_content(file_name, "")
        }

        fn syntax_for_stdin_with_content(&self, file_name: &str, content: &[u8]) -> String {
            let input = Input::stdin().with_name(Some(file_name));
            let mut opened_input = input.open(content, None).unwrap();

            self.get_syntax_name(None, &mut opened_input, &self.syntax_mapping)
        }

        fn syntax_is_same_for_inputkinds(&self, file_name: &str, content: &str) -> bool {
            let as_file = self.syntax_for_real_file_with_content_os(file_name.as_ref(), content);
            let as_reader = self.syntax_for_file_with_content_os(file_name.as_ref(), content);
            let consistent = as_file == as_reader;
            // TODO: Compare StdIn somehow?

            if !consistent {
                eprintln!(
                    "Inconsistent syntax detection:\nFor File: {}\nFor Reader: {}",
                    as_file, as_reader
                )
            }

            consistent
        }
    }

    #[test]
    fn syntax_detection_basic() {
        let test = SyntaxDetectionTest::new();

        assert_eq!(test.syntax_for_file("test.rs"), "Rust");
        assert_eq!(test.syntax_for_file("test.cpp"), "C++");
        assert_eq!(test.syntax_for_file("test.build"), "NAnt Build File");
        assert_eq!(
            test.syntax_for_file("PKGBUILD"),
            "Bourne Again Shell (bash)"
        );
        assert_eq!(test.syntax_for_file(".bashrc"), "Bourne Again Shell (bash)");
        assert_eq!(test.syntax_for_file("Makefile"), "Makefile");
    }

    #[cfg(unix)]
    #[test]
    fn syntax_detection_invalid_utf8() {
        use std::os::unix::ffi::OsStrExt;

        let test = SyntaxDetectionTest::new();

        assert_eq!(
            test.syntax_for_file_os(OsStr::from_bytes(b"invalid_\xFEutf8_filename.rs")),
            "Rust"
        );
    }

    #[test]
    fn syntax_detection_same_for_inputkinds() {
        let mut test = SyntaxDetectionTest::new();

        test.syntax_mapping
            .insert("*.myext", MappingTarget::MapTo("C"))
            .ok();
        test.syntax_mapping
            .insert("MY_FILE", MappingTarget::MapTo("Markdown"))
            .ok();

        assert!(test.syntax_is_same_for_inputkinds("Test.md", ""));
        assert!(test.syntax_is_same_for_inputkinds("Test.txt", "#!/bin/bash"));
        assert!(test.syntax_is_same_for_inputkinds(".bashrc", ""));
        assert!(test.syntax_is_same_for_inputkinds("test.h", ""));
        assert!(test.syntax_is_same_for_inputkinds("test.js", "#!/bin/bash"));
        assert!(test.syntax_is_same_for_inputkinds("test.myext", ""));
        assert!(test.syntax_is_same_for_inputkinds("MY_FILE", ""));
        assert!(test.syntax_is_same_for_inputkinds("MY_FILE", "<?php"));
    }

    #[test]
    fn syntax_detection_well_defined_mapping_for_duplicate_extensions() {
        let test = SyntaxDetectionTest::new();

        assert_eq!(test.syntax_for_file("test.h"), "C++");
        assert_eq!(test.syntax_for_file("test.sass"), "Sass");
        assert_eq!(test.syntax_for_file("test.js"), "JavaScript (Babel)");
        assert_eq!(test.syntax_for_file("test.fs"), "F#");
        assert_eq!(test.syntax_for_file("test.v"), "Verilog");
    }

    #[test]
    fn syntax_detection_first_line() {
        let test = SyntaxDetectionTest::new();

        assert_eq!(
            test.syntax_for_file_with_content("my_script", "#!/bin/bash"),
            "Bourne Again Shell (bash)"
        );
        assert_eq!(
            test.syntax_for_file_with_content("build", "#!/bin/bash"),
            "Bourne Again Shell (bash)"
        );
        assert_eq!(
            test.syntax_for_file_with_content("my_script", "<?php"),
            "PHP"
        );
    }

    #[test]
    fn syntax_detection_with_custom_mapping() {
        let mut test = SyntaxDetectionTest::new();

        assert_eq!(test.syntax_for_file("test.h"), "C++");
        test.syntax_mapping
            .insert("*.h", MappingTarget::MapTo("C"))
            .ok();
        assert_eq!(test.syntax_for_file("test.h"), "C");
    }

    #[test]
    fn syntax_detection_is_case_sensitive() {
        let mut test = SyntaxDetectionTest::new();

        assert_ne!(test.syntax_for_file("README.MD"), "Markdown");
        test.syntax_mapping
            .insert("*.MD", MappingTarget::MapTo("Markdown"))
            .ok();
        assert_eq!(test.syntax_for_file("README.MD"), "Markdown");
    }

    #[test]
    fn syntax_detection_stdin_filename() {
        let test = SyntaxDetectionTest::new();

        // from file extension
        assert_eq!(test.syntax_for_stdin_with_content("test.cpp", b"a"), "C++");
        // from first line (fallback)
        assert_eq!(
            test.syntax_for_stdin_with_content("my_script", b"#!/bin/bash"),
            "Bourne Again Shell (bash)"
        );
    }

    #[cfg(unix)]
    #[test]
    fn syntax_detection_for_symlinked_file() {
        use std::os::unix::fs::symlink;

        let test = SyntaxDetectionTest::new();
        let file_path = test.temp_dir.path().join("my_ssh_config_filename");
        {
            File::create(&file_path).unwrap();
        }
        let file_path_symlink = test.temp_dir.path().join(".ssh").join("config");

        std::fs::create_dir(test.temp_dir.path().join(".ssh"))
            .expect("creation of directory succeeds");
        symlink(&file_path, &file_path_symlink).expect("creation of symbolic link succeeds");

        let input = Input::ordinary_file(&file_path_symlink);
        let dummy_stdin: &[u8] = &[];
        let mut opened_input = input.open(dummy_stdin, None).unwrap();

        assert_eq!(
            test.get_syntax_name(None, &mut opened_input, &test.syntax_mapping),
            "SSH Config"
        );
    }
}

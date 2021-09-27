use std::ffi::OsStr;
use std::fs;
use std::path::Path;

use lazycell::LazyCell;

use syntect::highlighting::{Theme, ThemeSet};
use syntect::parsing::{SyntaxReference, SyntaxSet};

use path_abs::PathAbs;

use crate::bat_warning;
use crate::error::*;
use crate::input::{InputReader, OpenedInput};
use crate::syntax_mapping::{MappingTarget, SyntaxMapping};

use ignored_suffixes::*;
use minimal_assets::*;
use serialized_syntax_set::*;

#[cfg(feature = "build-assets")]
pub use crate::assets::build_assets::*;

pub(crate) mod assets_metadata;
#[cfg(feature = "build-assets")]
mod build_assets;
mod ignored_suffixes;
mod minimal_assets;
mod serialized_syntax_set;

#[derive(Debug)]
pub struct HighlightingAssets {
    syntax_set_cell: LazyCell<SyntaxSet>,
    serialized_syntax_set: SerializedSyntaxSet,

    minimal_assets: MinimalAssets,

    theme_set: ThemeSet,
    fallback_theme: Option<&'static str>,
}

#[derive(Debug)]
pub struct SyntaxReferenceInSet<'a> {
    pub syntax: &'a SyntaxReference,
    pub syntax_set: &'a SyntaxSet,
}

/// Compress for size of ~700 kB instead of ~4600 kB at the cost of ~30% longer deserialization time
pub(crate) const COMPRESS_SYNTAXES: bool = true;

/// Compress for size of ~20 kB instead of ~200 kB at the cost of ~30% longer deserialization time
pub(crate) const COMPRESS_THEMES: bool = true;

/// Compress for size of ~400 kB instead of ~2100 kB at the cost of ~30% longer deserialization time
pub(crate) const COMPRESS_SERIALIZED_MINIMAL_SYNTAXES: bool = true;

/// Whether or not to compress the serialized form of [MinimalSyntaxes]. Shall
/// always be `false`, because the data in
/// [MinimalSyntaxes.serialized_syntax_sets] has already been compressed
/// (assuming [COMPRESS_SERIALIZED_MINIMAL_SYNTAXES] is `true`). The "outer" data
/// structures like `by_name` are tiny. If we compress, deserialization can't do
/// efficient byte-by-byte copy of `serialized_syntax_sets`.
pub(crate) const COMPRESS_MINIMAL_SYNTAXES: bool = false;

impl HighlightingAssets {
    fn new(
        serialized_syntax_set: SerializedSyntaxSet,
        minimal_syntaxes: MinimalSyntaxes,
        theme_set: ThemeSet,
    ) -> Self {
        HighlightingAssets {
            syntax_set_cell: LazyCell::new(),
            serialized_syntax_set,
            minimal_assets: MinimalAssets::new(minimal_syntaxes),
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
        match self.minimal_assets.get_syntax_set_by_name(name) {
            Some(syntax_set) => Ok(syntax_set),
            None => self.get_syntax_set(),
        }
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

        let path = input.path();
        let path_syntax = if let Some(path) = path {
            // If a path was provided, we try and detect the syntax based on extension mappings.
            match mapping.get_syntax_for(
                PathAbs::new(path).map_or_else(|_| path.to_owned(), |p| p.as_path().to_path_buf()),
            ) {
                Some(MappingTarget::MapToUnknown) => {
                    Err(Error::UndetectedSyntax(path.to_string_lossy().into()))
                }

                Some(MappingTarget::MapTo(syntax_name)) => self
                    .find_syntax_by_name(syntax_name)?
                    .ok_or_else(|| Error::UnknownSyntax(syntax_name.to_owned())),

                None => {
                    let file_name = path.file_name().unwrap_or_default();
                    self.get_extension_syntax(file_name)?
                        .ok_or_else(|| Error::UndetectedSyntax(path.to_string_lossy().into()))
                }
            }
        } else {
            Err(Error::UndetectedSyntax("[unknown]".into()))
        };

        match path_syntax {
            // If a path wasn't provided, or if path based syntax detection
            // above failed, we fall back to first-line syntax detection.
            Err(Error::UndetectedSyntax(path)) => self
                .get_first_line_syntax(&mut input.reader)?
                .ok_or(Error::UndetectedSyntax(path)),
            _ => path_syntax,
        }
    }

    pub(crate) fn find_syntax_by_name(
        &self,
        syntax_name: &str,
    ) -> Result<Option<SyntaxReferenceInSet>> {
        let syntax_set = self.get_syntax_set()?;
        Ok(syntax_set
            .find_syntax_by_name(syntax_name)
            .map(|syntax| SyntaxReferenceInSet { syntax, syntax_set }))
    }

    fn find_syntax_by_extension(&self, e: Option<&OsStr>) -> Result<Option<SyntaxReferenceInSet>> {
        let syntax_set = self.get_syntax_set()?;
        let extension = e.and_then(|x| x.to_str()).unwrap_or_default();
        Ok(syntax_set
            .find_syntax_by_extension(extension)
            .map(|syntax| SyntaxReferenceInSet { syntax, syntax_set }))
    }

    fn get_extension_syntax(&self, file_name: &OsStr) -> Result<Option<SyntaxReferenceInSet>> {
        let mut syntax = self.find_syntax_by_extension(Some(file_name))?;
        if syntax.is_none() {
            syntax = self.find_syntax_by_extension(Path::new(file_name).extension())?;
        }
        if syntax.is_none() {
            syntax = try_with_stripped_suffix(file_name, |stripped_file_name| {
                self.get_extension_syntax(stripped_file_name) // Note: recursion
            })?;
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

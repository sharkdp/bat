use std::ffi::OsStr;
use std::fs;
use std::path::Path;

use once_cell::unsync::OnceCell;

use syntect::highlighting::Theme;
use syntect::parsing::{SyntaxReference, SyntaxSet};

use path_abs::PathAbs;

use crate::error::*;
use crate::input::{InputReader, OpenedInput};
use crate::syntax_mapping::ignored_suffixes::IgnoredSuffixes;
use crate::syntax_mapping::MappingTarget;
use crate::theme::{default_theme, ColorScheme};
use crate::{bat_warning, SyntaxMapping};

use lazy_theme_set::LazyThemeSet;

use serialized_syntax_set::*;

#[cfg(feature = "build-assets")]
pub use crate::assets::build_assets::*;

pub(crate) mod assets_metadata;
#[cfg(feature = "build-assets")]
mod build_assets;
mod lazy_theme_set;
mod serialized_syntax_set;

#[derive(Debug)]
pub struct HighlightingAssets {
    syntax_set_cell: OnceCell<SyntaxSet>,
    serialized_syntax_set: SerializedSyntaxSet,

    theme_set: LazyThemeSet,
    fallback_theme: Option<&'static str>,
}

#[derive(Debug)]
pub struct SyntaxReferenceInSet<'a> {
    pub syntax: &'a SyntaxReference,
    pub syntax_set: &'a SyntaxSet,
}

/// Lazy-loaded syntaxes are already compressed, and we don't want to compress
/// already compressed data.
pub(crate) const COMPRESS_SYNTAXES: bool = false;

/// We don't want to compress our [LazyThemeSet] since the lazy-loaded themes
/// within it are already compressed, and compressing another time just makes
/// performance suffer
pub(crate) const COMPRESS_THEMES: bool = false;

/// Compress for size of ~40 kB instead of ~200 kB without much difference in
/// performance due to lazy-loading
pub(crate) const COMPRESS_LAZY_THEMES: bool = true;

/// Compress for size of ~10 kB instead of ~120 kB
pub(crate) const COMPRESS_ACKNOWLEDGEMENTS: bool = true;

impl HighlightingAssets {
    fn new(serialized_syntax_set: SerializedSyntaxSet, theme_set: LazyThemeSet) -> Self {
        HighlightingAssets {
            syntax_set_cell: OnceCell::new(),
            serialized_syntax_set,
            theme_set,
            fallback_theme: None,
        }
    }

    pub fn from_cache(cache_path: &Path) -> Result<Self> {
        Ok(HighlightingAssets::new(
            SerializedSyntaxSet::FromFile(cache_path.join("syntaxes.bin")),
            asset_from_cache(&cache_path.join("themes.bin"), "theme set", COMPRESS_THEMES)?,
        ))
    }

    pub fn from_binary() -> Self {
        HighlightingAssets::new(
            SerializedSyntaxSet::FromBinary(get_serialized_integrated_syntaxset()),
            get_integrated_themeset(),
        )
    }

    pub fn set_fallback_theme(&mut self, theme: &'static str) {
        self.fallback_theme = Some(theme);
    }

    /// Return the collection of syntect syntax definitions.
    pub fn get_syntax_set(&self) -> Result<&SyntaxSet> {
        self.syntax_set_cell
            .get_or_try_init(|| self.serialized_syntax_set.deserialize())
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

    fn get_theme_set(&self) -> &LazyThemeSet {
        &self.theme_set
    }

    pub fn themes(&self) -> impl Iterator<Item = &str> {
        self.get_theme_set().themes()
    }

    /// Use [Self::get_syntax_for_path] instead
    #[deprecated]
    pub fn syntax_for_file_name(
        &self,
        file_name: impl AsRef<Path>,
        mapping: &SyntaxMapping,
    ) -> Option<&SyntaxReference> {
        self.get_syntax_for_path(file_name, mapping)
            .ok()
            .map(|syntax_in_set| syntax_in_set.syntax)
    }

    /// Detect the syntax based on, in order:
    ///  1. Syntax mappings with [MappingTarget::MapTo] and [MappingTarget::MapToUnknown]
    ///     (e.g. `/etc/profile` -> `Bourne Again Shell (bash)`)
    ///  2. The file name (e.g. `Dockerfile`)
    ///  3. Syntax mappings with [MappingTarget::MapExtensionToUnknown]
    ///     (e.g. `*.conf`)
    ///  4. The file name extension (e.g. `.rs`)
    ///
    /// When detecting syntax based on syntax mappings, the full path is taken
    /// into account. When detecting syntax based on file name, no regard is
    /// taken to the path of the file. Only the file name itself matters. When
    /// detecting syntax based on file name extension, only the file name
    /// extension itself matters.
    ///
    /// Returns [Error::UndetectedSyntax] if it was not possible detect syntax
    /// based on path/file name/extension (or if the path was mapped to
    /// [MappingTarget::MapToUnknown] or [MappingTarget::MapExtensionToUnknown]).
    /// In this case it is appropriate to fall back to other methods to detect
    /// syntax. Such as using the contents of the first line of the file.
    ///
    /// Returns [Error::UnknownSyntax] if a syntax mapping exist, but the mapped
    /// syntax does not exist.
    pub fn get_syntax_for_path(
        &self,
        path: impl AsRef<Path>,
        mapping: &SyntaxMapping,
    ) -> Result<SyntaxReferenceInSet<'_>> {
        let path = path.as_ref();

        let syntax_match = mapping.get_syntax_for(path);

        if let Some(MappingTarget::MapToUnknown) = syntax_match {
            return Err(Error::UndetectedSyntax(path.to_string_lossy().into()));
        }

        if let Some(MappingTarget::MapTo(syntax_name)) = syntax_match {
            return self
                .find_syntax_by_token(syntax_name)?
                .ok_or_else(|| Error::UnknownSyntax(syntax_name.to_owned()));
        }

        let file_name = path.file_name().unwrap_or_default();

        match (
            self.get_syntax_for_file_name(file_name, &mapping.ignored_suffixes)?,
            syntax_match,
        ) {
            (Some(syntax), _) => Ok(syntax),

            (_, Some(MappingTarget::MapExtensionToUnknown)) => {
                Err(Error::UndetectedSyntax(path.to_string_lossy().into()))
            }

            _ => self
                .get_syntax_for_file_extension(file_name, &mapping.ignored_suffixes)?
                .ok_or_else(|| Error::UndetectedSyntax(path.to_string_lossy().into())),
        }
    }

    /// Look up a syntect theme by name.
    pub fn get_theme(&self, theme: &str) -> &Theme {
        match self.get_theme_set().get(theme) {
            Some(theme) => theme,
            None => {
                if theme == "ansi-light" || theme == "ansi-dark" {
                    bat_warning!("Theme '{theme}' is deprecated, using 'ansi' instead.");
                    return self.get_theme("ansi");
                }
                if !theme.is_empty() {
                    bat_warning!("Unknown theme '{theme}', using default.")
                }
                self.get_theme_set()
                    .get(
                        self.fallback_theme
                            .unwrap_or_else(|| default_theme(ColorScheme::Dark)),
                    )
                    .expect("something is very wrong if the default theme is missing")
            }
        }
    }

    pub(crate) fn get_syntax(
        &self,
        language: Option<&str>,
        input: &mut OpenedInput,
        mapping: &SyntaxMapping,
    ) -> Result<SyntaxReferenceInSet<'_>> {
        if let Some(language) = language {
            let syntax_set = self.get_syntax_set()?;
            return syntax_set
                .find_syntax_by_token(language)
                .map(|syntax| SyntaxReferenceInSet { syntax, syntax_set })
                .ok_or_else(|| Error::UnknownSyntax(language.to_owned()));
        }

        let path = input.path();
        let path_syntax = if let Some(path) = path {
            self.get_syntax_for_path(
                PathAbs::new(path).map_or_else(|_| path.to_owned(), |p| p.as_path().to_path_buf()),
                mapping,
            )
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
    ) -> Result<Option<SyntaxReferenceInSet<'_>>> {
        let syntax_set = self.get_syntax_set()?;
        Ok(syntax_set
            .find_syntax_by_name(syntax_name)
            .map(|syntax| SyntaxReferenceInSet { syntax, syntax_set }))
    }

    fn find_syntax_by_extension(
        &self,
        e: Option<&OsStr>,
    ) -> Result<Option<SyntaxReferenceInSet<'_>>> {
        let syntax_set = self.get_syntax_set()?;
        let extension = e.and_then(|x| x.to_str()).unwrap_or_default();
        Ok(syntax_set
            .find_syntax_by_extension(extension)
            .map(|syntax| SyntaxReferenceInSet { syntax, syntax_set }))
    }

    fn find_syntax_by_token(&self, token: &str) -> Result<Option<SyntaxReferenceInSet<'_>>> {
        let syntax_set = self.get_syntax_set()?;
        Ok(syntax_set
            .find_syntax_by_token(token)
            .map(|syntax| SyntaxReferenceInSet { syntax, syntax_set }))
    }

    fn get_syntax_for_file_name(
        &self,
        file_name: &OsStr,
        ignored_suffixes: &IgnoredSuffixes,
    ) -> Result<Option<SyntaxReferenceInSet<'_>>> {
        let mut syntax = self.find_syntax_by_extension(Some(file_name))?;
        if syntax.is_none() {
            syntax =
                ignored_suffixes.try_with_stripped_suffix(file_name, |stripped_file_name| {
                    // Note: recursion
                    self.get_syntax_for_file_name(stripped_file_name, ignored_suffixes)
                })?;
        }
        Ok(syntax)
    }

    fn get_syntax_for_file_extension(
        &self,
        file_name: &OsStr,
        ignored_suffixes: &IgnoredSuffixes,
    ) -> Result<Option<SyntaxReferenceInSet<'_>>> {
        let mut syntax = self.find_syntax_by_extension(Path::new(file_name).extension())?;
        if syntax.is_none() {
            syntax =
                ignored_suffixes.try_with_stripped_suffix(file_name, |stripped_file_name| {
                    // Note: recursion
                    self.get_syntax_for_file_extension(stripped_file_name, ignored_suffixes)
                })?;
        }
        Ok(syntax)
    }

    fn get_first_line_syntax(
        &self,
        reader: &mut InputReader,
    ) -> Result<Option<SyntaxReferenceInSet<'_>>> {
        let syntax_set = self.get_syntax_set()?;
        Ok(String::from_utf8(reader.first_line.clone())
            .ok()
            .and_then(|l| {
                // Strip UTF-8 BOM if present
                let line = l.strip_prefix('\u{feff}').unwrap_or(&l);
                syntax_set.find_syntax_by_first_line(line)
            })
            .map(|syntax| SyntaxReferenceInSet { syntax, syntax_set }))
    }
}

pub(crate) fn get_serialized_integrated_syntaxset() -> &'static [u8] {
    include_bytes!("../assets/syntaxes.bin")
}

pub(crate) fn get_integrated_themeset() -> LazyThemeSet {
    from_binary(include_bytes!("../assets/themes.bin"), COMPRESS_THEMES)
}

pub fn get_acknowledgements() -> String {
    from_binary(
        include_bytes!("../assets/acknowledgements.bin"),
        COMPRESS_ACKNOWLEDGEMENTS,
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
    .map_err(|_| format!("Could not parse {description}").into())
}

fn asset_from_cache<T: serde::de::DeserializeOwned>(
    path: &Path,
    description: &str,
    compressed: bool,
) -> Result<T> {
    let contents = fs::read(path).map_err(|_| {
        format!(
            "Could not load cached {description} '{}'",
            path.to_string_lossy()
        )
    })?;
    asset_from_contents(&contents[..], description, compressed)
        .map_err(|_| format!("Could not parse cached {description}").into())
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

    impl SyntaxDetectionTest<'_> {
        fn new() -> Self {
            SyntaxDetectionTest {
                assets: HighlightingAssets::from_binary(),
                syntax_mapping: SyntaxMapping::new(),
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
                writeln!(temp_file, "{first_line}").unwrap();
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
                    "Inconsistent syntax detection:\nFor File: {as_file}\nFor Reader: {as_reader}"
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
    fn syntax_detection_first_line_with_utf8_bom() {
        let test = SyntaxDetectionTest::new();

        // Test that XML files are detected correctly even with UTF-8 BOM
        // The BOM should be stripped before first-line syntax detection
        let xml_with_bom = "\u{feff}<?xml version=\"1.0\" encoding=\"utf-8\"?>";
        assert_eq!(
            test.syntax_for_file_with_content("unknown_file", xml_with_bom),
            "XML"
        );

        // Test the specific .csproj case mentioned in the issue
        // Even if .csproj has extension mapping, this tests first-line fallback
        let csproj_content_with_bom = "\u{feff}<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<Project ToolsVersion=\"15.0\" xmlns=\"http://schemas.microsoft.com/developer/msbuild/2003\">";
        assert_eq!(
            test.syntax_for_file_with_content("test.csproj", csproj_content_with_bom),
            "XML"
        );

        // Test that shell scripts are detected correctly even with UTF-8 BOM
        let script_with_bom = "\u{feff}#!/bin/bash";
        assert_eq!(
            test.syntax_for_file_with_content("unknown_script", script_with_bom),
            "Bourne Again Shell (bash)"
        );

        // Test that PHP files are detected correctly even with UTF-8 BOM
        let php_with_bom = "\u{feff}<?php";
        assert_eq!(
            test.syntax_for_file_with_content("unknown_php", php_with_bom),
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
    fn syntax_detection_with_extension_mapping_to_unknown() {
        let mut test = SyntaxDetectionTest::new();

        // Normally, a CMakeLists.txt file shall use the CMake syntax, even if it is
        // a bash script in disguise
        assert_eq!(
            test.syntax_for_file_with_content("CMakeLists.txt", "#!/bin/bash"),
            "CMake"
        );

        // Other .txt files shall use the Plain Text syntax
        assert_eq!(
            test.syntax_for_file_with_content("some-other.txt", "#!/bin/bash"),
            "Plain Text"
        );

        // If we setup MapExtensionToUnknown on *.txt, the match on the full
        // file name of "CMakeLists.txt" shall have higher prio, and CMake shall
        // still be used for it
        test.syntax_mapping
            .insert("*.txt", MappingTarget::MapExtensionToUnknown)
            .ok();
        assert_eq!(
            test.syntax_for_file_with_content("CMakeLists.txt", "#!/bin/bash"),
            "CMake"
        );

        // However, for *other* files with a .txt extension, first-line fallback
        // shall now be used
        assert_eq!(
            test.syntax_for_file_with_content("some-other.txt", "#!/bin/bash"),
            "Bourne Again Shell (bash)"
        );
    }

    #[test]
    fn syntax_detection_is_case_insensitive() {
        let mut test = SyntaxDetectionTest::new();

        assert_eq!(test.syntax_for_file("README.md"), "Markdown");
        assert_eq!(test.syntax_for_file("README.mD"), "Markdown");
        assert_eq!(test.syntax_for_file("README.Md"), "Markdown");
        assert_eq!(test.syntax_for_file("README.MD"), "Markdown");

        // Adding a mapping for "MD" in addition to "md" should not break the mapping
        test.syntax_mapping
            .insert("*.MD", MappingTarget::MapTo("Markdown"))
            .ok();

        assert_eq!(test.syntax_for_file("README.md"), "Markdown");
        assert_eq!(test.syntax_for_file("README.mD"), "Markdown");
        assert_eq!(test.syntax_for_file("README.Md"), "Markdown");
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

use std::ffi::OsStr;
use std::fs;
use std::path::{Path, PathBuf};

use lazycell::LazyCell;

use syntect::dumps::{dump_to_file, from_binary, from_reader};
use syntect::highlighting::{Theme, ThemeSet};
use syntect::parsing::{SyntaxReference, SyntaxSet, SyntaxSetBuilder};

use path_abs::PathAbs;

use crate::assets_metadata::AssetsMetadata;
use crate::bat_warning;
use crate::error::*;
use crate::input::{InputReader, OpenedInput, OpenedInputKind};
use crate::syntax_mapping::{MappingTarget, SyntaxMapping};

#[derive(Debug)]
pub struct HighlightingAssets {
    syntax_set_cell: LazyCell<SyntaxSet>,
    serialized_syntax_set: Option<SerializedSyntaxSet>,
    theme_set: ThemeSet,
    fallback_theme: Option<&'static str>,
}

#[derive(Debug)]
pub struct SyntaxReferenceInSet<'a> {
    pub syntax: &'a SyntaxReference,
    pub syntax_set: &'a SyntaxSet,
}

const IGNORED_SUFFIXES: [&str; 10] = [
    // Editor etc backups
    "~",
    ".bak",
    ".old",
    ".orig",
    // Debian and derivatives apt/dpkg backups
    ".dpkg-dist",
    ".dpkg-old",
    // Red Hat and derivatives rpm backups
    ".rpmnew",
    ".rpmorig",
    ".rpmsave",
    // Build system input/template files
    ".in",
];

impl HighlightingAssets {
    fn new(
        syntax_set: Option<SyntaxSet>,
        serialized_syntax_set: Option<SerializedSyntaxSet>,
        theme_set: ThemeSet,
    ) -> Self {
        assert!(syntax_set.is_some() || serialized_syntax_set.is_some());

        let syntax_set_cell = LazyCell::new();
        if let Some(syntax_set) = syntax_set {
            syntax_set_cell.fill(syntax_set).expect("can never fail");
        }

        HighlightingAssets {
            syntax_set_cell,
            serialized_syntax_set,
            theme_set,
            fallback_theme: None,
        }
    }

    pub fn default_theme() -> &'static str {
        "Monokai Extended"
    }

    pub fn from_files(source_dir: &Path, include_integrated_assets: bool) -> Result<Self> {
        let mut theme_set = if include_integrated_assets {
            get_integrated_themeset()
        } else {
            ThemeSet::new()
        };

        let theme_dir = source_dir.join("themes");
        if theme_dir.exists() {
            let res = theme_set.add_from_folder(&theme_dir);
            if let Err(err) = res {
                println!(
                    "Failed to load one or more themes from '{}' (reason: '{}')",
                    theme_dir.to_string_lossy(),
                    err,
                );
            }
        } else {
            println!(
                "No themes were found in '{}', using the default set",
                theme_dir.to_string_lossy()
            );
        }

        let mut syntax_set_builder = if !include_integrated_assets {
            let mut builder = SyntaxSetBuilder::new();
            builder.add_plain_text_syntax();
            builder
        } else {
            get_integrated_syntaxset().into_builder()
        };

        let syntax_dir = source_dir.join("syntaxes");
        if syntax_dir.exists() {
            syntax_set_builder.add_from_folder(syntax_dir, true)?;
        } else {
            println!(
                "No syntaxes were found in '{}', using the default set.",
                syntax_dir.to_string_lossy()
            );
        }

        if std::env::var("BAT_PRINT_SYNTAX_DEPENDENCIES").is_ok() {
            // To trigger this code, run:
            // BAT_PRINT_SYNTAX_DEPENDENCIES=1 cargo run -- cache --build --source assets --blank --target /tmp
            crate::syntax_dependencies::print_syntax_dependencies(&syntax_set_builder);
        }

        let syntax_set = syntax_set_builder.build();
        let missing_contexts = syntax_set.find_unlinked_contexts();
        if !missing_contexts.is_empty() {
            println!("Some referenced contexts could not be found!");
            for context in missing_contexts {
                println!("- {}", context);
            }
        }

        Ok(HighlightingAssets::new(Some(syntax_set), None, theme_set))
    }

    pub fn from_cache(cache_path: &Path) -> Result<Self> {
        Ok(HighlightingAssets::new(
            None,
            Some(SerializedSyntaxSet::FromFile(
                cache_path.join("syntaxes.bin"),
            )),
            asset_from_cache(&cache_path.join("themes.bin"), "theme set")?,
        ))
    }

    pub fn from_binary() -> Self {
        HighlightingAssets::new(
            None,
            Some(SerializedSyntaxSet::FromBinary(
                get_serialized_integrated_syntaxset(),
            )),
            get_integrated_themeset(),
        )
    }

    pub fn save_to_cache(&self, target_dir: &Path, current_version: &str) -> Result<()> {
        let _ = fs::create_dir_all(target_dir);
        asset_to_cache(
            self.get_theme_set(),
            &target_dir.join("themes.bin"),
            "theme set",
        )?;
        asset_to_cache(
            self.get_syntax_set()?,
            &target_dir.join("syntaxes.bin"),
            "syntax set",
        )?;

        print!(
            "Writing metadata to folder {} ... ",
            target_dir.to_string_lossy()
        );
        AssetsMetadata::new(current_version).save_to_folder(target_dir)?;
        println!("okay");

        Ok(())
    }

    pub fn set_fallback_theme(&mut self, theme: &'static str) {
        self.fallback_theme = Some(theme);
    }

    pub(crate) fn get_syntax_set(&self) -> Result<&SyntaxSet> {
        if !self.syntax_set_cell.filled() {
            self.syntax_set_cell.fill(
                self.serialized_syntax_set
                .as_ref()
                .expect("a dev forgot to setup serialized_syntax_set, please report to https://github.com/sharkdp/bat/issues")
                .deserialize()?
             ).unwrap();
        }

        // It is safe to .unwrap() because we just made sure it was .filled()
        Ok(self.syntax_set_cell.borrow().unwrap())
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
            Some(MappingTarget::MapTo(syntax_name)) => {
                let syntax_set = self.get_syntax_set()?;
                syntax_set
                    .find_syntax_by_name(syntax_name)
                    .map(|syntax| SyntaxReferenceInSet { syntax, syntax_set })
            }
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
            let syntax_set = self.get_syntax_set()?;
            syntax_set
                .find_syntax_by_token(language)
                .map(|syntax| SyntaxReferenceInSet { syntax, syntax_set })
                .ok_or_else(|| ErrorKind::UnknownSyntax(language.to_owned()).into())
        } else {
            let line_syntax = self.get_first_line_syntax(&mut input.reader)?;

            // Get the path of the file:
            // If this was set by the metadata, that will take priority.
            // If it wasn't, it will use the real file path (if available).
            let path_str =
                input
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
                    Some(MappingTarget::MapToUnknown) => line_syntax.ok_or_else(|| {
                        ErrorKind::UndetectedSyntax(path.to_string_lossy().into()).into()
                    }),

                    Some(MappingTarget::MapTo(syntax_name)) => {
                        let syntax_set = self.get_syntax_set()?;
                        syntax_set
                            .find_syntax_by_name(syntax_name)
                            .map(|syntax| SyntaxReferenceInSet { syntax, syntax_set })
                            .ok_or_else(|| ErrorKind::UnknownSyntax(syntax_name.to_owned()).into())
                    }

                    None => {
                        let file_name = path.file_name().unwrap_or_default();
                        self.get_extension_syntax(file_name)?
                            .or(line_syntax)
                            .ok_or_else(|| {
                                ErrorKind::UndetectedSyntax(path.to_string_lossy().into()).into()
                            })
                    }
                }
            } else {
                // If a path wasn't provided, we fall back to the detect first-line syntax.
                line_syntax.ok_or_else(|| ErrorKind::UndetectedSyntax("[unknown]".into()).into())
            }
        }
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
            for suffix in IGNORED_SUFFIXES.iter() {
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
            SerializedSyntaxSet::FromBinary(data) => Ok(from_binary(data)),
            SerializedSyntaxSet::FromFile(ref path) => asset_from_cache(&path, "syntax set"),
        }
    }
}

fn get_serialized_integrated_syntaxset() -> &'static [u8] {
    include_bytes!("../assets/syntaxes.bin")
}

fn get_integrated_syntaxset() -> SyntaxSet {
    from_binary(get_serialized_integrated_syntaxset())
}

fn get_integrated_themeset() -> ThemeSet {
    from_binary(include_bytes!("../assets/themes.bin"))
}

fn asset_to_cache<T: serde::Serialize>(asset: &T, path: &Path, description: &str) -> Result<()> {
    print!("Writing {} to {} ... ", description, path.to_string_lossy());
    dump_to_file(asset, &path).chain_err(|| {
        format!(
            "Could not save {} to {}",
            description,
            path.to_string_lossy()
        )
    })?;
    println!("okay");
    Ok(())
}

fn asset_from_cache<T: serde::de::DeserializeOwned>(path: &Path, description: &str) -> Result<T> {
    let contents = fs::read(path).chain_err(|| {
        format!(
            "Could not load cached {} '{}'",
            description,
            path.to_string_lossy()
        )
    })?;
    from_reader(&contents[..]).chain_err(|| format!("Could not parse cached {}", description))
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

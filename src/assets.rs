use std::collections::BTreeMap;
use std::ffi::OsStr;
use std::fs::{self, File};
use std::io::BufReader;
use std::path::Path;

use syntect::dumps::{dump_to_file, from_binary, from_reader};
use syntect::highlighting::{Theme, ThemeSet};
use syntect::parsing::{SyntaxReference, SyntaxSet, SyntaxSetBuilder};

use path_abs::PathAbs;

use crate::assets_metadata::AssetsMetadata;
use crate::error::*;
use crate::input::{InputReader, OpenedInput, OpenedInputKind};
use crate::syntax_mapping::{MappingTarget, SyntaxMapping};

#[derive(Debug)]
pub struct HighlightingAssets {
    pub(crate) syntax_set: SyntaxSet,
    pub(crate) theme_set: ThemeSet,
    fallback_theme: Option<&'static str>,
}

impl HighlightingAssets {
    pub fn default_theme() -> &'static str {
        "Monokai Extended"
    }

    pub fn from_files(source_dir: &Path, include_integrated_assets: bool) -> Result<Self> {
        let mut theme_set = if include_integrated_assets {
            Self::get_integrated_themeset()
        } else {
            ThemeSet {
                themes: BTreeMap::new(),
            }
        };

        let theme_dir = source_dir.join("themes");

        let res = theme_set.add_from_folder(&theme_dir);
        if res.is_err() {
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
            Self::get_integrated_syntaxset().into_builder()
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

        Ok(HighlightingAssets {
            syntax_set: syntax_set_builder.build(),
            theme_set,
            fallback_theme: None,
        })
    }

    pub fn from_cache(cache_path: &Path) -> Result<Self> {
        let syntax_set_path = cache_path.join("syntaxes.bin");
        let theme_set_path = cache_path.join("themes.bin");

        let syntax_set_file = File::open(&syntax_set_path).chain_err(|| {
            format!(
                "Could not load cached syntax set '{}'",
                syntax_set_path.to_string_lossy()
            )
        })?;
        let syntax_set: SyntaxSet = from_reader(BufReader::new(syntax_set_file))
            .chain_err(|| "Could not parse cached syntax set")?;

        let theme_set_file = File::open(&theme_set_path).chain_err(|| {
            format!(
                "Could not load cached theme set '{}'",
                theme_set_path.to_string_lossy()
            )
        })?;
        let theme_set: ThemeSet = from_reader(BufReader::new(theme_set_file))
            .chain_err(|| "Could not parse cached theme set")?;

        Ok(HighlightingAssets {
            syntax_set,
            theme_set,
            fallback_theme: None,
        })
    }

    fn get_integrated_syntaxset() -> SyntaxSet {
        from_binary(include_bytes!("../assets/syntaxes.bin"))
    }

    fn get_integrated_themeset() -> ThemeSet {
        from_binary(include_bytes!("../assets/themes.bin"))
    }

    pub fn from_binary() -> Self {
        let syntax_set = Self::get_integrated_syntaxset();
        let theme_set = Self::get_integrated_themeset();

        HighlightingAssets {
            syntax_set,
            theme_set,
            fallback_theme: None,
        }
    }

    pub fn save_to_cache(&self, target_dir: &Path, current_version: &str) -> Result<()> {
        let _ = fs::create_dir_all(target_dir);
        let theme_set_path = target_dir.join("themes.bin");
        let syntax_set_path = target_dir.join("syntaxes.bin");

        print!(
            "Writing theme set to {} ... ",
            theme_set_path.to_string_lossy()
        );
        dump_to_file(&self.theme_set, &theme_set_path).chain_err(|| {
            format!(
                "Could not save theme set to {}",
                theme_set_path.to_string_lossy()
            )
        })?;
        println!("okay");

        print!(
            "Writing syntax set to {} ... ",
            syntax_set_path.to_string_lossy()
        );
        dump_to_file(&self.syntax_set, &syntax_set_path).chain_err(|| {
            format!(
                "Could not save syntax set to {}",
                syntax_set_path.to_string_lossy()
            )
        })?;
        println!("okay");

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

    pub fn syntaxes(&self) -> &[SyntaxReference] {
        self.syntax_set.syntaxes()
    }

    pub fn themes(&self) -> impl Iterator<Item = &str> {
        self.theme_set.themes.keys().map(|s| s.as_ref())
    }

    pub fn syntax_for_file_name(
        &self,
        file_name: impl AsRef<Path>,
        mapping: &SyntaxMapping,
    ) -> Option<&SyntaxReference> {
        let file_name = file_name.as_ref();
        match mapping.get_syntax_for(file_name) {
            Some(MappingTarget::MapToUnknown) => None,
            Some(MappingTarget::MapTo(syntax_name)) => {
                self.syntax_set.find_syntax_by_name(syntax_name)
            }
            None => self.get_extension_syntax(file_name.as_os_str()),
        }
    }

    pub(crate) fn get_theme(&self, theme: &str) -> &Theme {
        match self.theme_set.themes.get(theme) {
            Some(theme) => theme,
            None => {
                if theme != "" {
                    use ansi_term::Colour::Yellow;
                    eprintln!(
                        "{}: Unknown theme '{}', using default.",
                        Yellow.paint("[bat warning]"),
                        theme
                    );
                }
                &self.theme_set.themes[self.fallback_theme.unwrap_or_else(|| Self::default_theme())]
            }
        }
    }

    pub(crate) fn get_syntax(
        &self,
        language: Option<&str>,
        input: &mut OpenedInput,
        mapping: &SyntaxMapping,
    ) -> Result<&SyntaxReference> {
        if let Some(language) = language {
            self.syntax_set
                .find_syntax_by_token(language)
                .ok_or_else(|| ErrorKind::UnknownSyntax(language.to_owned()).into())
        } else {
            let line_syntax = self.get_first_line_syntax(&mut input.reader);

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

                    Some(MappingTarget::MapTo(syntax_name)) => self
                        .syntax_set
                        .find_syntax_by_name(syntax_name)
                        .ok_or_else(|| ErrorKind::UnknownSyntax(syntax_name.to_owned()).into()),

                    None => {
                        let file_name = path.file_name().unwrap_or_default();
                        self.get_extension_syntax(file_name)
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

    fn get_extension_syntax(&self, file_name: &OsStr) -> Option<&SyntaxReference> {
        self.syntax_set
            .find_syntax_by_extension(file_name.to_str().unwrap_or_default())
            .or_else(|| {
                self.syntax_set.find_syntax_by_extension(
                    Path::new(file_name)
                        .extension()
                        .and_then(|x| x.to_str())
                        .unwrap_or_default(),
                )
            })
    }

    fn get_first_line_syntax(&self, reader: &mut InputReader) -> Option<&SyntaxReference> {
        String::from_utf8(reader.first_line.clone())
            .ok()
            .and_then(|l| self.syntax_set.find_syntax_by_first_line(&l))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::ffi::OsStr;

    use std::fs::File;
    use std::io::Write;
    use tempdir::TempDir;

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
                temp_dir: TempDir::new("bat_syntax_detection_tests")
                    .expect("creation of temporary directory"),
            }
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

            let input = Input::ordinary_file(file_path.as_os_str());
            let dummy_stdin: &[u8] = &[];
            let mut opened_input = input.open(dummy_stdin).unwrap();

            self.assets
                .get_syntax(None, &mut opened_input, &self.syntax_mapping)
                .unwrap_or_else(|_| self.assets.syntax_set.find_syntax_plain_text())
                .name
                .clone()
        }

        fn syntax_for_file_with_content_os(&self, file_name: &OsStr, first_line: &str) -> String {
            let file_path = self.temp_dir.path().join(file_name);
            let input = Input::from_reader(Box::new(BufReader::new(first_line.as_bytes())))
                .with_name(Some(file_path.as_os_str()));
            let dummy_stdin: &[u8] = &[];
            let mut opened_input = input.open(dummy_stdin).unwrap();

            self.assets
                .get_syntax(None, &mut opened_input, &self.syntax_mapping)
                .unwrap_or_else(|_| self.assets.syntax_set.find_syntax_plain_text())
                .name
                .clone()
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
            let input = Input::stdin().with_name(Some(OsStr::new(file_name)));
            let mut opened_input = input.open(content).unwrap();

            self.assets
                .get_syntax(None, &mut opened_input, &self.syntax_mapping)
                .unwrap_or_else(|_| self.assets.syntax_set.find_syntax_plain_text())
                .name
                .clone()
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

        let input = Input::ordinary_file(file_path_symlink.as_os_str());
        let dummy_stdin: &[u8] = &[];
        let mut opened_input = input.open(dummy_stdin).unwrap();

        assert_eq!(
            test.assets
                .get_syntax(None, &mut opened_input, &test.syntax_mapping)
                .unwrap_or_else(|_| test.assets.syntax_set.find_syntax_plain_text())
                .name,
            "SSH Config"
        );
    }
}

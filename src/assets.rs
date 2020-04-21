use std::collections::BTreeMap;
use std::ffi::OsStr;
use std::fs::{self, File};
use std::io::BufReader;
use std::path::Path;

use syntect::dumps::{dump_to_file, from_binary, from_reader};
use syntect::highlighting::{Theme, ThemeSet};
use syntect::parsing::{SyntaxReference, SyntaxSet, SyntaxSetBuilder};

use crate::errors::*;
use crate::inputfile::{InputFile, InputFileReader};
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

    pub fn from_cache(theme_set_path: &Path, syntax_set_path: &Path) -> Result<Self> {
        let syntax_set_file = File::open(syntax_set_path).chain_err(|| {
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

    pub fn save_to_cache(&self, target_dir: &Path) -> Result<()> {
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

        Ok(())
    }

    pub fn set_fallback_theme(&mut self, theme: &'static str) {
        self.fallback_theme = Some(theme);
    }

    pub fn syntaxes(&self) -> &[SyntaxReference] {
        self.syntax_set.syntaxes()
    }

    pub fn themes(&self) -> impl Iterator<Item = &String> {
        self.theme_set.themes.keys()
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
                &self.theme_set.themes[self.fallback_theme.unwrap_or(Self::default_theme())]
            }
        }
    }

    pub(crate) fn get_syntax(
        &self,
        language: Option<&str>,
        file: InputFile,
        reader: &mut InputFileReader,
        mapping: &SyntaxMapping,
    ) -> &SyntaxReference {
        let syntax = match (language, file) {
            (Some(language), _) => self.syntax_set.find_syntax_by_token(language),
            (None, InputFile::Ordinary(ofile)) => {
                let path = Path::new(ofile.provided_path());
                let line_syntax = self.get_first_line_syntax(reader);

                let absolute_path = path.canonicalize().ok().unwrap_or(path.to_owned());
                match mapping.get_syntax_for(absolute_path) {
                    Some(MappingTarget::MapTo(syntax_name)) => {
                        // TODO: we should probably return an error here if this syntax can not be
                        // found. Currently, we just fall back to 'plain'.
                        self.syntax_set.find_syntax_by_name(syntax_name)
                    }
                    Some(MappingTarget::MapToUnknown) => line_syntax,
                    None => {
                        let file_name = path.file_name().unwrap_or_default();
                        self.get_extension_syntax(file_name).or(line_syntax)
                    }
                }
            }
            (None, InputFile::StdIn(None)) => String::from_utf8(reader.first_line.clone())
                .ok()
                .and_then(|l| self.syntax_set.find_syntax_by_first_line(&l)),
            (None, InputFile::StdIn(Some(file_name))) => self
                .get_extension_syntax(file_name)
                .or(self.get_first_line_syntax(reader)),
            (_, InputFile::ThemePreviewFile) => self.syntax_set.find_syntax_by_name("Rust"),
        };

        syntax.unwrap_or_else(|| self.syntax_set.find_syntax_plain_text())
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

    fn get_first_line_syntax(&self, reader: &mut InputFileReader) -> Option<&SyntaxReference> {
        String::from_utf8(reader.first_line.clone())
            .ok()
            .and_then(|l| self.syntax_set.find_syntax_by_first_line(&l))
    }
}

#[cfg(test)]
mod tests {
    use std::ffi::OsStr;
    use std::fs::File;
    use std::io;
    use std::io::Write;

    use tempdir::TempDir;

    use crate::assets::HighlightingAssets;
    use crate::inputfile::{InputFile, OrdinaryFile};
    use crate::syntax_mapping::{MappingTarget, SyntaxMapping};

    struct SyntaxDetectionTest<'a> {
        assets: HighlightingAssets,
        pub syntax_mapping: SyntaxMapping<'a>,
        temp_dir: TempDir,
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

        fn syntax_for_file_with_content_os(&self, file_name: &OsStr, first_line: &str) -> String {
            let file_path = self.temp_dir.path().join(file_name);
            {
                let mut temp_file = File::create(&file_path).unwrap();
                writeln!(temp_file, "{}", first_line).unwrap();
            }

            let input_file = InputFile::Ordinary(OrdinaryFile::from_path(file_path.as_os_str()));
            let syntax = self.assets.get_syntax(
                None,
                input_file,
                &mut input_file.get_reader(io::stdin().lock()).unwrap(),
                &self.syntax_mapping,
            );

            syntax.name.clone()
        }

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
            let input_file = InputFile::StdIn(Some(OsStr::new(file_name)));
            let syntax = self.assets.get_syntax(
                None,
                input_file,
                &mut input_file.get_reader(content).unwrap(),
                &self.syntax_mapping,
            );
            syntax.name.clone()
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
    fn syntax_detection_well_defined_mapping_for_duplicate_extensions() {
        let test = SyntaxDetectionTest::new();

        assert_eq!(test.syntax_for_file("test.h"), "C++");
        assert_eq!(test.syntax_for_file("test.sass"), "Sass");
        assert_eq!(test.syntax_for_file("test.hs"), "Haskell (improved)");
        assert_eq!(test.syntax_for_file("test.js"), "JavaScript (Babel)");
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
}

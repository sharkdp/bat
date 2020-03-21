use std::collections::BTreeMap;
use std::fs::{self, File};
use std::io::BufReader;
use std::path::Path;

use syntect::dumps::{dump_to_file, from_binary, from_reader};
use syntect::highlighting::{Theme, ThemeSet};
use syntect::parsing::{SyntaxReference, SyntaxSet, SyntaxSetBuilder};

use crate::errors::*;
use crate::inputfile::{InputFile, InputFileReader};
use crate::syntax_mapping::SyntaxMapping;

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
                use ansi_term::Colour::Yellow;
                eprintln!(
                    "{}: Unknown theme '{}', using default.",
                    Yellow.paint("[bat warning]"),
                    theme
                );
                &self.theme_set.themes[self.fallback_theme.unwrap_or(Self::default_theme())]
            }
        }
    }

    pub(crate) fn get_syntax(
        &self,
        language: Option<&str>,
        filename: InputFile,
        reader: &mut InputFileReader,
        mapping: &SyntaxMapping,
    ) -> &SyntaxReference {
        let syntax = match (language, filename) {
            (Some(language), _) => self.syntax_set.find_syntax_by_token(language),
            (None, InputFile::Ordinary(filename)) => {
                let path = Path::new(filename);
                let file_name = path.file_name().and_then(|n| n.to_str()).unwrap_or("");
                let extension = path.extension().and_then(|x| x.to_str()).unwrap_or("");

                let file_name = mapping.replace(file_name);
                let extension = mapping.replace(extension);

                let ext_syntax = self
                    .syntax_set
                    .find_syntax_by_extension(&file_name)
                    .or_else(|| self.syntax_set.find_syntax_by_extension(&extension));
                let line_syntax = if ext_syntax.is_none() {
                    String::from_utf8(reader.first_line.clone())
                        .ok()
                        .and_then(|l| self.syntax_set.find_syntax_by_first_line(&l))
                } else {
                    None
                };

                ext_syntax.or(line_syntax)
            }
            (None, InputFile::StdIn) => String::from_utf8(reader.first_line.clone())
                .ok()
                .and_then(|l| self.syntax_set.find_syntax_by_first_line(&l)),
            (_, InputFile::ThemePreviewFile) => self.syntax_set.find_syntax_by_name("Rust"),
        };

        syntax.unwrap_or_else(|| self.syntax_set.find_syntax_plain_text())
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
    use crate::inputfile::InputFile;
    use crate::syntax_mapping::SyntaxMapping;

    struct SyntaxDetectionTest {
        assets: HighlightingAssets,
        pub syntax_mapping: SyntaxMapping,
        temp_dir: TempDir,
    }

    impl SyntaxDetectionTest {
        fn new() -> Self {
            SyntaxDetectionTest {
                assets: HighlightingAssets::from_binary(),
                syntax_mapping: SyntaxMapping::new(),
                temp_dir: TempDir::new("bat_syntax_detection_tests")
                    .expect("creation of temporary directory"),
            }
        }

        fn syntax_name_with_content(&self, file_name: &str, first_line: &str) -> String {
            let file_path = self.temp_dir.path().join(file_name);
            {
                let mut temp_file = File::create(&file_path).unwrap();
                writeln!(temp_file, "{}", first_line).unwrap();
            }

            let input_file = InputFile::Ordinary(OsStr::new(&file_path));
            let syntax = self.assets.get_syntax(
                None,
                input_file,
                &mut input_file.get_reader(&io::stdin()).unwrap(),
                &self.syntax_mapping,
            );

            syntax.name.clone()
        }

        fn syntax_name(&self, file_name: &str) -> String {
            self.syntax_name_with_content(file_name, "")
        }
    }

    #[test]
    fn syntax_detection_basic() {
        let test = SyntaxDetectionTest::new();

        assert_eq!(test.syntax_name("test.rs"), "Rust");
        assert_eq!(test.syntax_name("test.cpp"), "C++");
        assert_eq!(test.syntax_name("PKGBUILD"), "Bourne Again Shell (bash)");
    }

    #[test]
    fn syntax_detection_well_defined_mapping_for_duplicate_extensions() {
        let test = SyntaxDetectionTest::new();

        assert_eq!(test.syntax_name("test.sass"), "Sass");
        // TODO: make these tests pass:
        // assert_eq!(test.syntax_name("test.h"), "C");
        // assert_eq!(test.syntax_name("test.hs"), "Haskell (Improved)");
        // assert_eq!(test.syntax_name("test.js"), "JavaScript (Babel)");
    }

    #[test]
    fn syntax_detection_first_line() {
        let test = SyntaxDetectionTest::new();

        assert_eq!(
            test.syntax_name_with_content("my_script", "#!/bin/bash"),
            "Bourne Again Shell (bash)"
        );
        assert_eq!(test.syntax_name_with_content("my_script", "<?php"), "PHP");
    }

    #[test]
    fn syntax_detection_with_custom_mapping() {
        let mut test = SyntaxDetectionTest::new();

        assert_ne!(test.syntax_name("test.h"), "C++");
        test.syntax_mapping.insert("h", "cpp");
        assert_eq!(test.syntax_name("test.h"), "C++");
    }
}

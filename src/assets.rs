use std::borrow::Cow;
use std::collections::BTreeMap;
use std::fs::{self, File};
use std::io::BufReader;
use std::path::{Path, PathBuf};

use syntect::dumps::{dump_to_file, from_binary, from_reader};
use syntect::highlighting::{Theme, ThemeSet};
use syntect::parsing::{SyntaxReference, SyntaxSet, SyntaxSetBuilder};

use dirs::PROJECT_DIRS;

use errors::*;
use inputfile::{InputFile, InputFileReader};
use syntax_mapping::SyntaxMapping;

pub const BAT_THEME_DEFAULT: &str = "Monokai Extended";

pub struct HighlightingAssets {
    pub syntax_set: SyntaxSet,
    pub theme_set: ThemeSet,
}

impl HighlightingAssets {
    pub fn new() -> Self {
        Self::from_cache().unwrap_or_else(|_| Self::from_binary())
    }

    pub fn from_files(dir: Option<&Path>, start_empty: bool) -> Result<Self> {
        let source_dir = dir.unwrap_or_else(|| PROJECT_DIRS.config_dir());

        let mut theme_set = if start_empty {
            ThemeSet {
                themes: BTreeMap::new(),
            }
        } else {
            Self::get_integrated_themeset()
        };

        let theme_dir = source_dir.join("themes");

        let res = theme_set.add_from_folder(&theme_dir);
        if !res.is_ok() {
            println!(
                "No themes were found in '{}', using the default set",
                theme_dir.to_string_lossy()
            );
        }

        let mut syntax_set_builder = if start_empty {
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
        })
    }

    fn from_cache() -> Result<Self> {
        let theme_set_path = theme_set_path();
        let syntax_set_file = File::open(&syntax_set_path()).chain_err(|| {
            format!(
                "Could not load cached syntax set '{}'",
                syntax_set_path().to_string_lossy()
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
        })
    }

    fn get_integrated_syntaxset() -> SyntaxSet {
        from_binary(include_bytes!("../assets/syntaxes.bin"))
    }

    fn get_integrated_themeset() -> ThemeSet {
        from_binary(include_bytes!("../assets/themes.bin"))
    }

    fn from_binary() -> Self {
        let syntax_set = Self::get_integrated_syntaxset();
        let theme_set = Self::get_integrated_themeset();

        HighlightingAssets {
            syntax_set,
            theme_set,
        }
    }

    pub fn save(&self, dir: Option<&Path>) -> Result<()> {
        let target_dir = dir.unwrap_or_else(|| PROJECT_DIRS.cache_dir());
        let _ = fs::create_dir(target_dir);
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

    pub fn get_theme(&self, theme: &str) -> &Theme {
        match self.theme_set.themes.get(theme) {
            Some(theme) => theme,
            None => {
                use ansi_term::Colour::Yellow;
                eprintln!(
                    "{}: Unknown theme '{}', using default.",
                    Yellow.paint("[bat warning]"),
                    theme
                );
                &self.theme_set.themes[BAT_THEME_DEFAULT]
            }
        }
    }

    pub fn get_syntax(
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
                let syntax = ext_syntax.or(line_syntax);
                syntax
            }
            (None, InputFile::StdIn) => String::from_utf8(reader.first_line.clone())
                .ok()
                .and_then(|l| self.syntax_set.find_syntax_by_first_line(&l)),
            (_, InputFile::ThemePreviewFile) => self.syntax_set.find_syntax_by_name("Rust"),
        };

        syntax.unwrap_or_else(|| self.syntax_set.find_syntax_plain_text())
    }
}

fn theme_set_path() -> PathBuf {
    PROJECT_DIRS.cache_dir().join("themes.bin")
}

fn syntax_set_path() -> PathBuf {
    PROJECT_DIRS.cache_dir().join("syntaxes.bin")
}

pub fn config_dir() -> Cow<'static, str> {
    PROJECT_DIRS.config_dir().to_string_lossy()
}

pub fn clear_assets() {
    print!("Clearing theme set cache ... ");
    fs::remove_file(theme_set_path()).ok();
    println!("okay");

    print!("Clearing syntax set cache ... ");
    fs::remove_file(syntax_set_path()).ok();
    println!("okay");
}

use std::ffi::OsStr;
use std::fs::File;
use std::io;
use std::io::Write;

use tempdir::TempDir;

use bat::assets::HighlightingAssets;
use bat::inputfile::InputFile;
use bat::syntax_mapping::SyntaxMapping;

struct SyntaxDetectionTest {
    assets: HighlightingAssets,
    pub syntax_mapping: SyntaxMapping,
    temp_dir: TempDir,
}

impl SyntaxDetectionTest {
    fn new() -> Self {
        SyntaxDetectionTest {
            assets: HighlightingAssets::new(),
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

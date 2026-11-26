mod utils;
use utils::command::bat;

#[test]
fn file_details_report_source_path_time_and_permissions() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("example.json");
    std::fs::write(&path, "{}\n").unwrap();
    let file = std::fs::File::options().write(true).open(&path).unwrap();
    file.set_times(std::fs::FileTimes::new().set_modified(std::time::UNIX_EPOCH))
        .unwrap();
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        file.set_permissions(std::fs::Permissions::from_mode(0o640))
            .unwrap();
    }
    drop(file);
    let output = bat()
        .args([
            "--style=header-filename,header-path,header-modified,header-permissions",
            "--decorations=always",
            "--color=never",
            "--terminal-width=240",
            "--file-name=virtual.json",
            "-r0:0",
        ])
        .arg(&path)
        .assert()
        .success()
        .get_output()
        .stdout
        .clone();
    let output = String::from_utf8(output).unwrap();
    assert!(output.contains("File: virtual.json\n"), "{output}");
    let rendered_path = output
        .lines()
        .find_map(|line| line.strip_prefix("Path: "))
        .expect("the header should include the source path");
    let rendered_path = std::path::Path::new(rendered_path);
    assert!(rendered_path.is_absolute(), "{output}");
    // Windows may render an extended-length path prefix. Compare the actual
    // source files rather than the spelling of their equivalent paths.
    assert_eq!(
        rendered_path.canonicalize().unwrap(),
        path.canonicalize().unwrap(),
        "{output}"
    );
    assert!(
        output.contains("Modified: 1970-01-01 00:00:00 UTC\n"),
        "{output}"
    );
    #[cfg(unix)]
    assert!(output.contains("Permissions: rw-r-----\n"), "{output}");
    #[cfg(not(unix))]
    assert!(output.contains("Permissions: read-write\n"), "{output}");
}

#[test]
fn full_style_includes_details_while_default_does_not() {
    for (style, present) in [("full", true), ("default", false)] {
        let output = bat()
            .arg(format!("--style={style}"))
            .args([
                "--decorations=always",
                "--color=never",
                "--terminal-width=80",
            ])
            .write_stdin("input\n")
            .assert()
            .success()
            .get_output()
            .stdout
            .clone();
        let output = String::from_utf8(output).unwrap();
        for field in ["Path: -", "Modified: -", "Permissions: -"] {
            assert_eq!(output.contains(field), present, "{style}: {output}");
        }
    }
}

#[test]
fn library_callers_can_select_individual_metadata_fields() {
    let mut output = String::new();
    bat::PrettyPrinter::new()
        .input_from_bytes(b"input\n")
        .colored_output(false)
        .header_path(true)
        .header_modified(true)
        .header_permissions(true)
        .print_with_writer(Some(&mut output))
        .unwrap();
    assert_eq!(output, "Path: -\nModified: -\nPermissions: -\ninput\n");
}

mod utils;

use std::path::Path;
use utils::command::bat_with_config;

fn command(directory: &Path) -> assert_cmd::Command {
    let mut command = bat_with_config();
    command
        .current_dir(directory)
        .env("BAT_CONFIG_PATH", directory.join("user-config"))
        .env("BAT_CACHE_PATH", directory.join("cache"))
        .args([
            "--paging=never",
            "--color=always",
            "--style=plain",
            "--theme=Monokai Extended",
        ]);
    command
}

fn output(directory: &Path, args: &[&str]) -> Vec<u8> {
    command(directory)
        .args(args)
        .write_stdin("fn main() { println!(\"hello\"); }\n")
        .assert()
        .success()
        .get_output()
        .stdout
        .clone()
}

#[test]
fn project_mappings_are_opt_in_and_inherited_in_directory_order() {
    let root = tempfile::tempdir().unwrap();
    let child = root.path().join("src/nested");
    std::fs::create_dir_all(&child).unwrap();
    std::fs::write(
        root.path().join(".batconfig"),
        "# Shared project mappings\n--map-syntax '*.data:JSON'\n--map-syntax '*.parent:Python'\n",
    )
    .unwrap();
    std::fs::write(child.join(".batconfig"), "--map-syntax '*.data:Rust'\n").unwrap();

    let rust = output(&child, &["--language=Rust"]);
    let plain = output(&child, &["--language=txt"]);
    assert_ne!(rust, plain);
    assert_eq!(output(&child, &["--file-name=input.data"]), plain);
    assert_eq!(
        output(&child, &["--local-config", "--file-name=input.data"]),
        rust
    );
    assert_eq!(
        output(&child, &["--local-config", "--file-name=input.parent"]),
        output(&child, &["--language=Python"])
    );
    assert_eq!(
        output(root.path(), &["--local-config", "--file-name=input.data"]),
        output(root.path(), &["--language=JSON"])
    );
}

#[test]
fn local_config_precedence_and_no_config_are_respected() {
    let root = tempfile::tempdir().unwrap();
    std::fs::write(root.path().join("user-config"), "--language=JSON\n").unwrap();
    std::fs::write(root.path().join(".batconfig"), "--language=Rust\n").unwrap();
    assert_eq!(
        output(root.path(), &["--local-config"]),
        output(root.path(), &["--language=Rust"])
    );
    assert_eq!(
        output(root.path(), &["--local-config", "--language=Python"]),
        output(root.path(), &["--language=Python"])
    );
    assert_eq!(
        output(root.path(), &["--local-config", "--no-config"]),
        output(root.path(), &["--no-config", "--language=txt"])
    );
    command(root.path())
        .env("BAT_OPTS", "--language=Python")
        .arg("--local-config")
        .write_stdin("fn main() { println!(\"hello\"); }\n")
        .assert()
        .success()
        .stdout(output(root.path(), &["--language=Rust"]));
}

#[test]
fn explicit_environment_settings_override_project_settings() {
    let root = tempfile::tempdir().unwrap();
    std::fs::write(root.path().join(".batconfig"), "--tabs=8\n").unwrap();
    command(root.path())
        .env("BAT_TABS", "2")
        .args(["--local-config", "--color=never", "--decorations=always"])
        .write_stdin("x\ty\n")
        .assert()
        .success()
        .stdout("x y\n");
}

#[test]
fn malformed_project_config_reports_its_path_and_help_remains_available() {
    let root = tempfile::tempdir().unwrap();
    std::fs::write(root.path().join(".batconfig"), "--theme='unfinished\n").unwrap();
    command(root.path())
        .arg("--local-config")
        .write_stdin("hello\n")
        .assert()
        .failure()
        .stdout("")
        .stderr(predicates::str::contains(".batconfig"));
    for bypass in ["--no-config", "--help", "--version"] {
        command(root.path())
            .args(["--local-config", bypass])
            .write_stdin("hello\n")
            .assert()
            .success();
    }
    output(root.path(), &[]);
    std::fs::remove_file(root.path().join(".batconfig")).unwrap();
    std::fs::create_dir(root.path().join(".batconfig")).unwrap();
    command(root.path())
        .arg("--local-config")
        .assert()
        .failure()
        .stderr(predicates::str::contains(
            "Could not read local configuration",
        ));
}

#[test]
fn local_config_requires_a_real_command_line_flag() {
    let root = tempfile::tempdir().unwrap();
    std::fs::write(root.path().join(".batconfig"), "--language=Rust\n").unwrap();
    std::fs::write(root.path().join("user-config"), "--local-config\n").unwrap();
    assert_eq!(
        output(root.path(), &[]),
        output(root.path(), &["--language=txt"])
    );
    assert_eq!(
        output(root.path(), &["--file-name=--local-config"]),
        output(root.path(), &["--language=txt"])
    );
    std::fs::write(root.path().join("--local-config"), "ordinary file\n").unwrap();
    command(root.path())
        .args(["--color=never", "--", "--local-config"])
        .assert()
        .success()
        .stdout("ordinary file\n");
}

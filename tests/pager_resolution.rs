#![cfg(feature = "paging")]

mod utils;
use utils::command::bat;

#[test]
fn missing_pager_falls_back_to_stdout() {
    bat()
        .args([
            "--style=plain",
            "--wrap=never",
            "--paging=always",
            "--pager=bat-test-missing-pager-2027",
        ])
        .write_stdin("input\n")
        .assert()
        .success()
        .stdout("input\n")
        .stderr(predicates::str::contains("outputting to stdout instead"));
}

#[cfg(windows)]
mod windows {
    use super::bat;
    use std::fs;
    use std::path::Path;
    use tempfile::tempdir;

    fn install_command(path: &Path) {
        let system_root = std::env::var_os("SystemRoot").expect("Windows system directory");
        fs::copy(Path::new(&system_root).join("System32/cmd.exe"), path).unwrap();
    }

    fn command(pager: &str, path: &Path, cwd: &Path) -> assert_cmd::Command {
        let mut command = bat();
        command
            .current_dir(cwd)
            .env("PATH", path)
            .args([
                "--style=plain",
                "--wrap=never",
                "--paging=always",
                "--pager",
            ])
            .arg(format!("\"{pager}\" /D /C echo pager-launched"))
            .write_stdin("input\n");
        command
    }

    #[test]
    fn working_directory_is_not_an_implicit_pager_search_path() {
        let root = tempdir().unwrap();
        let path = root.path().join("empty-path");
        fs::create_dir(&path).unwrap();
        for extension in ["exe", "com"] {
            install_command(&root.path().join(format!("bat-test-pager.{extension}")));
        }
        command("bat-test-pager", &path, root.path())
            .assert()
            .success()
            .stdout("input\n")
            .stderr(predicates::str::contains("outputting to stdout instead"));
    }

    #[test]
    fn path_resolves_native_and_com_pagers() {
        for extension in ["exe", "com"] {
            let root = tempdir().unwrap();
            let path = root.path().join("binary directory with spaces");
            fs::create_dir(&path).unwrap();
            install_command(&path.join(format!("bat-test-pager.{extension}")));
            command("bat-test-pager", &path, root.path())
                .assert()
                .success()
                .stdout("pager-launched\r\n");
        }
    }

    #[test]
    fn explicit_relative_and_absolute_paths_remain_available() {
        let root = tempdir().unwrap();
        let path = root.path().join("binary directory with spaces");
        fs::create_dir(&path).unwrap();
        for extension in ["exe", "com"] {
            let relative = format!(".\\binary directory with spaces\\bat-test-pager.{extension}");
            let absolute = path.join(format!("bat-test-pager.{extension}"));
            install_command(&absolute);
            for program in [relative.as_str(), absolute.to_str().unwrap()] {
                command(program, root.path(), root.path())
                    .assert()
                    .success()
                    .stdout("pager-launched\r\n");
            }
        }
    }

    #[test]
    fn explicit_extension_does_not_fall_back_to_com() {
        let root = tempdir().unwrap();
        let path = root.path().join("bin");
        fs::create_dir(&path).unwrap();
        install_command(&path.join("bat-test-pager.com"));
        command("bat-test-pager.exe", &path, root.path())
            .assert()
            .success()
            .stdout("input\n")
            .stderr(predicates::str::contains("outputting to stdout instead"));
    }
}

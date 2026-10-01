#![cfg(all(unix, feature = "paging"))]

use std::fs;
use std::os::unix::fs::PermissionsExt;
use std::path::{Path, PathBuf};
use tempfile::{tempdir, TempDir};

mod utils;
use utils::command::bat;

struct Pager {
    directory: TempDir,
    arguments: PathBuf,
}

impl Pager {
    fn new(name: &str, busybox: bool) -> Self {
        let directory = tempdir().unwrap();
        let script = directory.path().join(name);
        let version = if busybox {
            "printf 'BusyBox v1.35.0\n' >&2; exit 1"
        } else {
            "printf 'less 590\n'; exit 0"
        };
        fs::write(
            &script,
            format!(
                "#!/bin/sh\nif [ \"$1\" = --version ]; then {version}; fi\nprintf '%s\\n' \"$@\" > \"$BAT_TEST_PAGER_ARGS\"\ncat\n"
            ),
        )
        .unwrap();
        fs::set_permissions(&script, fs::Permissions::from_mode(0o700)).unwrap();
        let arguments = directory.path().join("arguments");
        Self {
            directory,
            arguments,
        }
    }

    fn command(&self, name: &str, options: &[&str]) -> assert_cmd::Command {
        let binary = self.directory.path().join(name);
        let pager = shell_words::join(
            std::iter::once(binary.to_str().unwrap()).chain(options.iter().copied()),
        );
        let mut command = bat();
        command
            .args(["--paging=always", "--style=plain", "--pager", &pager])
            .env_remove("LESS")
            .env("BAT_TEST_PAGER_ARGS", &self.arguments);
        command
    }

    fn args(&self) -> Vec<String> {
        fs::read_to_string(&self.arguments)
            .unwrap()
            .lines()
            .filter(|argument| *argument != "--no-lessopen")
            .map(str::to_owned)
            .collect()
    }
}

#[test]
fn less_receives_literal_filename_for_all_prompt_views() {
    let pager = Pager::new("less", false);
    pager
        .command("less", &["-RFM"])
        .args(["--file-name", "file?.:%\\$ name\n\t\x1b.txt", "test.txt"])
        .assert()
        .success()
        .stdout("hello world\n");
    let args = pager.args();
    let values: Vec<_> = args
        .windows(2)
        .filter(|pair| pair[0] == "-P")
        .map(|pair| &pair[1])
        .collect();
    assert_eq!(values.len(), 4);
    for (value, selector) in values.iter().zip(['s', 'm', 'M', '=']) {
        assert!(value.starts_with(selector));
        assert!(
            value.contains(r"file\?\.\:\%\\$ name^J^I^[\.txt"),
            "{value:?}"
        );
        assert!(!value.contains('\x1b'));
    }
    assert_eq!(args.last().unwrap(), "-RFM");
}

#[test]
fn less_uses_named_stdin_but_not_anonymous_or_multiple_inputs() {
    let pager = Pager::new("less", false);
    pager
        .command("less", &["-R"])
        .args(["--file-name", "named.txt"])
        .write_stdin("input\n")
        .assert()
        .success()
        .stdout("input\n");
    assert!(pager.args().iter().any(|arg| arg.contains(r"named\.txt")));
    pager
        .command("less", &["-R"])
        .write_stdin("input\n")
        .assert()
        .success()
        .stdout("input\n");
    assert_eq!(pager.args(), ["-R"]);
    pager
        .command("less", &["-R"])
        .args(["test.txt", "test.txt"])
        .assert()
        .success()
        .stdout("hello world\nhello world\n");
    assert_eq!(pager.args(), ["-R"]);
}

#[test]
fn explicit_less_prompt_follows_generated_defaults() {
    let pager = Pager::new("less", false);
    pager
        .command("less", &["-R", "-P", "Mcustom prompt"])
        .arg("test.txt")
        .assert()
        .success();
    assert!(pager
        .args()
        .ends_with(&["-R".into(), "-P".into(), "Mcustom prompt".into()]));
}

#[test]
fn less_environment_prompt_is_preserved() {
    let pager = Pager::new("less", false);
    for options in ["-R -PMcustom", "RMPcustom", "--prompt=custom"] {
        pager
            .command("less", &["-R"])
            .env("LESS", options)
            .arg("test.txt")
            .assert()
            .success();
        assert_eq!(pager.args(), ["-R"]);
    }
}

#[test]
fn busybox_and_other_pagers_receive_no_prompt_options() {
    for (name, busybox) in [("less", true), ("other-pager", false)] {
        let pager = Pager::new(name, busybox);
        pager
            .command(name, &["-R"])
            .arg("test.txt")
            .assert()
            .success();
        assert_eq!(pager.args(), ["-R"]);
    }
}

#[test]
fn unpaged_output_does_not_launch_the_pager() {
    let pager = Pager::new("less", false);
    pager
        .command("less", &["-R"])
        .args(["--paging=never", "test.txt"])
        .assert()
        .success()
        .stdout("hello world\n");
    assert!(!Path::new(&pager.arguments).exists());
}

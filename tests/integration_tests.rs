use predicates::boolean::PredicateBooleanExt;
use predicates::{prelude::predicate, str::PredicateStrExt};
use serial_test::serial;
use std::path::Path;
use std::str::from_utf8;
use tempfile::tempdir;

#[cfg(unix)]
mod unix {
    pub use std::fs::File;
    pub use std::io::{self, Write};
    pub use std::path::PathBuf;
    pub use std::process::Stdio;
    pub use std::thread;
    pub use std::time::Duration;

    pub use assert_cmd::assert::OutputAssertExt;
    pub use nix::pty::{openpty, OpenptyResult};
    pub use wait_timeout::ChildExt;

    pub const SAFE_CHILD_PROCESS_CREATION_TIME: Duration = Duration::from_millis(100);
    pub const CHILD_WAIT_TIMEOUT: Duration = Duration::from_secs(15);
}
#[cfg(unix)]
use unix::*;

mod utils;
use utils::command::{bat, bat_with_config};

#[cfg(unix)]
use utils::command::bat_raw_command;
use utils::mocked_pagers;

const EXAMPLES_DIR: &str = "tests/examples";

fn get_config() -> &'static str {
    if cfg!(windows) {
        "bat-windows.conf"
    } else {
        "bat.conf"
    }
}

#[test]
fn basic() {
    bat()
        .arg("test.txt")
        .assert()
        .success()
        .stdout("hello world\n")
        .stderr("");
}

#[test]
fn stdin() {
    bat()
        .write_stdin("foo\nbar\n")
        .assert()
        .success()
        .stdout("foo\nbar\n");
}

#[test]
fn concatenate() {
    bat()
        .arg("test.txt")
        .arg("test.txt")
        .assert()
        .success()
        .stdout("hello world\nhello world\n");
}

#[test]
fn concatenate_stdin() {
    bat()
        .arg("test.txt")
        .arg("-")
        .arg("test.txt")
        .write_stdin("stdin\n")
        .assert()
        .success()
        .stdout("hello world\nstdin\nhello world\n");
}

#[test]
fn concatenate_empty_first() {
    bat()
        .arg("empty.txt")
        .arg("test.txt")
        .assert()
        .success()
        .stdout("hello world\n");
}

#[test]
fn concatenate_empty_last() {
    bat()
        .arg("test.txt")
        .arg("empty.txt")
        .assert()
        .success()
        .stdout("hello world\n");
}

#[test]
fn concatenate_empty_both() {
    bat()
        .arg("empty.txt")
        .arg("empty.txt")
        .assert()
        .success()
        .stdout("");
}

#[test]
fn concatenate_empty_between() {
    bat()
        .arg("test.txt")
        .arg("empty.txt")
        .arg("test.txt")
        .assert()
        .success()
        .stdout("hello world\nhello world\n");
}

#[test]
fn concatenate_empty_first_and_last() {
    bat()
        .arg("empty.txt")
        .arg("test.txt")
        .arg("empty.txt")
        .assert()
        .success()
        .stdout("hello world\n");
}

#[test]
fn concatenate_single_line() {
    bat()
        .arg("single-line.txt")
        .arg("single-line.txt")
        .assert()
        .success()
        .stdout("Single LineSingle Line");
}

#[test]
fn concatenate_single_line_empty() {
    bat()
        .arg("single-line.txt")
        .arg("empty.txt")
        .arg("single-line.txt")
        .assert()
        .success()
        .stdout("Single LineSingle Line");
}

#[test]
fn line_numbers() {
    bat()
        .arg("multiline.txt")
        .arg("--style=numbers")
        .arg("--decorations=always")
        .assert()
        .success()
        .stdout("   1 line 1\n   2 line 2\n   3 line 3\n   4 line 4\n   5 line 5\n   6 line 6\n   7 line 7\n   8 line 8\n   9 line 9\n  10 line 10\n");
}

#[test]
fn line_range_2_3() {
    bat()
        .arg("multiline.txt")
        .arg("--line-range=2:3")
        .assert()
        .success()
        .stdout("line 2\nline 3\n");
}

#[test]
fn line_range_up_to_2_from_back() {
    bat()
        .arg("multiline.txt")
        .arg("--line-range=:-2")
        .assert()
        .success()
        .stdout("line 1\nline 2\nline 3\nline 4\nline 5\nline 6\nline 7\nline 8\n");
}

#[test]
fn line_range_up_to_2_from_back_single_line_is_empty() {
    bat()
        .arg("single-line.txt")
        .arg("--line-range=:-2")
        .assert()
        .success()
        .stdout("");
}

#[test]
fn line_range_from_back_last_two() {
    bat()
        .arg("multiline.txt")
        .arg("--line-range=-2:")
        .assert()
        .success()
        .stdout("line 9\nline 10\n");
}

#[test]
fn line_range_from_back_last_two_single_line() {
    bat()
        .arg("single-line.txt")
        .arg("--line-range=-2:")
        .assert()
        .success()
        .stdout("Single Line");
}

#[test]
fn line_range_first_two() {
    bat()
        .arg("multiline.txt")
        .arg("--line-range=:2")
        .assert()
        .success()
        .stdout("line 1\nline 2\n");
}

#[test]
fn line_range_last_3() {
    bat()
        .arg("multiline.txt")
        .arg("--line-range=8:")
        .assert()
        .success()
        .stdout("line 8\nline 9\nline 10\n");
}

#[test]
fn line_range_multiple() {
    bat()
        .arg("multiline.txt")
        .arg("--line-range=1:2")
        .arg("--line-range=4:4")
        .assert()
        .success()
        .stdout("line 1\nline 2\nline 4\n");
}

#[test]
fn line_range_multiple_with_context() {
    bat()
        .arg("multiline.txt")
        .arg("--line-range=2::1")
        .arg("--line-range=8::1")
        .assert()
        .success()
        .stdout("line 1\nline 2\nline 3\nline 7\nline 8\nline 9\n");
}

#[test]
fn line_range_context_around_single_line() {
    bat()
        .arg("multiline.txt")
        .arg("--line-range=5::2")
        .assert()
        .success()
        .stdout("line 3\nline 4\nline 5\nline 6\nline 7\n");
}

#[test]
fn line_range_context_around_single_line_minimal() {
    bat()
        .arg("multiline.txt")
        .arg("--line-range=5::1")
        .assert()
        .success()
        .stdout("line 4\nline 5\nline 6\n");
}

#[test]
fn line_range_context_around_range() {
    bat()
        .arg("multiline.txt")
        .arg("--line-range=4:6:2")
        .assert()
        .success()
        .stdout("line 2\nline 3\nline 4\nline 5\nline 6\nline 7\nline 8\n");
}

#[test]
fn line_range_context_at_file_boundaries() {
    bat()
        .arg("multiline.txt")
        .arg("--line-range=1::2")
        .assert()
        .success()
        .stdout("line 1\nline 2\nline 3\n");
}

#[test]
fn line_range_context_at_end_of_file() {
    bat()
        .arg("multiline.txt")
        .arg("--line-range=10::2")
        .assert()
        .success()
        .stdout("line 8\nline 9\nline 10\n");
}

#[test]
fn line_range_context_zero() {
    bat()
        .arg("multiline.txt")
        .arg("--line-range=5::0")
        .assert()
        .success()
        .stdout("line 5\n");
}

#[test]
fn line_range_context_negative_single_line() {
    bat()
        .arg("multiline.txt")
        .arg("--line-range=5::-1")
        .assert()
        .failure()
        .stderr(predicate::str::contains(
            "Invalid context number in N::C format",
        ));
}

#[test]
fn line_range_context_negative_range() {
    bat()
        .arg("multiline.txt")
        .arg("--line-range=5:6:-1")
        .assert()
        .failure()
        .stderr(predicate::str::contains(
            "Invalid context number in N:M:C format",
        ));
}

#[test]
fn line_range_context_non_numeric_single_line() {
    bat()
        .arg("multiline.txt")
        .arg("--line-range=10::abc")
        .assert()
        .failure()
        .stderr(predicate::str::contains(
            "Invalid context number in N::C format",
        ));
}

#[test]
fn line_range_context_non_numeric_range() {
    bat()
        .arg("multiline.txt")
        .arg("--line-range=10:12:xyz")
        .assert()
        .failure()
        .stderr(predicate::str::contains(
            "Invalid context number in N:M:C format",
        ));
}

#[test]
fn line_range_context_very_large() {
    bat()
        .arg("multiline.txt")
        .arg("--line-range=10::999999")
        .assert()
        .success()
        .stdout(
            "line 1\nline 2\nline 3\nline 4\nline 5\nline 6\nline 7\nline 8\nline 9\nline 10\n",
        );
}

#[test]
fn squeeze_blank() {
    bat()
        .arg("empty_lines.txt")
        .arg("--squeeze-blank")
        .assert()
        .success()
        .stdout("line 1\n\nline 5\n\nline 20\nline 21\n\nline 24\n\nline 26\n\nline 30\n");
}

#[test]
fn squeeze_blank_line_numbers() {
    bat()
        .arg("empty_lines.txt")
        .arg("--squeeze-blank")
        .arg("--decorations=always")
        .arg("--number")
        .assert()
        .success()
        .stdout("   1 line 1\n   2 \n   5 line 5\n   6 \n  20 line 20\n  21 line 21\n  22 \n  24 line 24\n  25 \n  26 line 26\n  27 \n  30 line 30\n");
}

#[test]
fn squeeze_limit() {
    bat()
        .arg("empty_lines.txt")
        .arg("--squeeze-blank")
        .arg("--squeeze-limit=2")
        .assert()
        .success()
        .stdout("line 1\n\n\nline 5\n\n\nline 20\nline 21\n\n\nline 24\n\nline 26\n\n\nline 30\n");

    bat()
        .arg("empty_lines.txt")
        .arg("--squeeze-blank")
        .arg("--squeeze-limit=5")
        .assert()
        .success()
        .stdout("line 1\n\n\n\nline 5\n\n\n\n\n\nline 20\nline 21\n\n\nline 24\n\nline 26\n\n\n\nline 30\n");
}

#[test]
fn squeeze_limit_line_numbers() {
    bat()
        .arg("empty_lines.txt")
        .arg("--squeeze-blank")
        .arg("--squeeze-limit=2")
        .arg("--decorations=always")
        .arg("--number")
        .assert()
        .success()
        .stdout("   1 line 1\n   2 \n   3 \n   5 line 5\n   6 \n   7 \n  20 line 20\n  21 line 21\n  22 \n  23 \n  24 line 24\n  25 \n  26 line 26\n  27 \n  28 \n  30 line 30\n");

    bat()
        .arg("empty_lines.txt")
        .arg("--squeeze-blank")
        .arg("--squeeze-limit=5")
        .arg("--decorations=always")
        .arg("--number")
        .assert()
        .success()
        .stdout("   1 line 1\n   2 \n   3 \n   4 \n   5 line 5\n   6 \n   7 \n   8 \n   9 \n  10 \n  20 line 20\n  21 line 21\n  22 \n  23 \n  24 line 24\n  25 \n  26 line 26\n  27 \n  28 \n  29 \n  30 line 30\n");
}

#[test]
fn list_themes_with_colors() {
    let default_theme_chunk = "Monokai Extended\x1B[0m (default)";
    let default_light_theme_chunk = "Monokai Extended Light\x1B[0m (default light)";

    bat()
        .arg("--color=always")
        .arg("--list-themes")
        .assert()
        .success()
        .stdout(predicate::str::contains("DarkNeon").normalize())
        .stdout(predicate::str::contains(default_theme_chunk).normalize())
        .stdout(predicate::str::contains(default_light_theme_chunk).normalize())
        .stdout(predicate::str::contains("Output the square of a number.").normalize());
}

#[test]
fn list_themes_without_colors() {
    let default_theme_chunk = "Monokai Extended (default)";
    let default_light_theme_chunk = "Monokai Extended Light (default light)";

    bat()
        .arg("--color=never")
        .arg("--decorations=always") // trick bat into setting `Config::loop_through` to false
        .arg("--list-themes")
        .assert()
        .success()
        .stdout(predicate::str::contains("DarkNeon").normalize())
        .stdout(predicate::str::contains(default_theme_chunk).normalize())
        .stdout(predicate::str::contains(default_light_theme_chunk).normalize());
}

#[test]
fn list_themes_to_piped_output() {
    bat().arg("--list-themes").assert().success().stdout(
        predicate::str::contains("(default)")
            .not()
            .and(predicate::str::contains("(default light)").not())
            .and(predicate::str::contains("(default dark)").not()),
    );
}

#[test]
fn list_languages() {
    bat()
        .arg("--list-languages")
        .assert()
        .success()
        .stdout(predicate::str::contains("Rust").normalize());
}

#[test]
#[cfg_attr(
    any(not(feature = "git"), feature = "lessopen", target_os = "windows"),
    ignore
)]
fn short_help() {
    test_help("-h", "../doc/short-help.txt");
}

#[test]
#[cfg_attr(
    any(not(feature = "git"), feature = "lessopen", target_os = "windows"),
    ignore
)]
fn long_help() {
    test_help("--help", "../doc/long-help.txt");
}

fn test_help(arg: &str, expect_file: &str) {
    let assert = bat().arg(arg).assert();
    expect_test::expect_file![expect_file]
        .assert_eq(&String::from_utf8_lossy(&assert.get_output().stdout));
}

#[cfg(unix)]
fn setup_temp_file(content: &[u8]) -> io::Result<(PathBuf, tempfile::TempDir)> {
    let dir = tempfile::tempdir().expect("Couldn't create tempdir");
    let path = dir.path().join("temp_file");
    File::create(&path)?.write_all(content)?;
    Ok((path, dir))
}

#[cfg(unix)]
#[test]
fn basic_io_cycle() -> io::Result<()> {
    let (filename, dir) = setup_temp_file(b"I am not empty")?;

    let file_out = Stdio::from(File::create(&filename)?);
    let res = bat_raw_command()
        .arg("test.txt")
        .arg(&filename)
        .stdout(file_out)
        .assert();
    drop(dir);
    res.failure();
    Ok(())
}

#[cfg(unix)]
#[test]
fn first_file_cyclic_is_ok() -> io::Result<()> {
    let (filename, dir) = setup_temp_file(b"I am not empty")?;

    let file_out = Stdio::from(File::create(&filename)?);
    let res = bat_raw_command()
        .arg(&filename)
        .arg("test.txt")
        .stdout(file_out)
        .assert();
    drop(dir);
    res.success();
    Ok(())
}

#[cfg(unix)]
#[test]
fn empty_file_cycle_is_ok() -> io::Result<()> {
    let (filename, dir) = setup_temp_file(b"I am not empty")?;

    let file_out = Stdio::from(File::create(&filename)?);
    let res = bat_raw_command()
        .arg("empty.txt")
        .arg(&filename)
        .stdout(file_out)
        .assert();
    drop(dir);
    res.success();
    Ok(())
}

#[cfg(unix)]
#[test]
fn stdin_to_stdout_cycle() -> io::Result<()> {
    let (filename, dir) = setup_temp_file(b"I am not empty")?;
    let file_in = Stdio::from(File::open(&filename)?);
    let file_out = Stdio::from(File::create(&filename)?);
    let res = bat_raw_command()
        .arg("test.txt")
        .arg("-")
        .stdin(file_in)
        .stdout(file_out)
        .assert();
    drop(dir);
    res.failure();
    Ok(())
}

#[cfg(unix)]
#[test]
fn bat_error_to_stderr() {
    bat()
        .arg("/tmp")
        .assert()
        .failure()
        .stderr(predicate::str::contains("[bat error]"));
}

#[cfg(unix)]
#[test]
fn no_args_doesnt_break() {
    // To simulate bat getting started from the shell, a process is created with stdin and stdout
    // as the slave end of a pseudo terminal. Although both point to the same "file", bat should
    // not exit, because in this case it is safe to read and write to the same fd, which is why
    // this test exists.

    let OpenptyResult { master, slave } = openpty(None, None).expect("Couldn't open pty.");
    let mut master = File::from(master);
    let stdin_file = File::from(slave);
    let stdout_file = stdin_file.try_clone().unwrap();
    let stdin = Stdio::from(stdin_file);
    let stdout = Stdio::from(stdout_file);

    let mut child = bat_raw_command()
        .stdin(stdin)
        .stdout(stdout)
        .env("TERM", "dumb") // Suppresses color detection
        .spawn()
        .expect("Failed to start.");

    // Some time for the child process to start and to make sure, that we can poll the exit status.
    // Although this waiting period is not necessary, it is best to keep it in and be absolutely
    // sure, that the try_wait does not error later.
    thread::sleep(SAFE_CHILD_PROCESS_CREATION_TIME);

    // The child process should be running and waiting for input,
    // therefore no exit status should be available.
    let exit_status = child
        .try_wait()
        .expect("Error polling exit status, this should never happen.");
    assert!(exit_status.is_none());

    // Write Ctrl-D (end of transmission) to the pty.
    master
        .write_all(&[0x04])
        .expect("Couldn't write EOT character to master end.");

    let exit_status = child
        .wait_timeout(CHILD_WAIT_TIMEOUT)
        .expect("Error polling exit status, this should never happen.")
        .expect("Exit status not set, but the child should have exited already.");

    assert!(exit_status.success());
}

#[test]
fn tabs_numbers() {
    bat()
        .arg("tabs.txt")
        .arg("--tabs=4")
        .arg("--style=numbers")
        .arg("--decorations=always")
        .assert()
        .success()
        .stdout(
            "   1     1   2   3   4
   2 1   ?
   3 22  ?
   4 333 ?
   5 4444    ?
   6 55555   ?
   7 666666  ?
   8 7777777 ?
   9 88888888    ?
",
        );
}

#[test]
fn tabs_passthrough_wrapped() {
    bat()
        .arg("tabs.txt")
        .arg("--tabs=0")
        .arg("--style=plain")
        .arg("--decorations=always")
        .assert()
        .success()
        .stdout(
            "	1	2	3	4
1	?
22	?
333	?
4444	?
55555	?
666666	?
7777777	?
88888888	?
",
        );
}

#[test]
fn tabs_4_wrapped() {
    bat()
        .arg("tabs.txt")
        .arg("--tabs=4")
        .arg("--style=plain")
        .arg("--decorations=always")
        .assert()
        .success()
        .stdout(
            "    1   2   3   4
1   ?
22  ?
333 ?
4444    ?
55555   ?
666666  ?
7777777 ?
88888888    ?
",
        );
}

#[test]
fn tabs_8_wrapped() {
    bat()
        .arg("tabs.txt")
        .arg("--tabs=8")
        .arg("--style=plain")
        .arg("--decorations=always")
        .assert()
        .success()
        .stdout(
            "        1       2       3       4
1       ?
22      ?
333     ?
4444    ?
55555   ?
666666  ?
7777777 ?
88888888        ?
",
        );
}

#[test]
fn tabs_passthrough() {
    bat()
        .arg("tabs.txt")
        .arg("--tabs=0")
        .arg("--style=plain")
        .arg("--decorations=always")
        .assert()
        .success()
        .stdout(
            "	1	2	3	4
1	?
22	?
333	?
4444	?
55555	?
666666	?
7777777	?
88888888	?
",
        );
}

#[test]
fn tabs_4() {
    bat()
        .arg("tabs.txt")
        .arg("--tabs=4")
        .arg("--style=plain")
        .arg("--decorations=always")
        .assert()
        .success()
        .stdout(
            "    1   2   3   4
1   ?
22  ?
333 ?
4444    ?
55555   ?
666666  ?
7777777 ?
88888888    ?
",
        );
}

#[test]
fn tabs_8() {
    bat()
        .arg("tabs.txt")
        .arg("--tabs=8")
        .arg("--style=plain")
        .arg("--decorations=always")
        .assert()
        .success()
        .stdout(
            "        1       2       3       4
1       ?
22      ?
333     ?
4444    ?
55555   ?
666666  ?
7777777 ?
88888888        ?
",
        );
}

#[test]
fn tabs_4_env_overrides_config() {
    bat_with_config()
        .env("BAT_CONFIG_PATH", "bat-tabs.conf")
        .env("BAT_TABS", "4")
        .arg("tabs.txt")
        .arg("--style=plain")
        .arg("--decorations=always")
        .assert()
        .success()
        .stdout(
            "    1   2   3   4
1   ?
22  ?
333 ?
4444    ?
55555   ?
666666  ?
7777777 ?
88888888    ?
",
        );
}

#[test]
fn tabs_4_arg_overrides_env() {
    bat_with_config()
        .env("BAT_CONFIG_PATH", "bat-tabs.conf")
        .env("BAT_TABS", "6")
        .arg("tabs.txt")
        .arg("--tabs=4")
        .arg("--style=plain")
        .arg("--decorations=always")
        .assert()
        .success()
        .stdout(
            "    1   2   3   4
1   ?
22  ?
333 ?
4444    ?
55555   ?
666666  ?
7777777 ?
88888888    ?
",
        );
}

#[test]
fn tabs_4_arg_overrides_env_noconfig() {
    bat()
        .env("BAT_TABS", "6")
        .arg("tabs.txt")
        .arg("--tabs=4")
        .arg("--style=plain")
        .arg("--decorations=always")
        .assert()
        .success()
        .stdout(
            "    1   2   3   4
1   ?
22  ?
333 ?
4444    ?
55555   ?
666666  ?
7777777 ?
88888888    ?
",
        );
}

#[test]
fn fail_non_existing() {
    bat().arg("non-existing-file").assert().failure();
}

#[test]
fn fail_directory() {
    bat().arg("sub_directory").assert().failure();
}

#[test]
fn do_not_exit_directory() {
    bat()
        .arg("sub_directory")
        .arg("test.txt")
        .assert()
        .stdout("hello world\n")
        .failure();
}

#[test]
#[serial]
fn pager_basic() {
    mocked_pagers::with_mocked_versions_of_more_and_most_in_path(|| {
        bat()
            .env("PAGER", mocked_pagers::from("echo pager-output"))
            .arg("--paging=always")
            .arg("test.txt")
            .assert()
            .success()
            .stdout(predicate::str::contains("pager-output\n").normalize());
    });
}

#[test]
#[serial]
fn pager_basic_arg() {
    mocked_pagers::with_mocked_versions_of_more_and_most_in_path(|| {
        bat()
            .arg(format!(
                "--pager={}",
                mocked_pagers::from("echo pager-output")
            ))
            .arg("--paging=always")
            .arg("test.txt")
            .assert()
            .success()
            .stdout(predicate::str::contains("pager-output\n").normalize());
    });
}

#[test]
#[serial]
fn pager_overwrite() {
    mocked_pagers::with_mocked_versions_of_more_and_most_in_path(|| {
        bat()
            .env("PAGER", mocked_pagers::from("echo other-pager"))
            .env("BAT_PAGER", mocked_pagers::from("echo pager-output"))
            .arg("--paging=always")
            .arg("test.txt")
            .assert()
            .success()
            .stdout(predicate::str::contains("pager-output\n").normalize());
    });
}

#[test]
fn pager_disable() {
    bat()
        .env("PAGER", "echo other-pager")
        .env("BAT_PAGER", "")
        .arg("--paging=always")
        .arg("test.txt")
        .assert()
        .success()
        .stdout(predicate::eq("hello world\n").normalize());
}

#[test]
#[serial]
fn pager_arg_override_env_withconfig() {
    mocked_pagers::with_mocked_versions_of_more_and_most_in_path(|| {
        bat_with_config()
            .env("BAT_CONFIG_PATH", get_config())
            .env("PAGER", mocked_pagers::from("echo another-pager"))
            .env("BAT_PAGER", mocked_pagers::from("echo other-pager"))
            .arg(format!(
                "--pager={}",
                mocked_pagers::from("echo pager-output")
            ))
            .arg("--paging=always")
            .arg("test.txt")
            .assert()
            .success()
            .stdout(predicate::str::contains("pager-output\n").normalize());
    });
}

#[test]
#[serial]
fn pager_arg_override_env_noconfig() {
    mocked_pagers::with_mocked_versions_of_more_and_most_in_path(|| {
        bat()
            .env("PAGER", mocked_pagers::from("echo another-pager"))
            .env("BAT_PAGER", mocked_pagers::from("echo other-pager"))
            .arg(format!(
                "--pager={}",
                mocked_pagers::from("echo pager-output")
            ))
            .arg("--paging=always")
            .arg("test.txt")
            .assert()
            .success()
            .stdout(predicate::str::contains("pager-output\n").normalize());
    });
}

#[test]
#[serial]
fn pager_env_bat_pager_override_config() {
    mocked_pagers::with_mocked_versions_of_more_and_most_in_path(|| {
        bat_with_config()
            .env("BAT_CONFIG_PATH", get_config())
            .env("PAGER", mocked_pagers::from("echo other-pager"))
            .env("BAT_PAGER", mocked_pagers::from("echo pager-output"))
            .arg("--paging=always")
            .arg("test.txt")
            .assert()
            .success()
            .stdout(predicate::str::contains("pager-output\n").normalize());
    });
}

#[test]
#[serial]
fn pager_env_pager_nooverride_config() {
    mocked_pagers::with_mocked_versions_of_more_and_most_in_path(|| {
        bat_with_config()
            .env("BAT_CONFIG_PATH", get_config())
            .env("PAGER", mocked_pagers::from("echo other-pager"))
            .arg("--paging=always")
            .arg("test.txt")
            .assert()
            .success()
            .stdout(predicate::str::contains("dummy-pager-from-config\n").normalize());
    });
}

#[test]
fn env_var_pager_value_bat() {
    bat()
        .env("PAGER", "bat")
        .arg("--paging=always")
        .arg("test.txt")
        .assert()
        .success()
        .stdout(predicate::eq("hello world\n").normalize());
}

#[test]
fn env_var_bat_pager_value_bat() {
    bat()
        .env("BAT_PAGER", "bat")
        .arg("--paging=always")
        .arg("test.txt")
        .assert()
        .failure()
        .stderr(predicate::str::contains("bat as a pager is disallowed"));
}

#[test]
fn pager_value_bat() {
    bat()
        .arg("--pager=bat")
        .arg("--paging=always")
        .arg("test.txt")
        .assert()
        .failure()
        .stderr(predicate::str::contains("bat as a pager is disallowed"));
}

/// We shall use less instead of most if PAGER is used since PAGER
/// is a generic env var
#[test]
#[serial] // Because of PATH
fn pager_most_from_pager_env_var() {
    mocked_pagers::with_mocked_versions_of_more_and_most_in_path(|| {
        // If the output is not "I am most" then we know 'most' is not used
        bat()
            .env("PAGER", mocked_pagers::from("most"))
            .arg("--paging=always")
            .arg("test.txt")
            .assert()
            .success()
            .stdout(predicate::eq("hello world\n").normalize());
    });
}

/// If the bat-specific BAT_PAGER is used, obey the wish of the user
/// and allow 'most'
#[test]
#[serial] // Because of PATH
fn pager_most_from_bat_pager_env_var() {
    mocked_pagers::with_mocked_versions_of_more_and_most_in_path(|| {
        bat()
            .env("BAT_PAGER", mocked_pagers::from("most"))
            .arg("--paging=always")
            .arg("test.txt")
            .assert()
            .success()
            .stdout(predicate::str::contains("I am most"));
    });
}

/// Same reasoning with --pager as with BAT_PAGER
#[test]
#[serial] // Because of PATH
fn pager_most_from_pager_arg() {
    mocked_pagers::with_mocked_versions_of_more_and_most_in_path(|| {
        bat()
            .arg("--paging=always")
            .arg(format!("--pager={}", mocked_pagers::from("most")))
            .arg("test.txt")
            .assert()
            .success()
            .stdout(predicate::str::contains("I am most"));
    });
}

/// Make sure the logic for 'most' applies even if an argument is passed
#[test]
#[serial] // Because of PATH
fn pager_most_with_arg() {
    mocked_pagers::with_mocked_versions_of_more_and_most_in_path(|| {
        bat()
            .env("PAGER", format!("{} -w", mocked_pagers::from("most")))
            .arg("--paging=always")
            .arg("test.txt")
            .assert()
            .success()
            .stdout(predicate::eq("hello world\n").normalize());
    });
}

/// Sanity check that 'more' is treated like 'most'
#[test]
#[serial] // Because of PATH
fn pager_more() {
    mocked_pagers::with_mocked_versions_of_more_and_most_in_path(|| {
        bat()
            .env("PAGER", mocked_pagers::from("more"))
            .arg("--paging=always")
            .arg("test.txt")
            .assert()
            .success()
            .stdout(predicate::eq("hello world\n").normalize());
    });
}

#[test]
fn alias_pager_disable() {
    bat()
        .env("PAGER", "echo other-pager")
        .arg("-P")
        .arg("test.txt")
        .assert()
        .success()
        .stdout(predicate::eq("hello world\n").normalize());
}

#[test]
#[serial]
fn alias_pager_disable_long_overrides_short() {
    mocked_pagers::with_mocked_versions_of_more_and_most_in_path(|| {
        bat()
            .env("PAGER", mocked_pagers::from("echo pager-output"))
            .arg("-P")
            .arg("--paging=always")
            .arg("test.txt")
            .assert()
            .success()
            .stdout(predicate::str::contains("pager-output\n").normalize());
    });
}

#[test]
fn disable_pager_if_disable_paging_flag_comes_after_paging() {
    bat()
        .env("PAGER", "echo pager-output")
        .arg("--paging=always")
        .arg("-P")
        .arg("test.txt")
        .assert()
        .success()
        .stdout(predicate::eq("hello world\n").normalize());
}

#[test]
fn disable_pager_if_pp_flag_comes_after_paging() {
    bat()
        .env("PAGER", "echo pager-output")
        .arg("--paging=always")
        .arg("-pp")
        .arg("test.txt")
        .assert()
        .success()
        .stdout(predicate::eq("hello world\n").normalize());
}

#[test]
fn enable_pager_if_disable_paging_flag_comes_before_paging() {
    bat()
        .env("PAGER", "echo pager-output")
        .arg("-P")
        .arg("--paging=always")
        .arg("test.txt")
        .assert()
        .success()
        .stdout(predicate::eq("pager-output\n").normalize());
}

#[test]
fn enable_pager_if_pp_flag_comes_before_paging() {
    bat()
        .env("PAGER", "echo pager-output")
        .arg("-pp")
        .arg("--paging=always")
        .arg("test.txt")
        .assert()
        .success()
        .stdout(predicate::eq("pager-output\n").normalize());
}

#[test]
fn paging_does_not_override_simple_plain() {
    bat()
        .env("PAGER", "echo pager-output")
        .arg("--decorations=always")
        .arg("--plain")
        .arg("--paging=never")
        .arg("test.txt")
        .assert()
        .success()
        .stdout(predicate::eq("hello world\n"));
}

#[test]
fn simple_plain_does_not_override_paging() {
    bat()
        .env("PAGER", "echo pager-output")
        .arg("--paging=always")
        .arg("--plain")
        .arg("test.txt")
        .assert()
        .success()
        .stdout(predicate::eq("pager-output\n"));
}

#[test]
fn pager_failed_to_parse() {
    bat()
        .env("BAT_PAGER", "mismatched-quotes 'a")
        .arg("--paging=always")
        .arg("test.txt")
        .assert()
        .failure()
        .stderr(predicate::str::contains("Could not parse pager command"));
}

#[test]
#[serial]
fn env_var_bat_paging() {
    mocked_pagers::with_mocked_versions_of_more_and_most_in_path(|| {
        bat()
            .env("BAT_PAGER", mocked_pagers::from("echo pager-output"))
            .env("BAT_PAGING", "always")
            .arg("test.txt")
            .assert()
            .success()
            .stdout(predicate::str::contains("pager-output\n").normalize());
    });
}

#[test]
fn basic_set_terminal_title() {
    bat()
        .arg("--paging=always")
        .arg("--set-terminal-title")
        .arg("test.txt")
        .assert()
        .success()
        .stdout("\u{1b}]0;bat: test.txt\x07hello world\n")
        .stderr("");
}

#[test]
fn diagnostic_sanity_check() {
    bat()
        .arg("--diagnostic")
        .assert()
        .success()
        .stdout(predicate::str::contains("BAT_PAGER="))
        .stderr("");
}

#[test]
fn config_location_test() {
    bat_with_config()
        .env("BAT_CONFIG_PATH", "bat.conf")
        .arg("--config-file")
        .assert()
        .success()
        .stdout("bat.conf\n");

    bat_with_config()
        .env("BAT_CONFIG_PATH", "not-existing.conf")
        .arg("--config-file")
        .assert()
        .success()
        .stdout("not-existing.conf\n");
}

#[test]
fn config_location_when_generating() {
    let tmp_dir = tempdir().expect("can create temporary directory");
    let tmp_config_path = tmp_dir.path().join("should-be-created.conf");

    // Create the file with bat
    bat_with_config()
        .env("BAT_CONFIG_PATH", tmp_config_path.to_str().unwrap())
        .arg("--generate-config-file")
        .assert()
        .success()
        .stdout(
            predicate::str::is_match("Success! Config file written to .*should-be-created.conf\n")
                .unwrap(),
        );

    // Now we expect the file to exist. If it exists, we assume contents are correct
    assert!(tmp_config_path.exists());
}

#[test]
fn config_location_from_bat_config_dir_variable() {
    bat_with_config()
        .env("BAT_CONFIG_DIR", "conf/")
        .arg("--config-file")
        .assert()
        .success()
        .stdout(predicate::str::is_match("conf/config\n").unwrap());
}

#[test]
#[serial]
fn config_read_arguments_from_file() {
    mocked_pagers::with_mocked_versions_of_more_and_most_in_path(|| {
        bat_with_config()
            .env("BAT_CONFIG_PATH", get_config())
            .arg("test.txt")
            .assert()
            .success()
            .stdout(predicate::str::contains("dummy-pager-from-config\n").normalize());
    });
}

#[cfg(unix)]
#[test]
fn cache_clear() {
    let src_dir = "cache_source";
    let tmp_dir = tempdir().expect("can create temporary directory");
    let themes_filename = "themes.bin";
    let syntaxes_filename = "syntaxes.bin";
    let metadata_filename = "metadata.yaml";
    [themes_filename, syntaxes_filename, metadata_filename]
        .iter()
        .map(|filename| {
            let fp = tmp_dir.path().join(filename);
            let mut file = File::create(fp).expect("can create temporary file");
            writeln!(file, "dummy content").expect("can write to file");
        })
        .count();

    // Clear the targeted cache
    // Include the BAT_CONFIG_PATH and BAT_THEME environment variables to ensure that
    // options loaded from a config or the environment are not inserted
    // before the cache subcommand, which would break it.
    bat_with_config()
        .current_dir(Path::new(EXAMPLES_DIR).join(src_dir))
        .env("BAT_CONFIG_PATH", "bat.conf")
        .env("BAT_THEME", "1337")
        .arg("cache")
        .arg("--clear")
        .arg("--source")
        .arg(".")
        .arg("--target")
        .arg(tmp_dir.path().to_str().unwrap())
        .assert()
        .success()
        .stdout(
            predicate::str::is_match(
                "Clearing theme set cache ... okay
Clearing syntax set cache ... okay
Clearing metadata file ... okay",
            )
            .unwrap(),
        );

    // We expect these files to be removed
    assert!(!tmp_dir.path().join(themes_filename).exists());
    assert!(!tmp_dir.path().join(syntaxes_filename).exists());
    assert!(!tmp_dir.path().join(metadata_filename).exists());
}

#[cfg(unix)]
#[test]
fn cache_build() {
    let src_dir = "cache_source";
    let tmp_dir = tempdir().expect("can create temporary directory");
    let tmp_themes_path = tmp_dir.path().join("themes.bin");
    let tmp_syntaxes_path = tmp_dir.path().join("syntaxes.bin");
    let tmp_acknowledgements_path = tmp_dir.path().join("acknowledgements.bin");
    let tmp_metadata_path = tmp_dir.path().join("metadata.yaml");

    // Build the cache
    // Include the BAT_CONFIG_PATH and BAT_THEME environment variables to ensure that
    // options loaded from a config or the environment are not inserted
    // before the cache subcommand, which would break it.
    bat_with_config()
        .current_dir(Path::new(EXAMPLES_DIR).join(src_dir))
        .env("BAT_CONFIG_PATH", "bat.conf")
        .env("BAT_THEME", "1337")
        .arg("cache")
        .arg("--build")
        .arg("--blank")
        .arg("--source")
        .arg(".")
        .arg("--target")
        .arg(tmp_dir.path().to_str().unwrap())
        .arg("--acknowledgements")
        .assert()
        .success()
        .stdout(
            predicate::str::is_match(
                "Writing theme set to .*/themes.bin ... okay
Writing syntax set to .*/syntaxes.bin ... okay
Writing acknowledgements to .*/acknowledgements.bin ... okay
Writing metadata to folder .* ... okay",
            )
            .unwrap(),
        );

    // Now we expect the files to exist. If they exist, we assume contents are correct
    assert!(tmp_themes_path.exists());
    assert!(tmp_syntaxes_path.exists());
    assert!(tmp_acknowledgements_path.exists());
    assert!(tmp_metadata_path.exists());
}

#[test]
fn utf16() {
    // The output will be converted to UTF-8 with the leading UTF-16
    // BOM removed. This behavior is wanted in interactive mode as
    // some terminals seem to display the BOM character as a space,
    // and it also breaks syntax highlighting.
    bat()
        .arg("--plain")
        .arg("--decorations=always")
        .arg("test_UTF-16LE.txt")
        .assert()
        .success()
        .stdout("hello world\n");

    bat()
        .arg("--plain")
        .arg("--decorations=always")
        .arg("test_UTF-16BE.txt")
        .assert()
        .success()
        .stdout("hello world\nthis is a test\n");
}

#[test]
fn utf16le() {
    bat()
        .arg("--decorations=always")
        .arg("--style=numbers")
        .arg("--color=never")
        .arg("test_UTF-16LE-complicated.txt")
        .assert()
        .success()
        .stdout("   1 上一伊刀\n   2 foo bar\n   3 hello world\n");
}

#[test]
fn utf16be() {
    bat()
        .arg("--decorations=always")
        .arg("--style=numbers")
        .arg("--color=never")
        .arg("test_UTF-16BE-complicated.txt")
        .assert()
        .success()
        .stdout("   1 上一伊刀\n   2 foo bar\n   3 hello world\n");
}

// Regression test for https://github.com/sharkdp/bat/issues/1922
#[test]
fn bom_not_stripped_in_loop_through_mode() {
    bat()
        .arg("--plain")
        .arg("--decorations=never")
        .arg("--color=never")
        .arg("test_BOM.txt")
        .assert()
        .success()
        .stdout("\u{feff}hello world\n");
}

// Regression test for https://github.com/sharkdp/bat/issues/1922
#[test]
fn bom_stripped_when_colored_output() {
    bat()
        .arg("--color=always")
        .arg("--decorations=never")
        .arg("test_BOM.txt")
        .assert()
        .success()
        .stdout(
            predicate::str::is_match("\u{1b}\\[38;5;[0-9]{3}mhello world\u{1b}\\[0m\n").unwrap(),
        );
}

// Regression test for https://github.com/sharkdp/bat/issues/1922
#[test]
fn bom_stripped_when_no_color_and_not_loop_through() {
    bat()
        .arg("--color=never")
        .arg("--decorations=always")
        .arg("--style=numbers,grid,header")
        .arg("--terminal-width=80")
        .arg("test_BOM.txt")
        .assert()
        .success()
        .stdout(
            "\
─────┬──────────────────────────────────────────────────────────────────────────
     │ File: test_BOM.txt
─────┼──────────────────────────────────────────────────────────────────────────
   1 │ hello world
─────┴──────────────────────────────────────────────────────────────────────────
",
        );
}

// Regression test for https://github.com/sharkdp/bat/issues/2541
#[test]
fn no_broken_osc_emit_with_line_wrapping() {
    bat()
        .arg("--color=always")
        .arg("--decorations=never")
        .arg("--wrap=character")
        .arg("--terminal-width=40")
        .arg("regression_tests/issue_2541.txt")
        .assert()
        .success()
        .stdout(predicate::function(|s: &str| s.lines().count() == 1));
}

#[test]
fn can_print_file_named_cache() {
    bat_with_config()
        .arg("cache")
        .assert()
        .success()
        .stdout("test\n")
        .stderr("");
}

#[test]
fn can_print_file_named_cache_with_additional_argument() {
    bat_with_config()
        .arg("cache")
        .arg("test.txt")
        .assert()
        .success()
        .stdout("test\nhello world\n")
        .stderr("");
}

#[test]
fn can_print_file_starting_with_cache() {
    bat_with_config()
        .arg("cache.c")
        .assert()
        .success()
        .stdout("test\n")
        .stderr("");
}

#[test]
fn does_not_print_unwanted_file_named_cache() {
    bat_with_config().arg("cach").assert().failure();
}

#[test]
fn accepts_no_custom_assets_arg() {
    // Just make sure --no-custom-assets is considered a valid arg
    // Don't bother to actually verify that it works
    bat()
        .arg("--no-custom-assets")
        .arg("test.txt")
        .assert()
        .success();
}

#[test]
fn unicode_wrap() {
    bat_with_config()
        .arg("unicode-wrap.txt")
        .arg("--style=numbers,snip")
        .arg("--decorations=always")
        .arg("--terminal-width=40")
        .assert()
        .success()
        .stdout(
            "   1 ビタミンA  ビタミンD  ビタミンE  ビ
     タミンK  ビタミンB1  ビタミンB2  ナ
     イアシン  パントテン酸  ビタミンB6 
      ビタミンB12  葉酸  ビオチン  ビタ
     ミンC
   2 
   3 고양이 고양이 고양이 고양이 고양이 
     고양이 고양이 고양이 고양이 고양이 
     고양이 고양이 고양이 고양이 고양이 
     고양이 고양이 고양이 고양이 고양이 
     고양이 고양이 고양이 고양이 고양이 
     고양이 고양이 고양이 고양이 고양이 
     고양이 고양이 고양이 고양이 고양이 
     고양이 고양이 고양이 고양이 고양이 
     고양이
   4 
   5 1 บวก 2 บวก 3 บวก 4 บวก 5 บวก 6 บวก
      7 บวก 8 บวก 9 บวก 10 บวก 11 บวก 12
      บวก 13 บวก 14 บวก 15 บวก 16 บวก 17
      บวก 18 บวก 19 บวก 20
   6 
   7 Бельгия Болгария Чехия Дания Герман
     ия Эстония Ирландия Греция Испания 
     Франция Хорватия Италия Кипр Латвия
      Литва Люксембург Венгрия Мальта Ни
     дерланды Австрия Польша Португалия 
     Румыния Словения Словакия Финляндия
      Швеция Великобритания
",
        );
}

#[test]
fn snip() {
    bat()
        .arg("multiline.txt")
        .arg("--style=numbers,snip")
        .arg("--decorations=always")
        .arg("--line-range=1:2")
        .arg("--line-range=4:")
        .arg("--terminal-width=80")
        .assert()
        .success()
        .stdout(
            "   1 line 1
   2 line 2
 ...─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ 8< ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─
   4 line 4
   5 line 5
   6 line 6
   7 line 7
   8 line 8
   9 line 9
  10 line 10
",
        );
}

#[test]
fn empty_file_leads_to_empty_output_with_grid_enabled() {
    bat()
        .arg("empty.txt")
        .arg("--style=grid")
        .arg("--decorations=always")
        .arg("--terminal-width=80")
        .assert()
        .success()
        .stdout("");
}

#[test]
fn empty_file_leads_to_empty_output_with_rule_enabled() {
    bat()
        .arg("empty.txt")
        .arg("--style=rule")
        .arg("--decorations=always")
        .arg("--terminal-width=80")
        .assert()
        .success()
        .stdout("");
}

#[test]
fn header_basic() {
    bat()
        .arg("test.txt")
        .arg("--decorations=always")
        .arg("--style=header")
        .arg("-r=0:0")
        .arg("--file-name=foo")
        .assert()
        .success()
        .stdout("File: foo\n")
        .stderr("");
}

#[test]
fn header_full_basic() {
    bat()
        .arg("test.txt")
        .arg("--decorations=always")
        .arg("--style=header-filename,header-filesize")
        .arg("-r=0:0")
        .arg("--file-name=foo")
        .assert()
        .success()
        .stdout("File: foo\nSize: 12 B\n")
        .stderr("");
}

#[test]
fn header_env_basic() {
    bat_with_config()
        .env("BAT_STYLE", "header-filename,header-filesize")
        .arg("test.txt")
        .arg("--decorations=always")
        .arg("-r=0:0")
        .arg("--file-name=foo")
        .assert()
        .success()
        .stdout("File: foo\nSize: 12 B\n")
        .stderr("");
}

#[test]
fn header_arg_overrides_env() {
    bat_with_config()
        .env("BAT_STYLE", "header-filesize")
        .arg("test.txt")
        .arg("--decorations=always")
        .arg("--style=header-filename")
        .arg("-r=0:0")
        .arg("--file-name=foo")
        .assert()
        .success()
        .stdout("File: foo\n")
        .stderr("");
}

#[test]
fn header_binary() {
    bat()
        .arg("test.binary")
        .arg("--decorations=always")
        .arg("--style=header")
        .arg("-r=0:0")
        .arg("--file-name=foo")
        .assert()
        .success()
        .stdout("File: foo   <BINARY>\n")
        .stderr("");
}

#[test]
fn header_full_binary() {
    bat()
        .arg("test.binary")
        .arg("--decorations=always")
        .arg("--style=header-filename,header-filesize")
        .arg("-r=0:0")
        .arg("--file-name=foo")
        .assert()
        .success()
        .stdout("File: foo   <BINARY>\nSize: 4 B\n")
        .stderr("");
}

#[test]
#[cfg(not(feature = "git"))]
fn header_narrow_terminal() {
    bat()
        .arg("--terminal-width=30")
        .arg("--decorations=always")
        .arg("this-file-path-is-really-long-and-would-have-broken-the-layout-of-the-header.txt")
        .assert()
        .success()
        .stdout(
            "\
─────┬────────────────────────
     │ File: this-file-path-is
     │ -really-long-and-would-
     │ have-broken-the-layout-
     │ of-the-header.txt
─────┼────────────────────────
   1 │ The header is not broke
     │ n
─────┴────────────────────────
",
        )
        .stderr("");
}

#[test]
fn header_very_narrow_terminal() {
    bat()
        .arg("--terminal-width=10")
        .arg("--decorations=always")
        .arg("this-file-path-is-really-long-and-would-have-broken-the-layout-of-the-header.txt")
        .assert()
        .success()
        .stdout(
            "\
──────────
File: this
-file-path
-is-really
-long-and-
would-have
-broken-th
e-layout-o
f-the-head
er.txt
──────────
The header
 is not br
oken
──────────
",
        )
        .stderr("");
}

#[test]
fn header_narrow_terminal_with_multibyte_chars() {
    bat()
        .arg("--terminal-width=30")
        .arg("--decorations=always")
        .arg("test.A—B가")
        .assert()
        .success()
        .stderr("");
}

#[test]
#[cfg(feature = "git")] // Expected output assumes git is enabled
fn header_default() {
    bat()
        .arg("--paging=never")
        .arg("--color=never")
        .arg("--terminal-width=80")
        .arg("--wrap=never")
        .arg("--decorations=always")
        .arg("--style=default")
        .arg("single-line.txt")
        .assert()
        .success()
        .stdout(
            "\
───────┬────────────────────────────────────────────────────────────────────────
       │ File: single-line.txt
───────┼────────────────────────────────────────────────────────────────────────
   1   │ Single Line
───────┴────────────────────────────────────────────────────────────────────────
",
        )
        .stderr("");
}

#[test]
#[cfg(feature = "git")] // Expected output assumes git is enabled
fn header_default_is_default() {
    bat()
        .arg("--paging=never")
        .arg("--color=never")
        .arg("--terminal-width=80")
        .arg("--wrap=never")
        .arg("--decorations=always")
        .arg("single-line.txt")
        .assert()
        .success()
        .stdout(
            "\
───────┬────────────────────────────────────────────────────────────────────────
       │ File: single-line.txt
───────┼────────────────────────────────────────────────────────────────────────
   1   │ Single Line
───────┴────────────────────────────────────────────────────────────────────────
",
        )
        .stderr("");
}

#[test]
fn filename_stdin() {
    bat()
        .arg("--decorations=always")
        .arg("--style=header")
        .arg("-r=0:0")
        .arg("-")
        .write_stdin("stdin\n")
        .arg("--file-name=foo")
        .assert()
        .success()
        .stdout("File: foo\n")
        .stderr("");
}

#[test]
fn filename_stdin_binary() {
    let vec = vec![0; 1];
    bat_with_config()
        .arg("--decorations=always")
        .arg("--style=header")
        .write_stdin(vec)
        .arg("--file-name=foo")
        .assert()
        .success()
        .stdout("File: foo   <BINARY>\n")
        .stderr("");
}

#[test]
fn filename_multiple_ok() {
    bat()
        .arg("--decorations=always")
        .arg("--style=header")
        .arg("-r=0:0")
        .arg("test.txt")
        .arg("--file-name=foo")
        .arg("single-line.txt")
        .arg("--file-name=bar")
        .assert()
        .success()
        .stdout("File: foo\n\nFile: bar\n")
        .stderr("");
}

#[test]
fn filename_multiple_err() {
    bat()
        .arg("--decorations=always")
        .arg("--style=header")
        .arg("-r=0:0")
        .arg("test.txt")
        .arg("--file-name=foo")
        .arg("single-line.txt")
        .assert()
        .failure();
}

#[test]
fn header_padding() {
    bat()
        .arg("--decorations=always")
        .arg("--style=header")
        .arg("test.txt")
        .arg("single-line.txt")
        .assert()
        .stdout("File: test.txt\nhello world\n\nFile: single-line.txt\nSingle Line\n")
        .stderr("");
}

#[test]
fn header_full_padding() {
    bat()
        .arg("--decorations=always")
        .arg("--style=header-filename,header-filesize")
        .arg("test.txt")
        .arg("single-line.txt")
        .assert()
        .stdout("File: test.txt\nSize: 12 B\nhello world\n\nFile: single-line.txt\nSize: 11 B\nSingle Line\n")
        .stderr("");
}

#[test]
fn header_padding_rule() {
    bat()
        .arg("--decorations=always")
        .arg("--style=header,rule")
        .arg("--terminal-width=80")
        .arg("test.txt")
        .arg("single-line.txt")
        .assert()
        .stdout(
            "File: test.txt
hello world
────────────────────────────────────────────────────────────────────────────────
File: single-line.txt
Single Line
",
        )
        .stderr("");
}

#[test]
fn header_full_padding_rule() {
    bat()
        .arg("--decorations=always")
        .arg("--style=header-filename,header-filesize,rule")
        .arg("--terminal-width=80")
        .arg("test.txt")
        .arg("single-line.txt")
        .assert()
        .stdout(
            "File: test.txt
Size: 12 B
hello world
────────────────────────────────────────────────────────────────────────────────
File: single-line.txt
Size: 11 B
Single Line
",
        )
        .stderr("");
}

#[test]
fn grid_overrides_rule() {
    bat()
        .arg("--decorations=always")
        .arg("--style=grid,rule")
        .arg("--terminal-width=80")
        .arg("test.txt")
        .arg("single-line.txt")
        .assert()
        .stdout(
            "\
────────────────────────────────────────────────────────────────────────────────
hello world
────────────────────────────────────────────────────────────────────────────────
────────────────────────────────────────────────────────────────────────────────
Single Line
────────────────────────────────────────────────────────────────────────────────
",
        )
        .stderr(
            "\x1b[33m[bat warning]\x1b[0m: Style 'rule' is a subset of style 'grid', 'rule' will not be visible.\n",
        );
}

#[cfg(target_os = "linux")]
#[test]
fn file_with_invalid_utf8_filename() {
    use std::ffi::OsStr;
    use std::fs::File;
    use std::io::Write;
    use std::os::unix::ffi::OsStrExt;

    let tmp_dir = tempdir().expect("can create temporary directory");
    let file_path = tmp_dir
        .path()
        .join(OsStr::from_bytes(b"test-invalid-utf8-\xC3(.rs"));
    {
        let mut file = File::create(&file_path).expect("can create temporary file");
        writeln!(file, "dummy content").expect("can write to file");
    }

    bat()
        .arg(file_path.as_os_str())
        .assert()
        .success()
        .stdout("dummy content\n");
}

#[test]
fn do_not_panic_regression_tests() {
    for filename in &[
        "issue_28.md",
        "issue_190.md",
        "issue_314.hs",
        "issue_914.rb",
        "issue_915.vue",
    ] {
        bat()
            .arg("--color=always")
            .arg(format!("regression_tests/{filename}"))
            .assert()
            .success();
    }
}

#[test]
fn do_not_detect_different_syntax_for_stdin_and_files() {
    let file = "regression_tests/issue_985.js";

    let cmd_for_file = bat()
        .arg("--color=always")
        .arg("--map-syntax=*.js:Markdown")
        .arg(format!("--file-name={file}"))
        .arg("--style=plain")
        .arg(file)
        .assert()
        .success();

    let cmd_for_stdin = bat()
        .arg("--color=always")
        .arg("--map-syntax=*.js:Markdown")
        .arg("--style=plain")
        .arg(format!("--file-name={file}"))
        .pipe_stdin(Path::new(EXAMPLES_DIR).join(file))
        .unwrap()
        .assert()
        .success();

    assert_eq!(
        from_utf8(&cmd_for_file.get_output().stdout).expect("output is valid utf-8"),
        from_utf8(&cmd_for_stdin.get_output().stdout).expect("output is valid utf-8")
    );
}

#[test]
fn no_first_line_fallback_when_mapping_to_invalid_syntax() {
    let file = "regression_tests/first_line_fallback.invalid-syntax";

    bat()
        .arg("--color=always")
        .arg("--map-syntax=*.invalid-syntax:InvalidSyntax")
        .arg(format!("--file-name={file}"))
        .arg("--style=plain")
        .arg(file)
        .assert()
        .failure()
        .stderr(predicate::str::contains("unknown syntax: 'InvalidSyntax'"));
}

#[test]
fn show_all_mode() {
    bat()
        .arg("--show-all")
        .arg("nonprintable.txt")
        .assert()
        .stdout("hello·world␊\n├──┤␍␀␇␈␛")
        .stderr("");
}

#[test]
fn show_all_extends_tab_markers_to_next_tabstop() {
    bat()
        .arg("tabs.txt")
        .arg("--show-all")
        .arg("--tabs=4")
        .arg("--style=plain")
        .assert()
        .success()
        .stdout(
            "├──┤1├─┤2├─┤3├─┤4␊
1├─┤?␊
22├┤?␊
333↹?␊
4444├──┤?␊
55555├─┤?␊
666666├┤?␊
7777777↹?␊
88888888├──┤?␊
",
        );
}

#[test]
fn show_all_extends_tab_markers_to_next_tabstop_width_8() {
    bat()
        .arg("tabs.txt")
        .arg("--show-all")
        .arg("--tabs=8")
        .arg("--style=plain")
        .assert()
        .success()
        .stdout(
            "├──────┤1├─────┤2├─────┤3├─────┤4␊
1├─────┤?␊
22├────┤?␊
333├───┤?␊
4444├──┤?␊
55555├─┤?␊
666666├┤?␊
7777777↹?␊
88888888├──────┤?␊
",
        );
}

#[test]
fn show_all_with_caret_notation() {
    bat()
        .arg("--show-all")
        .arg("--nonprintable-notation=caret")
        .arg("nonprintable.txt")
        .assert()
        .stdout("hello·world^J\n├──┤^M^@^G^H^[")
        .stderr("");

    bat()
        .arg("--show-all")
        .arg("--nonprintable-notation=caret")
        .arg("control_characters.txt")
        .assert()
        .stdout("^@^A^B^C^D^E^F^G^H├─┤^J\n^K^L^M^N^O^P^Q^R^S^T^U^V^W^X^Y^Z^[^\\^]^^^_^?")
        .stderr("");
}

#[test]
fn show_all_with_unicode() {
    bat()
        .arg("--show-all")
        .arg("--nonprintable-notation=unicode")
        .arg("control_characters.txt")
        .assert()
        .stdout("␀␁␂␃␄␅␆␇␈├─┤␊\n␋␌␍␎␏␐␑␒␓␔␕␖␗␘␙␚␛␜␝␞␟␡")
        .stderr("");
}

#[test]
fn binary_as_text() {
    bat()
        .arg("--binary=as-text")
        .arg("control_characters.txt")
        .assert()
        .stdout("\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0A\x0B\x0C\x0D\x0E\x0F\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\x1A\x1B\x1C\x1D\x1E\x1F\x7F")
        .stderr("");
}

#[test]
fn no_paging_arg() {
    bat()
        .arg("--no-paging")
        .arg("--color=never")
        .arg("--decorations=never")
        .arg("single-line.txt")
        .assert()
        .success()
        .stdout("Single Line");
}

#[test]
fn no_paging_short_arg() {
    bat()
        .arg("-P")
        .arg("--color=never")
        .arg("--decorations=never")
        .arg("single-line.txt")
        .assert()
        .success()
        .stdout("Single Line");
}

#[test]
fn no_pager_arg() {
    bat()
        .arg("--no-pager")
        .arg("--color=never")
        .arg("--decorations=never")
        .arg("single-line.txt")
        .assert()
        .success()
        .stdout("Single Line");
}

#[test]
fn plain_mode_does_not_add_nonexisting_newline() {
    bat()
        .arg("--paging=never")
        .arg("--color=never")
        .arg("--decorations=always")
        .arg("--style=plain")
        .arg("single-line.txt")
        .assert()
        .success()
        .stdout("Single Line");
}

// Regression test for https://github.com/sharkdp/bat/issues/299
#[test]
#[cfg(feature = "git")] // Expected output assumes git is enabled
fn grid_for_file_without_newline() {
    bat()
        .arg("--paging=never")
        .arg("--color=never")
        .arg("--terminal-width=80")
        .arg("--wrap=never")
        .arg("--decorations=always")
        .arg("--style=full")
        .arg("single-line.txt")
        .assert()
        .success()
        .stdout(
            "\
───────┬────────────────────────────────────────────────────────────────────────
       │ File: single-line.txt
       │ Size: 11 B
───────┼────────────────────────────────────────────────────────────────────────
   1   │ Single Line
───────┴────────────────────────────────────────────────────────────────────────
",
        )
        .stderr("");
}

// For ANSI theme, use underscore as a highlighter
#[test]
fn ansi_highlight_underline() {
    bat()
        .arg("--paging=never")
        .arg("--color=never")
        .arg("--terminal-width=80")
        .arg("--wrap=never")
        .arg("--decorations=always")
        .arg("--theme=ansi")
        .arg("--style=plain")
        .arg("--highlight-line=1")
        .write_stdin("Ansi Underscore Test\nAnother Line")
        .assert()
        .success()
        .stdout("\x1B[4mAnsi Underscore Test\n\x1B[24mAnother Line")
        .stderr("");
}

// Ensure that ANSI passthrough is emitted properly for both wrapping and non-wrapping printer.
// See https://github.com/sharkdp/bat/issues/2307 for what common use case this test tests.
#[test]
fn ansi_passthrough_emit() {
    for wrapping in &["never", "character"] {
        bat()
            .arg("--paging=never")
            .arg("--color=never")
            .arg("--terminal-width=80")
            .arg(format!("--wrap={wrapping}"))
            .arg("--decorations=always")
            .arg("--style=plain")
            .write_stdin("\x1B[33mColor\nColor \x1B[m\nPlain\n")
            .assert()
            .success()
            .stdout("\x1B[33m\x1B[33mColor\n\x1B[33mColor \x1B[m\nPlain\n")
            .stderr("");
    }
}

// Ensure that a simple ANSI sequence passthrough is emitted properly on wrapped lines.
// This also helps ensure that escape sequences are counted as part of the visible characters when wrapping.
#[test]
fn ansi_sgr_emitted_when_wrapped() {
    bat()
        .arg("--paging=never")
        .arg("--color=never")
        .arg("--terminal-width=20")
        .arg("--wrap=character")
        .arg("--decorations=always")
        .arg("--style=plain")
        .write_stdin("\x1B[33mColor...............Also color.\n")
        .assert()
        .success()
        .stdout("\x1B[33m\x1B[33mColor...............\n\x1B[33mAlso color.\n")
        // FIXME:              ~~~~~~~~ should not be emitted twice.
        .stderr("");
}

// Ensure that a simple ANSI sequence passthrough is emitted properly on wrapped lines.
// This also helps ensure that escape sequences are counted as part of the visible characters when wrapping.
#[test]
fn ansi_hyperlink_emitted_when_wrapped() {
    bat()
        .arg("--paging=never")
        .arg("--color=never")
        .arg("--terminal-width=20")
        .arg("--wrap=character")
        .arg("--decorations=always")
        .arg("--style=plain")
        .write_stdin("\x1B]8;;http://example.com/\x1B\\Hyperlinks..........Wrap across lines.\n")
        .assert()
        .success()
        .stdout("\x1B]8;;http://example.com/\x1B\\\x1B]8;;http://example.com/\x1B\\Hyperlinks..........\x1B]8;;\x1B\\\n\x1B]8;;http://example.com/\x1B\\Wrap across lines.\n")
        // FIXME:                                      ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ should not be emitted twice.
        .stderr("");
}

// Ensure that multiple ANSI sequence SGR attributes are combined when emitted on wrapped lines.
#[test]
fn ansi_sgr_joins_attributes_when_wrapped() {
    bat()
            .arg("--paging=never")
            .arg("--color=never")
            .arg("--terminal-width=20")
            .arg("--wrap=character")
            .arg("--decorations=always")
            .arg("--style=plain")
            .write_stdin("\x1B[33mColor. \x1B[1mBold.........Also bold and color.\n")
            .assert()
            .success()
            .stdout("\x1B[33m\x1B[33mColor. \x1B[1m\x1B[33m\x1B[1mBold.........\n\x1B[33m\x1B[1mAlso bold and color.\n")
            // FIXME:              ~~~~~~~~       ~~~~~~~~~~~~~~~ should not be emitted twice.
            .stderr("");
}

#[test]
fn ignored_suffix_arg() {
    bat()
        .arg("-f")
        .arg("--theme")
        .arg("Monokai Extended")
        .arg("-p")
        .arg("test.json~")
        .assert()
        .success()
        .stdout("\u{1b}[38;5;231m{\u{1b}[0m\u{1b}[38;5;208m\"\u{1b}[0m\u{1b}[38;5;208mtest\u{1b}[0m\u{1b}[38;5;208m\"\u{1b}[0m\u{1b}[38;5;231m:\u{1b}[0m\u{1b}[38;5;231m \u{1b}[0m\u{1b}[38;5;186m\"\u{1b}[0m\u{1b}[38;5;186mvalue\u{1b}[0m\u{1b}[38;5;186m\"\u{1b}[0m\u{1b}[38;5;231m}\u{1b}[0m")
        .stderr("");

    bat()
        .arg("-f")
        .arg("--theme")
        .arg("Monokai Extended")
        .arg("-p")
        .arg("--ignored-suffix=.suffix")
        .arg("test.json.suffix")
        .assert()
        .success()
        .stdout("\u{1b}[38;5;231m{\u{1b}[0m\u{1b}[38;5;208m\"\u{1b}[0m\u{1b}[38;5;208mtest\u{1b}[0m\u{1b}[38;5;208m\"\u{1b}[0m\u{1b}[38;5;231m:\u{1b}[0m\u{1b}[38;5;231m \u{1b}[0m\u{1b}[38;5;186m\"\u{1b}[0m\u{1b}[38;5;186mvalue\u{1b}[0m\u{1b}[38;5;186m\"\u{1b}[0m\u{1b}[38;5;231m}\u{1b}[0m")
        .stderr("");

    bat()
        .arg("-f")
        .arg("--theme")
        .arg("Monokai Extended")
        .arg("-p")
        .arg("test.json.suffix")
        .assert()
        .success()
        .stdout("\u{1b}[38;5;231m{\"test\": \"value\"}\u{1b}[0m")
        .stderr("");
}

fn wrapping_test(wrap_flag: &str, expect_wrap: bool) {
    let expected = match expect_wrap {
        true =>
            "abcdefghigklmnopqrstuvxyzabcdefghigklmnopqrstuvxyzabcdefghigklmnopqrstuvxyzabcde\nfghigklmnopqrstuvxyz\n",
        false =>
            "abcdefghigklmnopqrstuvxyzabcdefghigklmnopqrstuvxyzabcdefghigklmnopqrstuvxyzabcdefghigklmnopqrstuvxyz\n",
    };

    bat()
        .arg(wrap_flag)
        .arg("--style=rule")
        .arg("--color=never")
        .arg("--decorations=always")
        .arg("--terminal-width=80")
        .arg("long-single-line.txt")
        .assert()
        .success()
        .stdout(expected.to_owned())
        .stderr("");
}

#[test]
fn no_line_wrapping_when_set_to_never() {
    wrapping_test("--wrap=never", false);
}

#[test]
fn line_wrapping_when_auto() {
    wrapping_test("--wrap=auto", true);
}

#[test]
fn no_line_wrapping_with_s_flag() {
    wrapping_test("-S", false);
}

#[test]
fn no_wrapping_with_chop_long_lines() {
    wrapping_test("--chop-long-lines", false);
}

#[test]
fn theme_arg_overrides_env() {
    bat()
        .env("BAT_THEME", "TwoDark")
        .arg("--paging=never")
        .arg("--color=never")
        .arg("--terminal-width=80")
        .arg("--wrap=never")
        .arg("--decorations=always")
        .arg("--theme=ansi")
        .arg("--style=plain")
        .arg("--highlight-line=1")
        .write_stdin("Ansi Underscore Test\nAnother Line")
        .assert()
        .success()
        .stdout("\x1B[4mAnsi Underscore Test\n\x1B[24mAnother Line")
        .stderr("");
}

#[test]
fn theme_arg_overrides_env_withconfig() {
    bat_with_config()
        .env("BAT_CONFIG_PATH", "bat-theme.conf")
        .env("BAT_THEME", "TwoDark")
        .arg("--paging=never")
        .arg("--color=never")
        .arg("--terminal-width=80")
        .arg("--wrap=never")
        .arg("--decorations=always")
        .arg("--theme=ansi")
        .arg("--style=plain")
        .arg("--highlight-line=1")
        .write_stdin("Ansi Underscore Test\nAnother Line")
        .assert()
        .success()
        .stdout("\x1B[4mAnsi Underscore Test\n\x1B[24mAnother Line")
        .stderr("");
}

#[test]
fn theme_light_env_var_is_respected() {
    bat()
        .env("BAT_THEME_LIGHT", "Coldark-Cold")
        .env("COLORTERM", "truecolor")
        .arg("--theme=light")
        .arg("--paging=never")
        .arg("--color=never")
        .arg("--terminal-width=80")
        .arg("--wrap=never")
        .arg("--decorations=always")
        .arg("--style=plain")
        .arg("--highlight-line=1")
        .write_stdin("Lorem Ipsum")
        .assert()
        .success()
        .stdout("\x1B[48;2;208;218;231mLorem Ipsum\x1B[0m")
        .stderr("");
}

#[test]
fn theme_dark_env_var_is_respected() {
    bat()
        .env("BAT_THEME_DARK", "Coldark-Dark")
        .env("COLORTERM", "truecolor")
        .arg("--theme=dark")
        .arg("--paging=never")
        .arg("--color=never")
        .arg("--terminal-width=80")
        .arg("--wrap=never")
        .arg("--decorations=always")
        .arg("--style=plain")
        .arg("--highlight-line=1")
        .write_stdin("Lorem Ipsum")
        .assert()
        .success()
        .stdout("\x1B[48;2;33;48;67mLorem Ipsum\x1B[0m")
        .stderr("");
}

#[test]
fn theme_env_overrides_config() {
    bat_with_config()
        .env("BAT_CONFIG_PATH", "bat-theme.conf")
        .env("BAT_THEME", "ansi")
        .arg("--paging=never")
        .arg("--color=never")
        .arg("--terminal-width=80")
        .arg("--wrap=never")
        .arg("--decorations=always")
        .arg("--style=plain")
        .arg("--highlight-line=1")
        .write_stdin("Ansi Underscore Test\nAnother Line")
        .assert()
        .success()
        .stdout("\x1B[4mAnsi Underscore Test\n\x1B[24mAnother Line")
        .stderr("");
}

#[test]
fn highlighting_is_skipped_on_long_lines() {
    let expected = "\u{1b}[38;5;231m{\u{1b}[0m\u{1b}[38;5;208m\"\u{1b}[0m\u{1b}[38;5;208mapi\u{1b}[0m\u{1b}[38;5;208m\"\u{1b}[0m\u{1b}[38;5;231m:\u{1b}[0m\n".to_owned() +
        "\u{1b}" +
        r#"[38;5;231m    {"ANGLE_instanced_arrays":{"__compat":{"mdn_url":"https://developer.mozilla.org/docs/Web/API/ANGLE_instanced_arrays","spec_url":"https://www.khronos.org/registry/webgl/extensions/ANGLE_instanced_arrays/","support":{"chrome":{"version_added":"32"},"chrome_android":{"version_added":"32"},"edge":{"version_added":"12"},"firefox":{"version_added":"47"},"firefox_android":{"version_added":true},"ie":{"version_added":"11"},"opera":{"version_added":"19"},"opera_android":{"version_added":"19"},"safari":{"version_added":"8"},"safari_ios":{"version_added":"8"},"samsunginternet_android":{"version_added":"2.0"},"webview_android":{"version_added":"4.4"}},"status":{"experimental":false,"standard_track":true,"deprecated":false}},"drawArraysInstancedANGLE":{"__compat":{"mdn_url":"https://developer.mozilla.org/docs/Web/API/ANGLE_instanced_arrays/drawArraysInstancedANGLE","spec_url":"https://www.khronos.org/registry/webgl/extensions/ANGLE_instanced_arrays/","support":{"chrome":{"version_added":"32"},"chrome_android":{"version_added":"32"},"edge":{"version_added":"12"},"firefox":{"version_added":"47"},"firefox_android":{"version_added":true},"ie":{"version_added":"11"},"opera":{"version_added":"19"},"opera_android":{"version_added":"19"},"safari":{"version_added":"8"},"safari_ios":{"version_added":"8"},"samsunginternet_android":{"version_added":"2.0"},"webview_android":{"version_added":"4.4"}},"status":{"experimental":false,"standard_track":true,"deprecated":false}}},"drawElementsInstancedANGLE":{"__compat":{"mdn_url":"https://developer.mozilla.org/docs/Web/API/ANGLE_instanced_arrays/drawElementsInstancedANGLE","spec_url":"https://www.khronos.org/registry/webgl/extensions/ANGLE_instanced_arrays/","support":{"chrome":{"version_added":"32"},"chrome_android":{"version_added":"32"},"edge":{"version_added":"12"},"firefox":{"version_added":"47"},"firefox_android":{"version_added":true},"ie":{"version_added":"11"},"opera":{"version_added":"19"},"opera_android":{"version_added":"19"},"safari":{"version_added":"8"},"safari_ios":{"version_added":"8"},"samsunginternet_android":{"version_added":"2.0"},"webview_android":{"version_added":"4.4"}},"status":{"experimental":false,"standard_track":true,"deprecated":false}}},"vertexAttribDivisorANGLE":{"__compat":{"mdn_url":"https://developer.mozilla.org/docs/Web/API/ANGLE_instanced_arrays/vertexAttribDivisorANGLE","spec_url":"https://www.khronos.org/registry/webgl/extensions/ANGLE_instanced_arrays/","support":{"chrome":{"version_added":"32"},"chrome_android":{"version_added":"32"},"edge":{"version_added":"12"},"firefox":{"version_added":"47"},"firefox_android":{"version_added":true},"ie":{"version_added":"11"},"opera":{"version_added":"19"},"opera_android":{"version_added":"19"},"safari":{"version_added":"8"},"safari_ios":{"version_added":"8"},"samsunginternet_android":{"version_added":"2.0"},"webview_android":{"version_added":"4.4"}},"status":{"experimental":false,"standard_track":true,"deprecated":false}}}},"AbortController":{"__compat":{"mdn_url":"https://developer.mozilla.org/docs/Web/API/AbortController","spec_url":"https://dom.spec.whatwg.org/#interface-abortcontroller","support":{"chrome":{"version_added":"66"},"chrome_android":{"version_added":"66"},"edge":{"version_added":"16"},"firefox":{"version_added":"57"},"firefox_android":{"version_added":"57"},"ie":{"version_added":false},"nodejs":{"version_added":"15.0.0"},"opera":{"version_added":"53"},"opera_android":{"version_added":"47"},"safari":[{"version_added":"12.1"},{"version_added":"11.1","partial_implementation":true,"notes":"Even though <code>window.AbortController</code> is defined, it doesn't really abort <code>fetch</code> requests. See <a href='https://webkit.org/b/174980'>bug 174980</a>."}],"safari_ios":[{"version_added":"12.2"},{"version_added":"11.3","partial_implementation":true,"notes":"Even though <code>window.AbortController</code> is defined, it doesn't really abort <code>fetch</code> requests. See <a href='https://webkit.org/b/174980'>bug 174980</a>."}],"samsunginternet_android":{"version_added":"9.0"},"webview_android":{"version_added":"66"}},"status":{"experimental":true,"standard_track":true,"deprecated":false}},"AbortController":{"__compat":{"mdn_url":"https://developer.mozilla.org/docs/Web/API/AbortController/AbortController","spec_url":"https://dom.spec.whatwg.org/#ref-for-dom-abortcontroller-abortcontroller①","description":"<code>AbortController()</code> constructor","support":{"chrome":{"version_added":"66"},"chrome_android":{"version_added":"66"},"edge":{"version_added":"16"},"firefox":{"version_added":"57"},"firefox_android":{"version_added":"57"},"ie":{"version_added":false},"nodejs":{"version_added":"15.0.0"},"opera":{"version_added":"53"},"opera_android":{"version_added":"47"},"safari":[{"version_added":"12.1"},{"version_added":"11.1","partial_implementation":true,"notes":"Even though <code>window.AbortController</code> is defined, it doesn't really abort <code>fetch</code> requests. See <a href='https://webkit.org/b/174980'>bug 174980</a>."}],"safari_ios":[{"version_added":"12.2"},{"version_added":"11.3","partial_implementation":true,"notes":"Even though <code>window.AbortController</code> is defined, it doesn't really abort <code>fetch</code> requests. See <a href='https://webkit.org/b/174980'>bug 174980</a>."}],"samsunginternet_android":{"version_added":"9.0"},"webview_android":{"version_added":"66"}},"status":{"experimental":true,"standard_track":true,"deprecated":false}}},"abort":{"__compat":{"mdn_url":"https://developer.mozilla.org/docs/Web/API/AbortController/abort","spec_url":"https://dom.spec.whatwg.org/#ref-for-dom-abortcontroller-abortcontroller①","support":{"chrome":{"version_added":"66"},"chrome_android":{"version_added":"66"},"edge":{"version_added":"16"},"firefox":{"version_added":"57"},"firefox_android":{"version_added":"57"},"ie":{"version_added":false},"nodejs":{"version_added":"15.0.0"},"opera":{"version_added":"53"},"opera_android":{"version_added":"47"},"safari":[{"version_added":"12.1"},{"version_added":"11.1","partial_implementation":true,"notes":"Even though <code>window.AbortController</code> is defined, it doesn't really abort <code>fetch</code> requests. See <a href='https://webkit.org/b/174980'>bug 174980</a>."}],"safari_ios":[{"version_added":"12.2"},{"version_added":"11.3","partial_implementation":true,"notes":"Even though <code>window.AbortController</code> is defined, it doesn't really abort <code>fetch</code> requests. See <a href='https://webkit.org/b/174980'>bug 174980</a>."}],"samsunginternet_android":{"version_added":"9.0"},"webview_android":{"version_added":"66"}},"status":{"experimental":true,"standard_track":true,"deprecated":false}}},"signal":{"__compat":{"mdn_url":"https://developer.mozilla.org/docs/Web/API/AbortController/signal","spec_url":"https://dom.spec.whatwg.org/#ref-for-dom-abortcontroller-signal②","support":{"chrome":{"version_added":"66"},"chrome_android":{"version_added":"66"},"edge":{"version_added":"16"},"firefox":{"version_added":"57"},"firefox_android":{"version_added":"57"},"ie":{"version_added":false},"nodejs":{"version_added":"15.0.0"},"opera":{"version_added":"53"},"opera_android":{"version_added":"47"},"safari":[{"version_added":"12.1"},{"version_added":"11.1","partial_implementation":true,"notes":"Even though <code>window.AbortController</code> is defined, it doesn't really abort <code>fetch</code> requests. See <a href='https://webkit.org/b/174980'>bug 174980</a>."}],"safari_ios":[{"version_added":"12.2"},{"version_added":"11.3","partial_implementation":true,"notes":"Even though <code>window.AbortController</code> is defined, it doesn't really abort <code>fetch</code> requests. See <a href='https://webkit.org/b/174980'>bug 174980</a>."}],"samsunginternet_android":{"version_added":"9.0"},"webview_android":{"version_added":"66"}},"status":{"experimental":true,"standard_track":true,"deprecated":false}}}},"AbortPaymentEvent":{"__compat":{"mdn_url":"https://developer.mozilla.org/docs/Web/API/AbortPaymentEvent","support":{"chrome":{"version_added":"70"},"chrome_android":{"version_added":"70"},"edge":{"version_added":"79"},"firefox":{"version_added":false},"firefox_android":{"version_added":false},"ie":{"version_added":false},"opera":{"version_added":"57"},"opera_android":{"version_added":"49"},"safari":{"version_added":false},"safari_ios":{"version_added":false},"samsunginternet_android":{"version_added":"10.0"},"webview_android":{"version_added":false}},"status":{"experimental":true,"standard_track":false,"deprecated":false}},"AbortPaymentEvent":{"__compat":{"description":"<code>AbortPaymentEvent()</code> constructor","mdn_url":"https://developer.mozilla.org/docs/Web/API/AbortPaymentEvent/AbortPaymentEvent","support":{"chrome":{"version_added":"70"},"chrome_android":{"version_added":"70"},"edge":{"version_added":"79"},"firefox":{"version_added":false},"firefox_android":{"version_added":false},"ie":{"version_added":false},"opera":{"version_added":"57"},"opera_android":{"version_added":"49"},"safari":{"version_added":false},"safari_ios":{"version_added":false},"samsunginternet_android":{"version_added":"10.0"},"webview_android":{"version_added":false}},"status":{"experimental":true,"standard_track":false,"deprecated":false}}},"respondWith":{"__compat":{"mdn_url":"https://developer.mozilla.org/docs/Web/API/AbortPaymentEvent/respondWith","support":{"chrome":{"version_added":"70"},"chrome_android":{"version_added":"70"},"edge":{"version_added":"79"},"firefox":{"version_added":false},"firefox_android":{"version_added":false},"ie":{"version_added":false},"opera":{"version_added":"57"},"opera_android":{"version_added":"49"},"safari":{"version_added":false},"safari_ios":{"version_added":false},"samsunginternet_android":{"version_added":"10.0"},"webview_android":{"version_added":false}},"status":{"experimental":true,"standard_track":false,"deprecated":false}}}},"AbortSignal":{"__compat":{"mdn_url":"https://developer.mozilla.org/docs/Web/API/AbortSignal","spec_url":"https://dom.spec.whatwg.org/#interface-AbortSignal","support":{"chrome":{"version_added":"66"},"chrome_android":{"version_added":"66"},"edge":{"version_added":"16"},"firefox":{"version_added":"57"},"firefox_android":{"version_added":"57"},"ie":{"version_added":false},"nodejs":{"version_added":"15.0.0"},"opera":{"version_added":"53"},"opera_android":{"version_added":"47"},"safari":{"version_added":"11.1"},"safari_ios":{"version_added":"11.3"},"samsunginternet_android":{"version_added":"9.0"},"webview_android":{"version_added":"66"}},"status":{"experimental":false,"standard_track":true,"deprecated":false}},"abort":{"__compat":{"mdn_url":"https://developer.mozilla.org/docs/Web/API/AbortSignal/abort","spec_url":"https://dom.spec.whatwg.org/#ref-for-dom-abortsignal-abort①","support":{"chrome":{"version_added":false},"chrome_android":{"version_added":false},"edge":{"version_added":false},"firefox":{"version_added":"88"},"firefox_android":{"version_added":"88"},"ie":{"version_added":false},"nodejs":{"version_added":false},"opera":{"version_added":false},"opera_android":{"version_added":false},"safari":{"version_added":false},"safari_ios":{"version_added":false},"samsunginternet_android":{"version_added":false},"webview_android":{"version_added":false}},"status":{"experimental":false,"standard_track":true,"deprecated":false}}},"abort_event":{"__compat":{"description":"<code>abort</code> event","mdn_url":"https://developer.mozilla.org/docs/Web/API/AbortSignal/abort_event","spec_url":"https://dom.spec.whatwg.org/#eventdef-abortsignal-abort","support":{"chrome":{"version_added":"66"},"chrome_android":{"version_added":"66"},"edge":{"version_added":"16"},"firefox":{"version_added":"57"},"firefox_android":{"version_added":"57"},"ie":{"version_added":false},"nodejs":{"version_added":"15.0.0"},"opera":{"version_added":"53"},"opera_android":{"version_added":"47"},"safari":{"version_added":"11.1"},"safari_ios":{"version_added":"11.3"},"samsunginternet_android":{"version_added":"9.0"},"webview_android":{"version_added":"66"}},"status":{"experimental":false,"standard_track":true,"deprecated":false}}},"aborted":{"__compat":{"mdn_url":"https://developer.mozilla.org/docs/Web/API/AbortSignal/aborted","spec_url":"https://dom.spec.whatwg.org/#ref-for-dom-abortsignal-aborted①","support":{"chrome":{"version_added":"66"},"chrome_android":{"version_added":"66"},"edge":{"version_added":"16"},"firefox":{"version_added":"57"},"firefox_android":{"version_added":"57"},"ie":{"version_added":false},"nodejs":{"version_added":"15.0.0"},"opera":{"version_added":"53"},"opera_android":{"version_added":"47"},"safari":{"version_added":"11.1"},"safari_ios":{"version_added":"11.3"},"samsunginternet_android":{"version_added":"9.0"},"webview_android":{"version_added":"66"}},"status":{"experimental":false,"standard_track":true,"deprecated":false}}},"onabort":{"__compat":{"mdn_url":"https://developer.mozilla.org/docs/Web/API/AbortSignal/onabort","spec_url":"https://dom.spec.whatwg.org/#abortsignal-onabort","support":{"chrome":{"version_added":"66"},"chrome_android":{"version_added":"66"},"edge":{"version_added":"16"},"firefox":{"version_added":"57"},"firefox_android":{"version_added":"57"},"ie":{"version_added":false},"nodejs":{"version_added":"15.0.0"},"opera":{"version_added":"53"},"opera_android":{"version_added":"47"},"safari":{"version_added":"11.1"},"safari_ios":{"version_added":"11.3"},"samsunginternet_android":{"version_added":"9.0"},"webview_android":{"version_added":"66"}},"status":{"experimental":false,"standard_track":true,"deprecated":false}}}},"AbsoluteOrientationSensor":{"__compat":{"mdn_url":"https://developer.mozilla.org/docs/Web/API/AbsoluteOrientationSensor","spec_url":"https://w3c.github.io/orientation-sensor/#absoluteorientationsensor-interface","support":{"chrome":{"version_added":"67"},"chrome_android":{"version_added":"67"},"edge":{"version_added":"79"},"firefox":{"version_added":false},"firefox_android":{"version_added":false},"ie":{"version_added":false},"opera":{"version_added":"54"},"opera_android":{"version_added":"48"},"safari":{"version_added":false},"safari_ios":{"version_added":false},"samsunginternet_android":{"version_added":"9.0"},"webview_android":{"version_added":"67"}},"status":{"experimental":false,"standard_track":true,"deprecated":false}},"AbsoluteOrientationSensor":{"__compat":{"mdn_url":"https://developer.mozilla.org/docs/Web/API/AbsoluteOrientationSensor/AbsoluteOrientationSensor","spec_url":"https://w3c.github.io/orientation-sensor/#dom-absoluteorientationsensor-absoluteorientationsensor","description":"<code>AbsoluteOrientationSensor()</code> constructor","support":{"chrome":{"version_added":"67"},"chrome_android":{"version_added":"67"},"edge":{"version_added":"79"},"firefox":{"version_added":false},"firefox_android":{"version_added":false},"ie":{"version_added":false},"opera":{"version_added":"54"},"opera_android":{"version_added":"48"},"safari":{"version_added":false},"safari_ios":{"version_added":false},"samsunginternet_android":{"version_added":"9.0"},"webview_android":{"version_added":"67"}},"status":{"experimental":false,"standard_track":true,"deprecated":false}}}},"AbstractRange":{"__compat":{"mdn_url":"https://developer.mozilla.org/docs/Web/API/AbstractRange","spec_url":"https://dom.spec.whatwg.org/#interface-abstractrange","support":{"chrome":{"version_added":"90"},"chrome_android":{"version_added":"90"},"edge":[{"version_added":"90"},{"version_added":"18","version_removed":"79"}],"firefox":{"version_added":"69"},"firefox_android":{"version_added":false},"ie":{"version_added":false},"opera":{"version_added":false},"opera_android":{"version_added":false},"safari":{"version_added":"14.1"},"safari_ios":{"version_added":"14.5"},"samsunginternet_android":{"version_added":false},"webview_android":{"version_added":"90"}},"status":{"experimental":false,"standard_track":true,"deprecated":false}},"collapsed":{"__compat":{"mdn_url":"https://developer.mozilla.org/docs/Web/API/AbstractRange/collapsed","spec_url":"https://dom.spec.whatwg.org/#ref-for-dom-range-collapsed①","support":{"chrome":{"version_added":"90"},"chrome_android":{"version_added":"90"},"edge":[{"version_added":"90"},{"version_added":"18","version_removed":"79"}],"firefox":{"version_added":"69"},"firefox_android":{"version_added":false},"ie":{"version_added":false},"opera":{"version_added":false},"opera_android":"# +
        "\u{1b}[0m\n\u{1b}[38;5;231m    \u{1b}[0m\u{1b}[38;5;231m{\u{1b}[0m\u{1b}[38;5;208m\"\u{1b}[0m\u{1b}[38;5;208mversion_added\u{1b}[0m\u{1b}[38;5;208m\"\u{1b}[0m\u{1b}[38;5;231m:\u{1b}[0m\u{1b}[38;5;141mfalse\u{1b}[0m\u{1b}[38;5;231m}\u{1b}[0m\n";

    bat()
        .arg("-f")
        .arg("--theme")
        .arg("Monokai Extended")
        .arg("-p")
        .arg("longline.json")
        .assert()
        .success()
        .stdout(expected)
        .stderr("");
}

#[test]
fn all_global_git_config_locations_syntax_mapping_work() {
    let fake_home = Path::new(EXAMPLES_DIR).join("git").canonicalize().unwrap();
    let expected = "\u{1b}[38;5;231m[\u{1b}[0m\u{1b}[38;5;149muser\u{1b}[0m\u{1b}[38;5;231m]\u{1b}[0m
\u{1b}[38;5;231m    \u{1b}[0m\u{1b}[38;5;231memail\u{1b}[0m\u{1b}[38;5;231m \u{1b}[0m\u{1b}[38;5;203m=\u{1b}[0m\u{1b}[38;5;231m \u{1b}[0m\u{1b}[38;5;186mfoo@bar.net\u{1b}[0m
\u{1b}[38;5;231m    \u{1b}[0m\u{1b}[38;5;231mname\u{1b}[0m\u{1b}[38;5;231m \u{1b}[0m\u{1b}[38;5;203m=\u{1b}[0m\u{1b}[38;5;231m \u{1b}[0m\u{1b}[38;5;186mfoobar\u{1b}[0m
";

    bat()
        .env("XDG_CONFIG_HOME", fake_home.join(".config").as_os_str())
        .arg("-f")
        .arg("--theme")
        .arg("Monokai Extended")
        .arg("-p")
        .arg("git/.config/git/config")
        .assert()
        .success()
        .stdout(expected)
        .stderr("");

    bat()
        .env("HOME", fake_home.as_os_str())
        .arg("-f")
        .arg("--theme")
        .arg("Monokai Extended")
        .arg("-p")
        .arg("git/.config/git/config")
        .assert()
        .success()
        .stdout(expected)
        .stderr("");

    bat()
        .env("HOME", fake_home.as_os_str())
        .arg("-f")
        .arg("--theme")
        .arg("Monokai Extended")
        .arg("-p")
        .arg("git/.gitconfig")
        .assert()
        .success()
        .stdout(expected)
        .stderr("");
}

#[test]
fn map_syntax_and_ignored_suffix_work_together() {
    bat()
        .arg("-f")
        .arg("--theme")
        .arg("Monokai Extended")
        .arg("-p")
        .arg("--ignored-suffix=.suffix")
        .arg("--map-syntax=*.demo:JSON")
        .arg("test.demo.suffix")
        .assert()
        .success()
        .stdout("\u{1b}[38;5;231m{\u{1b}[0m\u{1b}[38;5;208m\"\u{1b}[0m\u{1b}[38;5;208mtest\u{1b}[0m\u{1b}[38;5;208m\"\u{1b}[0m\u{1b}[38;5;231m:\u{1b}[0m\u{1b}[38;5;231m \u{1b}[0m\u{1b}[38;5;186m\"\u{1b}[0m\u{1b}[38;5;186mvalue\u{1b}[0m\u{1b}[38;5;186m\"\u{1b}[0m\u{1b}[38;5;231m}\u{1b}[0m")
        .stderr("");

    bat()
        .arg("-f")
        .arg("--theme")
        .arg("Monokai Extended")
        .arg("-p")
        .arg("--ignored-suffix=.suffix")
        .arg("--ignored-suffix=.foo")
        .arg("--map-syntax=*.demo:JSON")
        .arg("test.demo.foo.suffix")
        .assert()
        .success()
        .stdout("\u{1b}[38;5;231m{\u{1b}[0m\u{1b}[38;5;208m\"\u{1b}[0m\u{1b}[38;5;208mtest\u{1b}[0m\u{1b}[38;5;208m\"\u{1b}[0m\u{1b}[38;5;231m:\u{1b}[0m\u{1b}[38;5;231m \u{1b}[0m\u{1b}[38;5;186m\"\u{1b}[0m\u{1b}[38;5;186mvalue\u{1b}[0m\u{1b}[38;5;186m\"\u{1b}[0m\u{1b}[38;5;231m}\u{1b}[0m")
        .stderr("");
}

#[test]
fn acknowledgements() {
    bat()
        .arg("--acknowledgements")
        .assert()
        .success()
        .stdout(
            // Just some sanity checking that avoids names of persons, except our own Keith Hall :)
            predicate::str::contains(
                "Copyright (c) 2018-2021 bat-developers (https://github.com/sharkdp/bat).",
            )
            .and(predicate::str::contains(
                "Copyright (c) 2012-2020 The Sublime CMake authors",
            ))
            .and(predicate::str::contains(
                "Copyright 2014-2015 SaltStack Team",
            ))
            .and(predicate::str::contains(
                "Copyright (c) 2013-present Dracula Theme",
            ))
            .and(predicate::str::contains(
                "## syntaxes/01_Packages/Rust/LICENSE.txt",
            ))
            .and(predicate::str::contains(
                "## syntaxes/02_Extra/http-request-response/LICENSE",
            ))
            .and(predicate::str::contains(
                "## themes/dracula-sublime/LICENSE",
            ))
            .and(predicate::str::contains("Copyright (c) 2017 b123400"))
            .and(predicate::str::contains("Copyright (c) 2021 Keith Hall"))
            .and(predicate::str::contains("Copyright 2014 Clams")),
        )
        .stderr("");
}

#[cfg(unix)] // Expected output assumed that tests are run on a Unix-like system
#[cfg(feature = "lessopen")]
#[test]
fn lessopen_file_piped() {
    bat()
        .env("LESSOPEN", "|echo File is %s")
        .arg("--lessopen")
        .arg("test.txt")
        .assert()
        .success()
        .stdout("File is test.txt\n");
}

#[cfg(unix)] // Expected output assumed that tests are run on a Unix-like system
#[cfg(feature = "lessopen")]
#[test]
fn lessopen_stdin_piped() {
    bat()
        .env("LESSOPEN", "|cat")
        .arg("--lessopen")
        .write_stdin("hello world\n")
        .assert()
        .success()
        .stdout("hello world\n");
}

#[cfg(unix)] // Expected output assumed that tests are run on a Unix-like system
#[cfg(feature = "lessopen")]
#[test]
fn lessopen_and_lessclose_file_temp() {
    // This is mainly to test that $LESSCLOSE gets passed the correct file paths
    // In this case, the original file and the temporary file returned by $LESSOPEN
    bat()
        // Need a %s for $LESSOPEN to be valid
        .env("LESSOPEN", "echo empty.txt && echo %s >/dev/null")
        .env("LESSCLOSE", "echo lessclose: %s %s")
        .arg("--lessopen")
        .arg("test.txt")
        .assert()
        .success()
        .stdout("lessclose: test.txt empty.txt\n");
}

#[cfg(unix)] // Expected output assumed that tests are run on a Unix-like system
#[cfg(feature = "lessopen")]
#[test]
fn lessopen_and_lessclose_file_piped() {
    // This is mainly to test that $LESSCLOSE gets passed the correct file paths
    // In these cases, the original file and a dash
    bat()
        // This test will not work properly if $LESSOPEN does not output anything
        .env("LESSOPEN", "|cat %s")
        .env("LESSCLOSE", "echo lessclose: %s %s")
        .arg("--lessopen")
        .arg("test.txt")
        .assert()
        .success()
        .stdout("hello world\nlessclose: test.txt -\n");

    bat()
        .env("LESSOPEN", "||cat %s")
        .env("LESSCLOSE", "echo lessclose: %s %s")
        .arg("--lessopen")
        .arg("empty.txt")
        .assert()
        .success()
        .stdout("lessclose: empty.txt -\n");
}

#[cfg(unix)] // Expected output assumed that tests are run on a Unix-like system
#[cfg(feature = "lessopen")]
#[test]
fn lessopen_and_lessclose_stdin_temp() {
    // This is mainly to test that $LESSCLOSE gets passed the correct file paths
    // In this case, a dash and the temporary file returned by $LESSOPEN
    bat()
        // Need a %s for $LESSOPEN to be valid
        .env("LESSOPEN", "-echo empty.txt && echo %s >/dev/null")
        .env("LESSCLOSE", "echo lessclose: %s %s")
        .arg("--lessopen")
        .write_stdin("test.txt")
        .assert()
        .success()
        .stdout("lessclose: - empty.txt\n");
}

#[cfg(unix)] // Expected output assumed that tests are run on a Unix-like system
#[cfg(feature = "lessopen")]
#[test]
fn lessopen_and_lessclose_stdin_piped() {
    // This is mainly to test that $LESSCLOSE gets passed the correct file paths
    // In these cases, two dashes
    bat()
        // This test will not work properly if $LESSOPEN does not output anything
        // Need a %s for $LESSOPEN to be valid
        .env("LESSOPEN", "|-cat test.txt && echo %s >/dev/null")
        .env("LESSCLOSE", "echo lessclose: %s %s")
        .arg("--lessopen")
        .write_stdin("empty.txt")
        .assert()
        .success()
        .stdout("hello world\nlessclose: - -\n");

    bat()
        // Need a %s for $LESSOPEN to be valid
        .env("LESSOPEN", "||-cat empty.txt && echo %s >/dev/null")
        .env("LESSCLOSE", "echo lessclose: %s %s")
        .arg("--lessopen")
        .write_stdin("empty.txt")
        .assert()
        .success()
        .stdout("lessclose: - -\n");
}

#[cfg(unix)] // Expected output assumed that tests are run on a Unix-like system
#[cfg(feature = "lessopen")]
#[test]
fn lessopen_handling_empty_output_file() {
    bat()
        // Need a %s for $LESSOPEN to be valid
        .env("LESSOPEN", "|cat empty.txt && echo %s >/dev/null")
        .arg("--lessopen")
        .arg("test.txt")
        .assert()
        .success()
        .stdout("hello world\n");

    bat()
        // Need a %s for $LESSOPEN to be valid
        .env("LESSOPEN", "|cat nonexistent.txt && echo %s >/dev/null")
        .arg("--lessopen")
        .arg("test.txt")
        .assert()
        .success()
        .stdout("hello world\n");

    bat()
        // Need a %s for $LESSOPEN to be valid
        .env("LESSOPEN", "||cat empty.txt && echo %s >/dev/null")
        .arg("--lessopen")
        .arg("test.txt")
        .assert()
        .success()
        .stdout("");

    bat()
        // Need a %s for $LESSOPEN to be valid
        .env("LESSOPEN", "||cat nonexistent.txt && echo %s >/dev/null")
        .arg("--lessopen")
        .arg("test.txt")
        .assert()
        .success()
        .stdout("hello world\n");
}

#[cfg(unix)] // Expected output assumed that tests are run on a Unix-like system
#[cfg(feature = "lessopen")]
#[test]
// FIXME
fn lessopen_handling_empty_output_stdin() {
    bat()
        // Need a %s for $LESSOPEN to be valid
        .env("LESSOPEN", "|-cat empty.txt && echo %s >/dev/null")
        .arg("--lessopen")
        .write_stdin("hello world\n")
        .assert()
        .success()
        .stdout("hello world\n");

    bat()
        // Need a %s for $LESSOPEN to be valid
        .env("LESSOPEN", "|-cat nonexistent.txt && echo %s >/dev/null")
        .arg("--lessopen")
        .write_stdin("hello world\n")
        .assert()
        .success()
        .stdout("hello world\n");

    bat()
        // Need a %s for $LESSOPEN to be valid
        .env("LESSOPEN", "||-cat empty.txt && echo %s >/dev/null")
        .arg("--lessopen")
        .write_stdin("hello world\n")
        .assert()
        .success()
        .stdout("");

    bat()
        // Need a %s for $LESSOPEN to be valid
        .env("LESSOPEN", "||-cat nonexistent.txt && echo %s >/dev/null")
        .arg("--lessopen")
        .write_stdin("hello world\n")
        .assert()
        .success()
        .stdout("hello world\n");
}

#[cfg(unix)] // Expected output assumed that tests are run on a Unix-like system
#[cfg(feature = "lessopen")]
#[test]
fn lessopen_uses_shell() {
    bat()
        .env("LESSOPEN", "|cat < %s")
        .arg("--lessopen")
        .arg("test.txt")
        .assert()
        .success()
        .stdout("hello world\n");
}

#[cfg(unix)]
#[cfg(feature = "lessopen")]
#[test]
fn do_not_use_lessopen_by_default() {
    bat()
        .env("LESSOPEN", "|echo File is %s")
        .arg("test.txt")
        .assert()
        .success()
        .stdout("hello world\n");
}

#[cfg(unix)]
#[cfg(feature = "lessopen")]
#[test]
fn do_not_use_lessopen_if_overridden() {
    bat()
        .env("LESSOPEN", "|echo File is %s")
        .arg("--lessopen")
        .arg("--no-lessopen")
        .arg("test.txt")
        .assert()
        .success()
        .stdout("hello world\n");
}

#[cfg(unix)]
#[cfg(feature = "lessopen")]
#[test]
fn lessopen_validity() {
    bat()
        .env("LESSOPEN", "|echo File is test.txt")
        .arg("--lessopen")
        .arg("test.txt")
        .assert()
        .success()
        .stdout("hello world\n")
        .stderr(
            "\u{1b}[33m[bat warning]\u{1b}[0m: LESSOPEN ignored: must contain exactly one %s\n",
        );

    bat()
        .env("LESSOPEN", "|echo File is %s")
        .arg("--lessopen")
        .arg("test.txt")
        .assert()
        .success()
        .stdout("File is test.txt\n")
        .stderr("");

    bat()
        .env("LESSOPEN", "|echo %s is %s")
        .arg("--lessopen")
        .arg("test.txt")
        .assert()
        .success()
        .stdout("hello world\n")
        .stderr(
            "\u{1b}[33m[bat warning]\u{1b}[0m: LESSOPEN ignored: must contain exactly one %s\n",
        );
}

// Regression test for issue #2520 and PR #2650
// Syntax highlighting should be the same regardless of
// --map-syntax' case or file extension's case
#[test]
fn highlighting_independant_from_map_syntax_case() {
    let expected = bat()
        .arg("-f")
        .arg("--map-syntax=*.config:JSON")
        .arg("map-syntax_case.Config")
        .assert()
        .get_output()
        .stdout
        .clone();

    bat()
        .arg("-f")
        .arg("--map-syntax=*.Config:JSON")
        .arg("map-syntax_case.Config")
        .assert()
        .success()
        .stdout(expected)
        .stderr("");
}

#[test]
fn map_syntax_target_syntax_case_insensitive() {
    let expected = bat()
        .arg("-f")
        .arg("--map-syntax=*.config:json")
        .arg("map-syntax_case.Config")
        .assert()
        .get_output()
        .stdout
        .clone();

    bat()
        .arg("-f")
        .arg("--map-syntax=*.config:json")
        .arg("map-syntax_case.Config")
        .assert()
        .success()
        .stdout(expected)
        .stderr("");
}

#[test]
fn strip_ansi_always_strips_ansi() {
    bat()
        .arg("--style=plain")
        .arg("--decorations=always")
        .arg("--color=never")
        .arg("--strip-ansi=always")
        .write_stdin("\x1B[33mYellow\x1B[m")
        .assert()
        .success()
        .stdout("Yellow");
}

#[test]
fn strip_ansi_never_does_not_strip_ansi() {
    let output = String::from_utf8(
        bat()
            .arg("--style=plain")
            .arg("--decorations=always")
            .arg("--color=never")
            .arg("--strip-ansi=never")
            .write_stdin("\x1B[33mYellow\x1B[m")
            .assert()
            .success()
            .get_output()
            .stdout
            .clone(),
    )
    .expect("valid utf8");

    assert!(output.contains("\x1B[33mYellow"))
}

#[test]
fn strip_ansi_does_not_affect_simple_printer() {
    let output = String::from_utf8(
        bat()
            .arg("--style=plain")
            .arg("--decorations=never")
            .arg("--color=never")
            .arg("--strip-ansi=always")
            .write_stdin("\x1B[33mYellow\x1B[m")
            .assert()
            .success()
            .get_output()
            .stdout
            .clone(),
    )
    .expect("valid utf8");

    assert!(output.contains("\x1B[33mYellow"))
}

#[test]
fn strip_ansi_does_not_strip_when_show_nonprintable() {
    let output = String::from_utf8(
        bat()
            .arg("--style=plain")
            .arg("--decorations=never")
            .arg("--color=always")
            .arg("--strip-ansi=always")
            .arg("--show-nonprintable")
            .write_stdin("\x1B[33mY")
            .assert()
            .success()
            .get_output()
            .stdout
            .clone(),
    )
    .expect("valid utf8");

    assert!(output.contains("␛"))
}

#[test]
fn strip_ansi_auto_strips_ansi_when_detected_syntax_by_filename() {
    bat()
        .arg("--style=plain")
        .arg("--decorations=always")
        .arg("--color=never")
        .arg("--strip-ansi=auto")
        .arg("--file-name=test.rs")
        .write_stdin("fn \x1B[33mYellow\x1B[m() -> () {}")
        .assert()
        .success()
        .stdout("fn Yellow() -> () {}");
}

#[test]
fn strip_ansi_auto_strips_ansi_when_provided_syntax_by_option() {
    bat()
        .arg("--style=plain")
        .arg("--decorations=always")
        .arg("--color=never")
        .arg("--strip-ansi=auto")
        .arg("--language=rust")
        .write_stdin("fn \x1B[33mYellow\x1B[m() -> () {}")
        .assert()
        .success()
        .stdout("fn Yellow() -> () {}");
}

#[test]
fn strip_ansi_auto_does_not_strip_when_plain_text_by_filename() {
    let output = String::from_utf8(
        bat()
            .arg("--style=plain")
            .arg("--decorations=always")
            .arg("--color=never")
            .arg("--strip-ansi=auto")
            .arg("--file-name=ansi.txt")
            .write_stdin("\x1B[33mYellow\x1B[m")
            .assert()
            .success()
            .get_output()
            .stdout
            .clone(),
    )
    .expect("valid utf8");

    assert!(output.contains("\x1B[33mYellow"))
}

#[test]
fn strip_ansi_auto_does_not_strip_ansi_when_plain_text_by_option() {
    let output = String::from_utf8(
        bat()
            .arg("--style=plain")
            .arg("--decorations=always")
            .arg("--color=never")
            .arg("--strip-ansi=auto")
            .arg("--language=txt")
            .write_stdin("\x1B[33mYellow\x1B[m")
            .assert()
            .success()
            .get_output()
            .stdout
            .clone(),
    )
    .expect("valid utf8");

    assert!(output.contains("\x1B[33mYellow"))
}

// Tests that style components can be removed with `-component`.
#[test]
fn style_components_can_be_removed() {
    bat()
        .arg({
            #[cfg(not(feature = "git"))]
            {
                "--style=full,-grid"
            }
            #[cfg(feature = "git")]
            {
                "--style=full,-grid,-changes"
            }
        })
        .arg("--decorations=always")
        .arg("--color=never")
        .write_stdin("test")
        .assert()
        .success()
        .stdout("     STDIN\n     Size: -\n   1 test\n")
        .stderr("");
}

// Tests that style components are chosen based on the rightmost `--style` argument.
#[test]
fn style_components_can_be_overidden() {
    bat()
        .arg("--style=full")
        .arg("--style=header,numbers")
        .arg("--decorations=always")
        .arg("--color=never")
        .write_stdin("test")
        .assert()
        .success()
        .stdout("     STDIN\n   1 test\n")
        .stderr("");
}

// Tests that style components can be merged across multiple `--style` arguments.
#[test]
fn style_components_will_merge() {
    bat()
        .arg("--style=header,grid")
        .arg("--style=-grid,+numbers")
        .arg("--decorations=always")
        .arg("--color=never")
        .write_stdin("test")
        .assert()
        .success()
        .stdout("     STDIN\n   1 test\n")
        .stderr("");
}

// Tests that style components can be merged with the `BAT_STYLE` environment variable.
#[test]
fn style_components_will_merge_with_env_var() {
    bat()
        .env("BAT_STYLE", "header,grid")
        .arg("--style=-grid,+numbers")
        .arg("--decorations=always")
        .arg("--color=never")
        .write_stdin("test")
        .assert()
        .success()
        .stdout("     STDIN\n   1 test\n")
        .stderr("");
}

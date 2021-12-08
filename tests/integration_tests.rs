use assert_cmd::cargo::CommandCargoExt;
use predicates::{prelude::predicate, str::PredicateStrExt};
use serial_test::serial;
use std::path::Path;
use std::process::Command;
use std::str::from_utf8;
use tempfile::tempdir;

#[cfg(unix)]
mod unix {
    pub use std::fs::File;
    pub use std::io::{self, Write};
    pub use std::os::unix::io::FromRawFd;
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
use utils::mocked_pagers;

const EXAMPLES_DIR: &str = "tests/examples";

fn bat_raw_command_with_config() -> Command {
    let mut cmd = Command::cargo_bin("bat").unwrap();
    cmd.current_dir("tests/examples");
    cmd.env_remove("BAT_CACHE_PATH");
    cmd.env_remove("BAT_CONFIG_DIR");
    cmd.env_remove("BAT_CONFIG_PATH");
    cmd.env_remove("BAT_OPTS");
    cmd.env_remove("BAT_PAGER");
    cmd.env_remove("BAT_STYLE");
    cmd.env_remove("BAT_TABS");
    cmd.env_remove("BAT_THEME");
    cmd.env_remove("COLORTERM");
    cmd.env_remove("NO_COLOR");
    cmd.env_remove("PAGER");
    cmd
}

fn bat_raw_command() -> Command {
    let mut cmd = bat_raw_command_with_config();
    cmd.arg("--no-config");
    cmd
}

fn bat_with_config() -> assert_cmd::Command {
    assert_cmd::Command::from_std(bat_raw_command_with_config())
}

fn bat() -> assert_cmd::Command {
    assert_cmd::Command::from_std(bat_raw_command())
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
        .stdout("   1 line 1\n   2 line 2\n   3 line 3\n   4 line 4\n");
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
        .arg("--line-range=2:")
        .assert()
        .success()
        .stdout("line 2\nline 3\nline 4\n");
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
fn no_args_doesnt_break() {
    // To simulate bat getting started from the shell, a process is created with stdin and stdout
    // as the slave end of a pseudo terminal. Although both point to the same "file", bat should
    // not exit, because in this case it is safe to read and write to the same fd, which is why
    // this test exists.
    let OpenptyResult { master, slave } = openpty(None, None).expect("Couldn't open pty.");
    let mut master = unsafe { File::from_raw_fd(master) };
    let stdin = unsafe { Stdio::from_raw_fd(slave) };
    let stdout = unsafe { Stdio::from_raw_fd(slave) };

    let mut child = bat_raw_command()
        .stdin(stdin)
        .stdout(stdout)
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
fn pager_basic() {
    bat()
        .env("PAGER", "echo pager-output")
        .arg("--paging=always")
        .arg("test.txt")
        .assert()
        .success()
        .stdout(predicate::eq("pager-output\n").normalize());
}

#[test]
fn pager_overwrite() {
    bat()
        .env("PAGER", "echo other-pager")
        .env("BAT_PAGER", "echo pager-output")
        .arg("--paging=always")
        .arg("test.txt")
        .assert()
        .success()
        .stdout(predicate::eq("pager-output\n").normalize());
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
fn alias_pager_disable_long_overrides_short() {
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
fn config_read_arguments_from_file() {
    bat_with_config()
        .env("BAT_CONFIG_PATH", "bat.conf")
        .arg("test.txt")
        .assert()
        .success()
        .stdout(predicate::eq("dummy-pager-from-config\n").normalize());
}

#[test]
fn utf16() {
    // The output will be converted to UTF-8 with a leading UTF-8 BOM
    bat()
        .arg("--plain")
        .arg("--decorations=always")
        .arg("test_UTF-16LE.txt")
        .assert()
        .success()
        .stdout(std::str::from_utf8(b"\xEF\xBB\xBFhello world\n").unwrap());
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
fn filename_basic() {
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
fn filename_binary() {
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
        .stderr("\x1b[33m[bat warning]\x1b[0m: Style 'rule' is a subset of style 'grid', 'rule' will not be visible.\n");
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
            .arg(&format!("regression_tests/{}", filename))
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
        .arg(&format!("--file-name={}", file))
        .arg("--style=plain")
        .arg(file)
        .assert()
        .success();

    let cmd_for_stdin = bat()
        .arg("--color=always")
        .arg("--map-syntax=*.js:Markdown")
        .arg("--style=plain")
        .arg(&format!("--file-name={}", file))
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
        .arg(&format!("--file-name={}", file))
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
───────┼────────────────────────────────────────────────────────────────────────
   1   │ Single Line
───────┴────────────────────────────────────────────────────────────────────────
",
        )
        .stderr("");
}

// Ensure that ANSI passthrough is emitted properly for both wrapping and non-wrapping printer.
#[test]
fn ansi_passthrough_emit() {
    for wrapping in &["never", "character"] {
        bat()
            .arg("--paging=never")
            .arg("--color=never")
            .arg("--terminal-width=80")
            .arg(format!("--wrap={}", wrapping))
            .arg("--decorations=always")
            .arg("--style=plain")
            .write_stdin("\x1B[33mColor\nColor \x1B[m\nPlain\n")
            .assert()
            .success()
            .stdout("\x1B[33m\x1B[33mColor\n\x1B[33mColor \x1B[m\nPlain\n")
            .stderr("");
    }
}

#[test]
fn ignored_suffix_arg() {
    bat()
        .arg("-f")
        .arg("-p")
        .arg("test.json~")
        .assert()
        .success()
        .stdout("\u{1b}[38;5;231m{\u{1b}[0m\u{1b}[38;5;208m\"\u{1b}[0m\u{1b}[38;5;208mtest\u{1b}[0m\u{1b}[38;5;208m\"\u{1b}[0m\u{1b}[38;5;231m:\u{1b}[0m\u{1b}[38;5;231m \u{1b}[0m\u{1b}[38;5;186m\"\u{1b}[0m\u{1b}[38;5;186mvalue\u{1b}[0m\u{1b}[38;5;186m\"\u{1b}[0m\u{1b}[38;5;231m}\u{1b}[0m")
        .stderr("");

    bat()
        .arg("-f")
        .arg("-p")
        .arg("--ignored-suffix=.suffix")
        .arg("test.json.suffix")
        .assert()
        .success()
        .stdout("\u{1b}[38;5;231m{\u{1b}[0m\u{1b}[38;5;208m\"\u{1b}[0m\u{1b}[38;5;208mtest\u{1b}[0m\u{1b}[38;5;208m\"\u{1b}[0m\u{1b}[38;5;231m:\u{1b}[0m\u{1b}[38;5;231m \u{1b}[0m\u{1b}[38;5;186m\"\u{1b}[0m\u{1b}[38;5;186mvalue\u{1b}[0m\u{1b}[38;5;186m\"\u{1b}[0m\u{1b}[38;5;231m}\u{1b}[0m")
        .stderr("");

    bat()
        .arg("-f")
        .arg("-p")
        .arg("test.json.suffix")
        .assert()
        .success()
        .stdout("\u{1b}[38;5;231m{\"test\": \"value\"}\u{1b}[0m")
        .stderr("");
}

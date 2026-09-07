mod utils;

use std::io::{Read, Write};
use std::process::{Command, Stdio};
use std::sync::mpsc;
use std::time::{Duration, Instant};
use utils::command::bat;

fn streaming_command() -> Command {
    let mut command = Command::new(assert_cmd::cargo::cargo_bin!("bat"));
    command
        .args([
            "--no-config",
            "--paging=never",
            "--color=never",
            "--style=plain",
        ])
        .env_remove("BAT_OPTS")
        .env_remove("LESSOPEN")
        .env_remove("LESSCLOSE")
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());
    command
}

fn read_prefix(
    stdout: std::process::ChildStdout,
    length: usize,
) -> mpsc::Receiver<std::io::Result<Vec<u8>>> {
    let (sender, receiver) = mpsc::channel();
    std::thread::spawn(move || {
        let mut output = vec![0; length];
        let result = stdout
            .take(length as u64)
            .read_exact(&mut output)
            .map(|_| output);
        sender.send(result).ok();
    });
    receiver
}

fn wait_or_kill(child: &mut std::process::Child) -> std::process::ExitStatus {
    let deadline = Instant::now() + Duration::from_secs(5);
    loop {
        if let Some(status) = child.try_wait().unwrap() {
            return status;
        }
        if Instant::now() >= deadline {
            child.kill().ok();
            child.wait().ok();
            panic!("bat did not exit after its output pipe closed");
        }
        std::thread::sleep(Duration::from_millis(10));
    }
}

#[test]
fn short_input_is_visible_before_newline_or_eof() {
    let mut child = streaming_command().stdin(Stdio::piped()).spawn().unwrap();
    let stdout = child.stdout.take().unwrap();
    let mut stdin = child.stdin.take().unwrap();
    let received = read_prefix(stdout, 5);
    stdin.write_all(b"ready").unwrap();
    stdin.flush().unwrap();
    let result = received.recv_timeout(Duration::from_secs(5));
    if result.is_err() {
        child.kill().ok();
    }
    drop(stdin);
    let status = wait_or_kill(&mut child);
    assert_eq!(result.unwrap().unwrap(), b"ready");
    assert!(status.success());
}

#[test]
#[cfg(unix)]
fn infinite_binary_input_stops_when_the_output_pipe_closes() {
    let mut child = streaming_command().arg("/dev/zero").spawn().unwrap();
    let received = read_prefix(child.stdout.take().unwrap(), 10);
    let result = received.recv_timeout(Duration::from_secs(5));
    if result.is_err() {
        child.kill().ok();
    }
    let status = wait_or_kill(&mut child);
    assert_eq!(result.unwrap().unwrap(), vec![0; 10]);
    assert!(status.success());
}

#[test]
fn byte_content_is_preserved_and_line_options_still_apply() {
    let input = (0..=255).cycle().take(100_000).collect::<Vec<u8>>();
    bat()
        .write_stdin(input.clone())
        .assert()
        .success()
        .stdout(input);
    bat()
        .arg("--line-range=2:3")
        .write_stdin("one\ntwo\nthree\nfour\n")
        .assert()
        .success()
        .stdout("two\nthree\n");
    bat()
        .arg("--squeeze-blank")
        .write_stdin("one\n\n\ntwo\n")
        .assert()
        .success()
        .stdout("one\n\ntwo\n");
}

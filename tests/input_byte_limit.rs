mod utils;
use utils::command::bat;

#[test]
fn limits_apply_before_first_line_buffering_and_can_split_characters() {
    for (args, expected) in [
        (vec!["--max-bytes=3"], b"abc".as_slice()),
        (vec!["--head=0"], b""),
        (vec!["--max-bytes=99"], b"abcdef"),
        (vec!["--max-bytes=2", "--max-bytes=4"], b"abcd"),
    ] {
        bat()
            .args(args)
            .write_stdin("abcdef")
            .assert()
            .success()
            .stdout(expected);
    }
    bat()
        .arg("--max-bytes=1")
        .write_stdin("é\n")
        .assert()
        .success()
        .stdout(vec![0xc3]);
}

#[test]
fn each_file_and_stdin_occurrence_has_its_own_limit() {
    let dir = tempfile::tempdir().unwrap();
    for name in ["one", "two"] {
        std::fs::write(dir.path().join(name), "abcdef").unwrap();
    }
    bat()
        .arg("--max-bytes=2")
        .arg(dir.path().join("one"))
        .arg(dir.path().join("two"))
        .assert()
        .success()
        .stdout("abab");
    bat()
        .args(["--max-bytes=2", "-", "-"])
        .write_stdin("abcdef")
        .assert()
        .success()
        .stdout("abcd");
}

#[test]
#[cfg(unix)]
fn a_device_without_newlines_is_bounded() {
    bat()
        .args(["--max-bytes=17", "--binary=as-text", "/dev/zero"])
        .timeout(std::time::Duration::from_secs(5))
        .assert()
        .success()
        .stdout(vec![0; 17]);
}

#[test]
fn library_limits_underlying_reader_requests() {
    struct LimitedSource;
    impl std::io::Read for LimitedSource {
        fn read(&mut self, output: &mut [u8]) -> std::io::Result<usize> {
            assert!(output.len() <= 7, "read beyond configured byte limit");
            output.fill(b'x');
            Ok(output.len())
        }
    }
    let mut output = String::new();
    bat::PrettyPrinter::new()
        .colored_output(false)
        .input(bat::Input::from_reader(Box::new(LimitedSource)).max_bytes(7))
        .max_bytes(9)
        .print_with_writer(Some(&mut output))
        .unwrap();
    assert_eq!(output, "xxxxxxx");
}

#[test]
#[cfg(all(unix, feature = "lessopen"))]
fn preprocessor_output_is_limited() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("input.txt");
    std::fs::write(&path, "abcdef").unwrap();
    bat()
        .env("LESSOPEN", "|cat %s")
        .args(["--lessopen", "--max-bytes=3"])
        .arg(path)
        .assert()
        .success()
        .stdout("abc");
}

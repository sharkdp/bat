use std::io::{self, Cursor, Write};

use bat::assets::HighlightingAssets;
use bat::config::Config;
use bat::controller::Controller;
use bat::input::Input;
use bat::output::OutputHandle;
use bat::style::StyleComponent;
use bat::WrappingMode;

#[derive(Default)]
struct RecordingWriter {
    writes: Vec<Vec<u8>>,
    maximum_write: Option<usize>,
}

impl Write for RecordingWriter {
    fn write(&mut self, bytes: &[u8]) -> io::Result<usize> {
        let size = self.maximum_write.unwrap_or(bytes.len()).min(bytes.len());
        self.writes.push(bytes[..size].to_vec());
        Ok(size)
    }
    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

fn config(wrapping_mode: WrappingMode) -> Config<'static> {
    let mut config = Config {
        language: Some("txt"),
        term_width: 38,
        tab_width: 4,
        colored_output: true,
        true_color: true,
        theme: "Monokai Extended".to_owned(),
        wrapping_mode,
        ..Default::default()
    };
    config.style_components.insert(StyleComponent::LineNumbers);
    config
}

const TEXT: &[u8] = b"* \x1b[31m12345678\x1b[0m - \x1b[33mHEAD\x1b[0m colored history\n* \x1b[32mabcdefgh\x1b[0m a longer line of text that wraps several times\nlast";

#[test]
fn colored_and_wrapped_lines_are_submitted_as_whole_lines() {
    let assets = HighlightingAssets::from_binary();
    for wrapping in [
        WrappingMode::NoWrapping(false),
        WrappingMode::Character,
        WrappingMode::Word,
    ] {
        let config = config(wrapping);
        let controller = Controller::new(&config, &assets);
        let mut writer = RecordingWriter::default();
        assert!(controller
            .run(
                vec![Input::from_reader(Box::new(Cursor::new(TEXT)))],
                Some(&mut OutputHandle::IoWrite(&mut writer)),
            )
            .unwrap());
        assert_eq!(writer.writes.len(), 3, "{:?}", config.wrapping_mode);
        let mut expected = String::new();
        controller
            .run(
                vec![Input::from_reader(Box::new(Cursor::new(TEXT)))],
                Some(&mut OutputHandle::FmtWrite(&mut expected)),
            )
            .unwrap();
        assert_eq!(writer.writes.concat(), expected.as_bytes());
        assert!(expected.contains("12345678") && expected.contains("last"));
    }
}

#[test]
fn short_writes_do_not_truncate_or_repeat_rendered_bytes() {
    let assets = HighlightingAssets::from_binary();
    let config = config(WrappingMode::Character);
    let controller = Controller::new(&config, &assets);
    let mut writer = RecordingWriter {
        maximum_write: Some(3),
        ..Default::default()
    };
    controller
        .run(
            vec![Input::from_reader(Box::new(Cursor::new(TEXT)))],
            Some(&mut OutputHandle::IoWrite(&mut writer)),
        )
        .unwrap();
    let mut expected = String::new();
    controller
        .run(
            vec![Input::from_reader(Box::new(Cursor::new(TEXT)))],
            Some(&mut OutputHandle::FmtWrite(&mut expected)),
        )
        .unwrap();
    assert_eq!(writer.writes.concat(), expected.as_bytes());
}

#[test]
fn failed_output_preserves_the_io_error_kind() {
    struct Fails;
    impl Write for Fails {
        fn write(&mut self, _: &[u8]) -> io::Result<usize> {
            Err(io::ErrorKind::BrokenPipe.into())
        }
        fn flush(&mut self) -> io::Result<()> {
            Ok(())
        }
    }
    let assets = HighlightingAssets::from_binary();
    let config = config(WrappingMode::Character);
    let mut kind = None;
    let successful = Controller::new(&config, &assets)
        .run_with_error_handler(
            vec![Input::from_reader(Box::new(Cursor::new(TEXT)))],
            Some(&mut OutputHandle::IoWrite(&mut Fails)),
            |error, _| {
                if let bat::error::Error::Io(error) = error {
                    kind = Some(error.kind());
                }
            },
        )
        .unwrap();
    assert!(!successful);
    assert_eq!(kind, Some(io::ErrorKind::BrokenPipe));
}

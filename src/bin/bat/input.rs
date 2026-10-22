use bat::input::Input;
use std::path::Path;

pub fn new_file_input<'a>(file: &'a Path, name: Option<&'a Path>) -> Input<'a> {
    named(Input::ordinary_file(file), name.or(Some(file)))
}

pub fn new_stdin_input(name: Option<&Path>) -> Input<'_> {
    let detected_name = if name.is_none() { stdin_path() } else { None };
    named(Input::stdin(), name.or(detected_name.as_deref()))
}

/// Recover a filename hint without opening or reading the redirected file.
/// The input remains stdin, so its current offset and IO-circle checks are preserved.
#[cfg(any(target_os = "linux", target_os = "android"))]
fn stdin_path() -> Option<std::path::PathBuf> {
    let descriptor = Path::new("/proc/self/fd/0");
    if !descriptor.metadata().ok()?.is_file() {
        return None;
    }

    // canonicalize also rejects deleted files and anonymous descriptors whose
    // procfs link target is a descriptive label rather than an existing path.
    std::fs::canonicalize(descriptor).ok()
}

#[cfg(not(any(target_os = "linux", target_os = "android")))]
fn stdin_path() -> Option<std::path::PathBuf> {
    None
}

fn named<'a>(input: Input<'a>, name: Option<&Path>) -> Input<'a> {
    if let Some(provided_name) = name {
        let mut input = input.with_name(Some(provided_name));
        input.description_mut().set_kind(Some("File".to_owned()));
        input
    } else {
        input
    }
}

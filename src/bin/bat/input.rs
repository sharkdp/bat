use bat::input::Input;
use std::ffi::OsStr;

pub fn new_file_input<'a>(file: &'a OsStr, name: Option<&'a OsStr>) -> Input<'a> {
    named(Input::ordinary_file(file), name.or_else(|| Some(file)))
}

pub fn new_stdin_input(name: Option<&OsStr>) -> Input {
    named(Input::stdin(), name)
}

fn named<'a>(input: Input<'a>, name: Option<&OsStr>) -> Input<'a> {
    if let Some(provided_name) = name {
        let mut input = input.with_name(Some(provided_name));
        input.description_mut().set_kind(Some("File".to_owned()));
        input
    } else {
        input
    }
}

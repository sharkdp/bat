use bat::input::Input;
use std::path::Path;

pub fn new_file_input<'a>(file: &'a Path, name: Option<&'a Path>) -> Input<'a> {
    named(Input::ordinary_file(file), name.or(Some(file)))
}

pub fn new_stdin_input(name: Option<&Path>) -> Input<'_> {
    named(Input::stdin(), name)
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

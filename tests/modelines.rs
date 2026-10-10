mod utils;

use utils::command::bat;

fn output(args: &[&str], input: &str) -> Vec<u8> {
    bat()
        .args([
            "--color=always",
            "--theme=TwoDark",
            "--style=plain",
            "--paging=never",
        ])
        .args(args)
        .write_stdin(input)
        .assert()
        .success()
        .get_output()
        .stdout
        .clone()
}

#[test]
fn vim_and_emacs_modelines_select_syntax_without_extensions() {
    for first in [
        "# -*- python -*-",
        "# -*- coding: utf-8; mode: python; -*-",
        "# vim: set ts=4 filetype=python:",
        "# vi: ft=python",
        "# ex: syntax=python",
        "\u{feff}# -*- python -*-",
    ] {
        let text = format!("{first}\nprint('hello')\n");
        assert_eq!(
            output(&[], &text),
            output(&["--language=python"], &text),
            "{first}"
        );
    }
}

#[test]
fn modelines_override_extensions_but_not_explicit_language_or_mapping() {
    let text = "# -*- python -*-\nprint('hello')\n";
    assert_eq!(
        output(&["--file-name=unknown.rs"], text),
        output(&["--language=python"], text)
    );
    assert_eq!(
        output(&["--file-name=unknown.rs", "--language=rust"], text),
        output(&["--language=rust"], text)
    );
    assert_eq!(
        output(&["--file-name=custom", "--map-syntax=custom:Rust"], text),
        output(&["--language=rust"], text)
    );
}

#[test]
fn unrecognized_modeline_falls_back_to_filename_and_shebang() {
    let rust = "// -*- not-a-language -*-\nfn main() {}\n";
    assert_eq!(
        output(&["--file-name=example.rs"], rust),
        output(&["--language=rust"], rust)
    );
    let python = "#!/usr/bin/python # -*- not-a-language -*-\nprint('hello')\n";
    assert_eq!(output(&[], python), output(&["--language=python"], python));
}

#[test]
fn first_modeline_wins_and_later_lines_are_not_scanned() {
    let text = "# -*- python -*- vim: ft=rust\nprint('hello')\n";
    assert_eq!(output(&[], text), output(&["--language=python"], text));
    let text = "no modeline here\n# -*- python -*-\nprint('hello')\n";
    assert_eq!(output(&[], text), output(&["--language=txt"], text));
}

#[test]
fn ordinary_text_does_not_become_a_modeline() {
    for text in [
        "notvim: ft=python\nprint('hello')\n",
        "# -*- python\nprint('hello')\n",
        "# mode: python\nprint('hello')\n",
    ] {
        assert_eq!(output(&[], text), output(&["--language=txt"], text));
    }
}

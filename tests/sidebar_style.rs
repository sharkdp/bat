mod utils;

use utils::command::bat;

fn output(style: &str) -> Vec<u8> {
    bat()
        .args([
            "--decorations=always",
            "--color=never",
            "--paging=never",
            "--terminal-width=30",
        ])
        .arg(format!("--style={style}"))
        .write_stdin("first\nsecond\n")
        .assert()
        .success()
        .get_output()
        .stdout
        .clone()
}

#[test]
fn sidebar_expands_to_numbers_and_changes() {
    #[cfg(feature = "git")]
    let explicit = "numbers,changes";
    #[cfg(not(feature = "git"))]
    let explicit = "numbers";
    assert_eq!(output("sidebar"), output(explicit));
    assert_eq!(output("plain,+sidebar"), output(explicit));
}

#[test]
fn removing_sidebar_keeps_headers_and_borders() {
    #[cfg(feature = "git")]
    let explicit = "full,-numbers,-changes";
    #[cfg(not(feature = "git"))]
    let explicit = "full,-numbers";
    assert_eq!(output("full,-sidebar"), output(explicit));
    assert_eq!(output("sidebar,-sidebar"), output("plain"));
}

#[test]
fn components_can_override_the_sidebar_alias() {
    assert_eq!(
        output("plain,+sidebar,-sidebar,+numbers"),
        output("numbers")
    );
    assert_eq!(output("full,-sidebar,+sidebar"), output("full"));
}

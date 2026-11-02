use std::io::Write;
use std::process::{Command, Stdio};

fn run_bat(args: &[&str], envs: &[(&str, &str)], stdin_data: &str) -> String {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_bat"));
    cmd.arg("--no-config")
        .arg("--paging=never")
        .args(args)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());
    for &(k, v) in envs {
        cmd.env(k, v);
    }
    let output = cmd
        .spawn()
        .and_then(|mut child| {
            child
                .stdin
                .as_mut()
                .unwrap()
                .write_all(stdin_data.as_bytes())?;
            child.wait_with_output()
        })
        .expect("run bat");

    assert!(
        output.status.success(),
        "bat exited with {}: {}",
        output.status,
        String::from_utf8_lossy(&output.stderr)
    );
    String::from_utf8_lossy(&output.stdout).into_owned()
}

#[test]
fn test_cli_mermaid_markdown_block() {
    let md = "# Doc\n\n```mermaid\ngraph TD\n  A --> B\n```\n\nFooter\n";
    let stdout = run_bat(
        &[
            "--mermaid=always",
            "--style=plain",
            "--language=markdown",
            "-",
        ],
        &[],
        md,
    );

    assert!(stdout.contains("A"));
    assert!(stdout.contains("B"));
    assert!(stdout.contains("Doc"));
    assert!(stdout.contains("Footer"));
    // The raw fence header should be replaced by the diagram
    assert!(!stdout.contains("```mermaid"));
    assert!(stdout.contains("┌") || stdout.contains("+"));
}

#[test]
fn test_cli_mermaid_never_mode() {
    let md = "```mermaid\ngraph TD\n  A --> B\n```\n";
    let stdout = run_bat(
        &[
            "--mermaid=never",
            "--style=plain",
            "--language=markdown",
            "-",
        ],
        &[],
        md,
    );

    // In never mode, raw fence is printed
    assert!(stdout.contains("```mermaid"));
    assert!(stdout.contains("A --> B"));
}

#[test]
fn test_cli_mermaid_env_var() {
    let md = "# Header\n\n```mermaid\ngraph TD\n  StartNode --> EndNode\n```\n";

    let stdout_always = run_bat(
        &["--style=plain", "--language=markdown", "-"],
        &[("BAT_MERMAID", "always")],
        md,
    );
    assert!(!stdout_always.contains("```mermaid"));
    assert!(stdout_always.contains("StartNode"));
    assert!(stdout_always.contains("EndNode"));

    let stdout_never = run_bat(
        &["--style=plain", "--language=markdown", "-"],
        &[("BAT_MERMAID", "never")],
        md,
    );
    assert!(stdout_never.contains("```mermaid"));
    assert!(stdout_never.contains("StartNode --> EndNode"));
}

#[test]
fn test_cli_mermaid_markdown_gutter_line_numbers_and_continuity() {
    let md = "# Doc\n\n```mermaid\ngraph TD\n  A --> B\n```\n\n## Footer\n";
    let stdout = run_bat(
        &[
            "--mermaid=always",
            "--decorations=always",
            "--style=numbers,grid",
            "--color=never",
            "--language=markdown",
            "-",
        ],
        &[],
        md,
    );

    // Line 1 is "# Doc", line 2 is blank, lines 3..=6 are the mermaid fence (replaced by blank-gutter diagram),
    // line 7 is blank, line 8 is "## Footer".
    assert!(stdout.contains("   1 │ # Doc\n"), "output:\n{stdout}");
    assert!(stdout.contains("   2 │ \n"), "output:\n{stdout}");
    assert!(stdout.contains("     │ ┌"), "output:\n{stdout}");
    assert!(stdout.contains("   7 │ \n"), "output:\n{stdout}");
    assert!(stdout.contains("   8 │ ## Footer\n"), "output:\n{stdout}");
    assert!(!stdout.contains("   3 │"));
    assert!(!stdout.contains("```mermaid"));
}

#[test]
fn test_cli_mermaid_invalid_block_fallback_and_error_marker() {
    let md = "# Doc\n\n```mermaid\nnot_a_valid_diagram\n```\n\nFooter\n";
    let stdout = run_bat(
        &[
            "--mermaid=always",
            "--decorations=always",
            "--style=numbers,grid",
            "--color=never",
            "--language=markdown",
            "-",
        ],
        &[],
        md,
    );

    assert!(stdout.contains("   1 │ # Doc\n"), "output:\n{stdout}");
    assert!(stdout.contains("!  3 │ ```mermaid\n"), "output:\n{stdout}");
    assert!(
        stdout.contains("   4 │ not_a_valid_diagram\n"),
        "output:\n{stdout}"
    );
    assert!(stdout.contains("   5 │ ```\n"), "output:\n{stdout}");
    assert!(stdout.contains("   7 │ Footer\n"), "output:\n{stdout}");
}

#[test]
fn test_cli_mermaid_standalone_mmd_file() {
    let mmd = "graph LR\n  Client --> Server\n";
    let stdout = run_bat(
        &[
            "--mermaid=always",
            "--decorations=always",
            "--style=header,grid,numbers",
            "--color=never",
            "--file-name=diagram.mmd",
            "-",
        ],
        &[],
        mmd,
    );

    assert!(stdout.contains("File: diagram.mmd"), "output:\n{stdout}");
    assert!(stdout.contains("Client"), "output:\n{stdout}");
    assert!(stdout.contains("Server"), "output:\n{stdout}");
    assert!(stdout.contains("     │ "), "output:\n{stdout}");
    assert!(!stdout.contains("Client --> Server"), "output:\n{stdout}");
}

#[test]
fn test_cli_mermaid_out_of_range_block_not_rendered() {
    let md = "# Doc\n\n```mermaid\ngraph TD\n  A --> B\n```\n\nFooter\n";
    let stdout = run_bat(
        &[
            "--mermaid=always",
            "--decorations=always",
            "--style=numbers,grid",
            "--color=never",
            "--language=markdown",
            "--line-range=8:8",
            "-",
        ],
        &[],
        md,
    );

    assert_eq!(stdout.lines().filter(|l| l.contains("│")).count(), 1);
    assert!(stdout.contains("   8 │ Footer"), "output:\n{stdout}");
    assert!(!stdout.contains("┌"));
}

#[test]
fn test_cli_mermaid_auto_mode_piped_does_not_render() {
    let md = "# Doc\n\n```mermaid\ngraph TD\n  A --> B\n```\n";
    let stdout = run_bat(&["--mermaid=auto", "--language=markdown", "-"], &[], md);

    assert!(stdout.contains("```mermaid"));
    assert!(stdout.contains("A --> B"));
}

#[test]
fn test_controller_mermaid_auto_mode_interactive() {
    use bat::assets::HighlightingAssets;
    use bat::config::{Config, MermaidMode};
    use bat::controller::Controller;
    use bat::input::Input;
    use bat::output::OutputHandle;
    use bat::style::{StyleComponent, StyleComponents};

    let assets = HighlightingAssets::from_binary();
    let config = Config {
        language: Some("markdown"),
        mermaid_mode: MermaidMode::Auto,
        loop_through: false,
        style_components: StyleComponents::new(&[
            StyleComponent::LineNumbers,
            StyleComponent::Grid,
        ]),
        colored_output: false,
        term_width: 80,
        ..Config::default()
    };

    let md = b"# Title\n\n```mermaid\ngraph TD\n  Node1 --> Node2\n```\n\nEnd\n";
    let input = Input::from_reader(Box::new(&md[..]));
    let controller = Controller::new(&config, &assets);
    let mut out = String::new();
    let mut handle = OutputHandle::FmtWrite(&mut out);
    controller.run(vec![input], Some(&mut handle)).unwrap();

    assert!(out.contains("   1 │ # Title\n"), "output:\n{out}");
    assert!(out.contains("Node1"), "output:\n{out}");
    assert!(out.contains("Node2"), "output:\n{out}");
    assert!(!out.contains("```mermaid"), "output:\n{out}");
    assert!(out.contains("   8 │ End\n"), "output:\n{out}");
}

#[test]
fn test_cli_mermaid_syntax_highlighting_state_continuity() {
    let md_mermaid = "# Doc\n\n```mermaid\ngraph TD\n  A --> B\n```\n\n## After Heading\n";
    let md_plain = "# Doc\n\n```text\ngraph TD\n  A --> B\n```\n\n## After Heading\n";

    let out_mermaid = run_bat(
        &[
            "--mermaid=always",
            "--color=always",
            "--style=plain",
            "--language=markdown",
            "-",
        ],
        &[],
        md_mermaid,
    );
    let out_plain = run_bat(
        &[
            "--mermaid=never",
            "--color=always",
            "--style=plain",
            "--language=markdown",
            "-",
        ],
        &[],
        md_plain,
    );

    let last_mermaid = out_mermaid.lines().last().expect("last line");
    let last_plain = out_plain.lines().last().expect("last line");
    assert_eq!(last_mermaid, last_plain);
}

#[test]
fn test_cli_mermaid_standalone_language_flag_and_invalid_fallback() {
    let valid_mmd = "sequenceDiagram\n  Alice->>Bob: Ping\n";
    let stdout = run_bat(
        &[
            "--mermaid=always",
            "--color=always",
            "--style=plain",
            "--language=mermaid",
            "-",
        ],
        &[],
        valid_mmd,
    );
    assert!(stdout.contains("Alice"));
    assert!(stdout.contains("Bob"));
    assert!(stdout.contains("Ping"));
    assert!(!stdout.contains("sequenceDiagram"));

    let invalid_mmd = "not_a_mermaid_diagram\nline_two\n";
    let fallback = run_bat(
        &[
            "--mermaid=always",
            "--decorations=always",
            "--style=numbers,grid",
            "--color=never",
            "--file-name=broken.mermaid",
            "-",
        ],
        &[],
        invalid_mmd,
    );
    assert!(
        fallback.contains("   1 │ not_a_mermaid_diagram\n"),
        "output:\n{fallback}"
    );
    assert!(
        fallback.contains("   2 │ line_two\n"),
        "output:\n{fallback}"
    );
}

#[test]
fn test_cli_mermaid_unclosed_fence_fallback() {
    let md = "# Doc\n\n```mermaid\ngraph TD\n  A --> B\n";
    let stdout = run_bat(
        &[
            "--mermaid=always",
            "--decorations=always",
            "--style=numbers,grid",
            "--color=never",
            "--language=markdown",
            "-",
        ],
        &[],
        md,
    );

    assert!(stdout.contains("!  3 │ ```mermaid\n"), "output:\n{stdout}");
    assert!(stdout.contains("   4 │ graph TD\n"), "output:\n{stdout}");
    assert!(stdout.contains("   5 │   A --> B\n"), "output:\n{stdout}");
}

#[test]
fn test_cli_mermaid_theme_contrast_and_spaceless_arrows() {
    let mmd = "graph TD\nA-->B\n";
    let dark_out = run_bat(
        &[
            "--mermaid=always",
            "--color=always",
            "--theme=Monokai Extended",
            "--style=plain",
            "--language=mermaid",
            "-",
        ],
        &[("COLORTERM", "truecolor")],
        mmd,
    );
    // Monokai Extended foreground is #f8f8f2 (248, 248, 242), not #27272a (39, 39, 42)
    assert!(
        dark_out.contains("\x1b[38;2;248;248;242mA\x1b[0m"),
        "dark_out:\n{dark_out:?}"
    );
    assert!(
        dark_out.contains("\x1b[38;2;248;248;242mB\x1b[0m"),
        "dark_out:\n{dark_out:?}"
    );

    // Non-ASCII state diagram with malformed arrow must not panic
    let non_ascii_state = "stateDiagram-v2\nxé->\n";
    let fallback_out = run_bat(
        &[
            "--mermaid=always",
            "--color=never",
            "--style=plain",
            "--language=mermaid",
            "-",
        ],
        &[],
        non_ascii_state,
    );
    assert!(fallback_out.contains("stateDiagram-v2"));
    assert!(fallback_out.contains("xé->"));
}

#[test]
fn test_cli_mermaid_unwrapped_diagram_with_wrapped_markdown() {
    let md = "# Title\n\n```mermaid\ngraph LR\n  LongNodeA1234567890 --> LongNodeB1234567890\n```\n\nA very long markdown text line that should wrap within narrow terminal width.\n";
    let stdout = run_bat(
        &[
            "--mermaid=always",
            "--style=plain",
            "--wrap=character",
            "--terminal-width=40",
            "--language=markdown",
            "-",
        ],
        &[],
        md,
    );
    assert!(stdout.contains("LongNodeA1234567890"));
    assert!(stdout.contains("LongNodeB1234567890"));
    assert!(stdout.contains("A very long markdown text line that shou"));
}

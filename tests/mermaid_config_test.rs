use bat::config::{Config, MermaidMode};

#[test]
fn test_mermaid_mode_values() {
    assert_eq!(MermaidMode::default(), MermaidMode::Auto);
    assert_ne!(MermaidMode::Auto, MermaidMode::Always);
    assert_ne!(MermaidMode::Auto, MermaidMode::Never);
    assert_ne!(MermaidMode::Always, MermaidMode::Never);
}

#[test]
fn test_config_default_mermaid_mode() {
    let config = Config::default();
    assert_eq!(config.mermaid_mode, MermaidMode::Auto);
}

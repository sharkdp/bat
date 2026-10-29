use bat::assets::HighlightingAssets;
use bat::config::{Config, MermaidMode};
use bat::controller::Controller;
use bat::input::Input;

#[test]
fn test_diagram_rendering_configuration_exists() {
    let config = Config::default();
    assert_eq!(config.mermaid_mode, MermaidMode::Auto);
    let assets = HighlightingAssets::from_binary();
    let _controller = Controller::new(&config, &assets);
    let _input = Input::from_reader(Box::new(&b"graph TD\n  A --> B\n"[..]));
}

#[cfg(test)]
mod security_tests {
    use std::fs;
    use std::path::Path;
    use yaml_rust::{YamlLoader, Yaml};

    #[test]
    fn test_dependabot_config_has_cooldown_period() {
        // Invariant: All package-ecosystem entries in Dependabot config must have cooldown with default-days >= 7
        let config_path = Path::new(".github/dependabot.yml");
        
        // Adversarial payloads: configurations that violate the security property
        let payloads = vec![
            // Exact exploit case: missing cooldown entirely
            r#"updates:
  - package-ecosystem: "cargo"
    directory: "/"
    schedule:
      interval: "daily""#,
            
            // Boundary case: cooldown exists but with insufficient days (0)
            r#"updates:
  - package-ecosystem: "cargo"
    directory: "/"
    schedule:
      interval: "daily"
    cooldown:
      default-days: 0"#,
            
            // Valid input: cooldown with sufficient days (7)
            r#"updates:
  - package-ecosystem: "cargo"
    directory: "/"
    schedule:
      interval: "daily"
    cooldown:
      default-days: 7"#,
        ];

        // First, verify the actual production file exists and is valid YAML
        let config_content = fs::read_to_string(config_path)
            .expect("Dependabot config file should exist");
        
        let docs = YamlLoader::load_from_str(&config_content)
            .expect("Dependabot config should be valid YAML");
        let config = &docs[0];

        // Extract updates section
        if let Some(updates) = config["updates"].as_vec() {
            for update in updates {
                if let Some(package_ecosystem) = update["package-ecosystem"].as_str() {
                    // Security property: must have cooldown with default-days >= 7
                    let cooldown = &update["cooldown"];
                    let default_days = cooldown["default-days"].as_i64();
                    
                    assert!(
                        default_days.is_some() && default_days.unwrap() >= 7,
                        "Package ecosystem '{}' must have cooldown with default-days >= 7 days",
                        package_ecosystem
                    );
                }
            }
        }
    }
}
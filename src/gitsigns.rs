pub mod env {
    pub const BAT_GITSIGNS: &str = "BAT_GITSIGNS";
}

#[cfg(feature = "git")]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Gitsigns {
    pub modified: String,
    pub added: String,
    pub removed_above: String,
    pub removed_below: String,
}

#[cfg(feature = "git")]
impl Default for Gitsigns {
    fn default() -> Self {
        Gitsigns::classic()
    }
}

#[cfg(feature = "git")]
impl Gitsigns {
    pub fn classic() -> Self {
        Self {
            modified: "~".into(),
            added: "+".into(),
            removed_above: "‾".into(),
            removed_below: "_".into(),
        }
    }

    pub fn modern() -> Self {
        Self {
            modified: "▎".to_string(),
            added: "▎".to_string(),
            removed_above: "‾".to_string(),
            removed_below: "_".to_string(),
        }
    }

    pub fn parse(s: &str) -> Result<Self, String> {
        let parts: Vec<&str> = s
            .split(',')
            .map(|p| {
                // allow single space char as gitsign
                if p == " " {
                    return p;
                }

                p.trim()
            })
            .collect();

        if parts.len() != 4 {
            return Err("Expected 4 comma-separated signs: `modified,added,removed-above,removed-below`, e.g. `~,+,‾,_`".into());
        }

        for (i, part) in parts.iter().enumerate() {
            if part.chars().count() != 1 {
                return Err(format!(
                    "Invalid sign at position {}: `{}`. Each sign must be a single character.",
                    i + 1,
                    part
                ));
            }
        }

        Ok(Self {
            modified: parts[0].to_string(),
            added: parts[1].to_string(),
            removed_above: parts[2].to_string(),
            removed_below: parts[3].to_string(),
        })
    }
}

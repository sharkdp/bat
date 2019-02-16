use std::collections::HashSet;
use std::str::FromStr;

use errors::*;

#[derive(Debug, Eq, PartialEq, Copy, Clone, Hash)]
pub enum OutputComponent {
    Auto,
    Changes,
    Grid,
    Header,
    Numbers,
    Full,
    Plain,
}

#[derive(Debug, Eq, PartialEq, Copy, Clone, Hash)]
pub enum OutputWrap {
    Character,
    None,
}

impl OutputComponent {
    pub fn components(&self, interactive_terminal: bool) -> &'static [OutputComponent] {
        match *self {
            Self::Auto => {
                if interactive_terminal {
                    Self::Full.components(interactive_terminal)
                } else {
                    Self::Plain.components(interactive_terminal)
                }
            }
            Self::Changes => &[Self::Changes],
            Self::Grid => &[Self::Grid],
            Self::Header => &[Self::Header],
            Self::Numbers => &[Self::Numbers],
            Self::Full => &[
                Self::Changes,
                Self::Grid,
                Self::Header,
                Self::Numbers,
            ],
            Self::Plain => &[],
        }
    }
}

impl FromStr for OutputComponent {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self> {
        match s {
            "auto" => Ok(Self::Auto),
            "changes" => Ok(Self::Changes),
            "grid" => Ok(Self::Grid),
            "header" => Ok(Self::Header),
            "numbers" => Ok(Self::Numbers),
            "full" => Ok(Self::Full),
            "plain" => Ok(Self::Plain),
            _ => Err(format!("Unknown style '{}'", s).into()),
        }
    }
}

#[derive(Clone)]
pub struct OutputComponents(pub HashSet<OutputComponent>);

impl OutputComponents {
    pub fn changes(&self) -> bool {
        self.0.contains(&OutputComponent::Changes)
    }

    pub fn grid(&self) -> bool {
        self.0.contains(&OutputComponent::Grid)
    }

    pub fn header(&self) -> bool {
        self.0.contains(&OutputComponent::Header)
    }

    pub fn numbers(&self) -> bool {
        self.0.contains(&OutputComponent::Numbers)
    }

    pub fn plain(&self) -> bool {
        self.0.iter().all(|c| c == &OutputComponent::Plain)
    }
}

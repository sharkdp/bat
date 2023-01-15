use std::collections::HashSet;
use std::str::FromStr;

use crate::error::*;

#[non_exhaustive]
#[derive(Debug, Eq, PartialEq, Copy, Clone, Hash)]
pub enum StyleComponent {
    Auto,
    #[cfg(feature = "git")]
    Changes,
    Grid,
    Rule,
    Header,
    HeaderFilename,
    HeaderFilesize,
    LineNumbers,
    Snip,
    Full,
    Default,
    Plain,
}

impl StyleComponent {
    pub fn components(self, interactive_terminal: bool) -> &'static [StyleComponent] {
        match self {
            StyleComponent::Auto => {
                if interactive_terminal {
                    StyleComponent::Default.components(interactive_terminal)
                } else {
                    StyleComponent::Plain.components(interactive_terminal)
                }
            }
            #[cfg(feature = "git")]
            StyleComponent::Changes => &[StyleComponent::Changes],
            StyleComponent::Grid => &[StyleComponent::Grid],
            StyleComponent::Rule => &[StyleComponent::Rule],
            StyleComponent::Header => &[StyleComponent::HeaderFilename],
            StyleComponent::HeaderFilename => &[StyleComponent::HeaderFilename],
            StyleComponent::HeaderFilesize => &[StyleComponent::HeaderFilesize],
            StyleComponent::LineNumbers => &[StyleComponent::LineNumbers],
            StyleComponent::Snip => &[StyleComponent::Snip],
            StyleComponent::Full => &[
                #[cfg(feature = "git")]
                StyleComponent::Changes,
                StyleComponent::Grid,
                StyleComponent::HeaderFilename,
                StyleComponent::HeaderFilesize,
                StyleComponent::LineNumbers,
                StyleComponent::Snip,
            ],
            StyleComponent::Default => &[
                #[cfg(feature = "git")]
                StyleComponent::Changes,
                StyleComponent::Grid,
                StyleComponent::HeaderFilename,
                StyleComponent::LineNumbers,
                StyleComponent::Snip,
            ],
            StyleComponent::Plain => &[],
        }
    }
}

impl FromStr for StyleComponent {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self> {
        match s {
            "auto" => Ok(StyleComponent::Auto),
            #[cfg(feature = "git")]
            "changes" => Ok(StyleComponent::Changes),
            "grid" => Ok(StyleComponent::Grid),
            "rule" => Ok(StyleComponent::Rule),
            "header" => Ok(StyleComponent::Header),
            "header-filename" => Ok(StyleComponent::HeaderFilename),
            "header-filesize" => Ok(StyleComponent::HeaderFilesize),
            "numbers" => Ok(StyleComponent::LineNumbers),
            "snip" => Ok(StyleComponent::Snip),
            "full" => Ok(StyleComponent::Full),
            "default" => Ok(StyleComponent::Default),
            "plain" => Ok(StyleComponent::Plain),
            _ => Err(format!("Unknown style '{}'", s).into()),
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct StyleComponents(pub HashSet<StyleComponent>);

impl StyleComponents {
    pub fn new(components: &[StyleComponent]) -> StyleComponents {
        StyleComponents(components.iter().cloned().collect())
    }

    #[cfg(feature = "git")]
    pub fn changes(&self) -> bool {
        self.0.contains(&StyleComponent::Changes)
    }

    pub fn grid(&self) -> bool {
        self.0.contains(&StyleComponent::Grid)
    }

    pub fn rule(&self) -> bool {
        self.0.contains(&StyleComponent::Rule)
    }

    pub fn header(&self) -> bool {
        self.header_filename() || self.header_filesize()
    }

    pub fn header_filename(&self) -> bool {
        self.0.contains(&StyleComponent::HeaderFilename)
    }

    pub fn header_filesize(&self) -> bool {
        self.0.contains(&StyleComponent::HeaderFilesize)
    }

    pub fn numbers(&self) -> bool {
        self.0.contains(&StyleComponent::LineNumbers)
    }

    pub fn snip(&self) -> bool {
        self.0.contains(&StyleComponent::Snip)
    }

    pub fn plain(&self) -> bool {
        self.0.iter().all(|c| c == &StyleComponent::Plain)
    }

    pub fn insert(&mut self, component: StyleComponent) {
        self.0.insert(component);
    }

    pub fn clear(&mut self) {
        self.0.clear();
    }
}

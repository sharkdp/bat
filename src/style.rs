use std::collections::HashSet;
use std::str::FromStr;

use crate::error::*;

#[derive(Debug, Eq, PartialEq, Copy, Clone, Hash)]
pub enum StyleComponent {
    Auto,
    #[cfg(feature = "git")]
    Changes,
    Grid,
    Rule,
    Header,
    HeaderFull,
    HeaderFilename,
    HeaderFilesize,
    HeaderPermissions,
    HeaderLastModified,
    LineNumbers,
    Snip,
    Full,
    Plain,
}

impl StyleComponent {
    pub fn components(self, interactive_terminal: bool) -> &'static [StyleComponent] {
        match self {
            StyleComponent::Auto => {
                if interactive_terminal {
                    &[
                        #[cfg(feature = "git")]
                        StyleComponent::Changes,
                        StyleComponent::Grid,
                        StyleComponent::HeaderFilename,
                        StyleComponent::HeaderFilesize,
                        StyleComponent::LineNumbers,
                        StyleComponent::Snip,
                    ]
                } else {
                    StyleComponent::Plain.components(interactive_terminal)
                }
            }
            #[cfg(feature = "git")]
            StyleComponent::Changes => &[StyleComponent::Changes],
            StyleComponent::Grid => &[StyleComponent::Grid],
            StyleComponent::Rule => &[StyleComponent::Rule],
            StyleComponent::Header => &[
                StyleComponent::HeaderFilename,
                StyleComponent::HeaderFilesize,
            ],
            StyleComponent::HeaderFull => &[
                StyleComponent::HeaderFilename,
                StyleComponent::HeaderFilesize,
                StyleComponent::HeaderPermissions,
                StyleComponent::HeaderLastModified,
            ],
            StyleComponent::HeaderFilename => &[StyleComponent::HeaderFilename],
            StyleComponent::HeaderFilesize => &[StyleComponent::HeaderFilesize],
            StyleComponent::HeaderPermissions => &[StyleComponent::HeaderPermissions],
            StyleComponent::HeaderLastModified => &[StyleComponent::HeaderLastModified],
            StyleComponent::LineNumbers => &[StyleComponent::LineNumbers],
            StyleComponent::Snip => &[StyleComponent::Snip],
            StyleComponent::Full => &[
                #[cfg(feature = "git")]
                StyleComponent::Changes,
                StyleComponent::Grid,
                StyleComponent::HeaderFilename,
                StyleComponent::HeaderFilesize,
                StyleComponent::HeaderPermissions,
                StyleComponent::HeaderLastModified,
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
            "header-full" => Ok(StyleComponent::HeaderFull),
            "header-filename" => Ok(StyleComponent::HeaderFilename),
            "header-filesize" => Ok(StyleComponent::HeaderFilesize),
            "header-permissions" => Ok(StyleComponent::HeaderPermissions),
            "header-lastmodified" => Ok(StyleComponent::HeaderLastModified),
            "numbers" => Ok(StyleComponent::LineNumbers),
            "snip" => Ok(StyleComponent::Snip),
            "full" => Ok(StyleComponent::Full),
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
        self.header_filename()
            || self.header_filesize()
            || self.header_permissions()
            || self.header_last_modified()
    }

    pub fn header_filename(&self) -> bool {
        self.0.contains(&StyleComponent::HeaderFilename)
    }

    pub fn header_filesize(&self) -> bool {
        self.0.contains(&StyleComponent::HeaderFilesize)
    }

    pub fn header_permissions(&self) -> bool {
        self.0.contains(&StyleComponent::HeaderPermissions)
    }

    pub fn header_last_modified(&self) -> bool {
        self.0.contains(&StyleComponent::HeaderLastModified)
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
}

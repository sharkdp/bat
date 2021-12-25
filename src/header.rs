use std::collections::HashSet;
use std::fmt;
use std::str::FromStr;

use crate::error::*;

#[derive(Debug, Eq, PartialEq, Copy, Clone, Hash)]
pub enum HeaderComponent {
    Filename,
    Size,
    Permissions,
    LastModified,
    Full,
}

impl fmt::Display for HeaderComponent {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // Write strictly the first element into the supplied output
        // stream: `f`. Returns `fmt::Result` which indicates whether the
        // operation succeeded or failed. Note that `write!` uses syntax which
        // is very similar to `println!`.
        let name = match self {
            HeaderComponent::Filename => "filename",
            HeaderComponent::Size => "size",
            HeaderComponent::Permissions => "permissions",
            HeaderComponent::LastModified => "last-modified",
            HeaderComponent::Full => "full",
        };

        write!(f, "{}", name)
    }
}

impl HeaderComponent {
    pub fn components(self) -> &'static [HeaderComponent] {
        match self {
            HeaderComponent::Filename => &[HeaderComponent::Filename],
            HeaderComponent::Size => &[HeaderComponent::Size],
            HeaderComponent::Permissions => &[HeaderComponent::Permissions],
            HeaderComponent::LastModified => &[HeaderComponent::LastModified],
            HeaderComponent::Full => &[
                HeaderComponent::Filename,
                HeaderComponent::Size,
                HeaderComponent::Permissions,
                HeaderComponent::LastModified,
            ],
        }
    }
}

impl FromStr for HeaderComponent {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self> {
        match s {
            "filename" => Ok(HeaderComponent::Filename),
            "size" => Ok(HeaderComponent::Size),
            "permissions" => Ok(HeaderComponent::Permissions),
            "last-modified" => Ok(HeaderComponent::LastModified),
            "full" => Ok(HeaderComponent::Full),
            _ => Err(format!("Unknown header-info '{}'", s).into()),
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct HeaderComponents(pub HashSet<HeaderComponent>);

impl HeaderComponents {
    pub fn new(components: &[HeaderComponent]) -> HeaderComponents {
        HeaderComponents(components.iter().cloned().collect())
    }

    pub fn filename(&self) -> bool {
        self.0.contains(&HeaderComponent::Filename)
    }

    pub fn size(&self) -> bool {
        self.0.contains(&HeaderComponent::Size)
    }

    pub fn permissions(&self) -> bool {
        self.0.contains(&HeaderComponent::Permissions)
    }

    pub fn last_modified(&self) -> bool {
        self.0.contains(&HeaderComponent::LastModified)
    }
}

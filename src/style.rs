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
            _ => Err(format!("Unknown style '{s}'").into()),
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

#[derive(Debug, PartialEq)]
enum ComponentAction {
    Override,
    Add,
    Remove,
}

impl ComponentAction {
    fn extract_from_str(string: &str) -> (ComponentAction, &str) {
        match string.chars().next() {
            Some('-') => (ComponentAction::Remove, string.strip_prefix('-').unwrap()),
            Some('+') => (ComponentAction::Add, string.strip_prefix('+').unwrap()),
            _ => (ComponentAction::Override, string),
        }
    }
}

/// A list of [StyleComponent] that can be parsed from a string.
pub struct StyleComponentList(Vec<(ComponentAction, StyleComponent)>);

impl StyleComponentList {
    fn expand_into(&self, components: &mut HashSet<StyleComponent>, interactive_terminal: bool) {
        for (action, component) in self.0.iter() {
            let subcomponents = component.components(interactive_terminal);

            use ComponentAction::*;
            match action {
                Override | Add => components.extend(subcomponents),
                Remove => components.retain(|c| !subcomponents.contains(c)),
            }
        }
    }

    /// Returns `true` if any component in the list was not prefixed with `+` or `-`.
    fn contains_override(&self) -> bool {
        self.0.iter().any(|(a, _)| *a == ComponentAction::Override)
    }

    /// Combines multiple [StyleComponentList]s into a single [StyleComponents] set.
    ///
    /// ## Precedence
    /// The most recent list will take precedence and override all previous lists
    /// unless it only contains components prefixed with `-` or `+`. When this
    /// happens, the list's components will be merged into the previous list.
    ///
    /// ## Example
    /// ```text
    /// [numbers,grid] + [header,changes]  -> [header,changes]
    /// [numbers,grid] + [+header,-grid]   -> [numbers,header]
    /// ```
    ///
    /// ## Parameters
    ///  - `with_default`: If true, the styles lists will build upon the StyleComponent::Auto style.
    pub fn to_components(
        lists: impl IntoIterator<Item = StyleComponentList>,
        interactive_terminal: bool,
        with_default: bool,
    ) -> StyleComponents {
        let mut components: HashSet<StyleComponent> = HashSet::new();
        if with_default {
            components.extend(StyleComponent::Auto.components(interactive_terminal))
        }

        StyleComponents(lists.into_iter().fold(components, |mut components, list| {
            if list.contains_override() {
                components.clear();
            }

            list.expand_into(&mut components, interactive_terminal);
            components
        }))
    }
}

impl Default for StyleComponentList {
    fn default() -> Self {
        StyleComponentList(vec![(ComponentAction::Override, StyleComponent::Default)])
    }
}

impl FromStr for StyleComponentList {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self> {
        Ok(StyleComponentList(
            s.split(",")
                .map(ComponentAction::extract_from_str) // If the component starts with "-", it's meant to be removed
                .map(|(a, s)| Ok((a, StyleComponent::from_str(s)?)))
                .collect::<Result<Vec<(ComponentAction, StyleComponent)>>>()?,
        ))
    }
}

#[cfg(test)]
mod test {
    use std::collections::HashSet;
    use std::str::FromStr;

    use super::ComponentAction::*;
    use super::StyleComponent;
    use super::StyleComponent::*;
    use super::StyleComponentList;

    #[test]
    pub fn style_component_list_parse() {
        assert_eq!(
            StyleComponentList::from_str("grid,+numbers,snip,-snip,header")
                .expect("no error")
                .0,
            vec![
                (Override, Grid),
                (Add, LineNumbers),
                (Override, Snip),
                (Remove, Snip),
                (Override, Header),
            ]
        );

        assert!(StyleComponentList::from_str("not-a-component").is_err());
        assert!(StyleComponentList::from_str("grid,not-a-component").is_err());
        assert!(StyleComponentList::from_str("numbers,-not-a-component").is_err());
    }

    #[test]
    pub fn style_component_list_to_components() {
        assert_eq!(
            StyleComponentList::to_components(
                vec![StyleComponentList::from_str("grid,numbers").expect("no error")],
                false,
                false
            )
            .0,
            HashSet::from([Grid, LineNumbers])
        );
    }

    #[test]
    pub fn style_component_list_to_components_removes_negated() {
        assert_eq!(
            StyleComponentList::to_components(
                vec![StyleComponentList::from_str("grid,numbers,-grid").expect("no error")],
                false,
                false
            )
            .0,
            HashSet::from([LineNumbers])
        );
    }

    #[test]
    pub fn style_component_list_to_components_expands_subcomponents() {
        assert_eq!(
            StyleComponentList::to_components(
                vec![StyleComponentList::from_str("full").expect("no error")],
                false,
                false
            )
            .0,
            HashSet::from_iter(Full.components(true).to_owned())
        );
    }

    #[test]
    pub fn style_component_list_expand_negates_subcomponents() {
        assert!(!StyleComponentList::to_components(
            vec![StyleComponentList::from_str("full,-numbers").expect("no error")],
            true,
            false
        )
        .numbers());
    }

    #[test]
    pub fn style_component_list_to_components_precedence_overrides_previous_lists() {
        assert_eq!(
            StyleComponentList::to_components(
                vec![
                    StyleComponentList::from_str("grid").expect("no error"),
                    StyleComponentList::from_str("numbers").expect("no error"),
                ],
                false,
                false
            )
            .0,
            HashSet::from([LineNumbers])
        );
    }

    #[test]
    pub fn style_component_list_to_components_precedence_merges_previous_lists() {
        assert_eq!(
            StyleComponentList::to_components(
                vec![
                    StyleComponentList::from_str("grid,header").expect("no error"),
                    StyleComponentList::from_str("-grid").expect("no error"),
                    StyleComponentList::from_str("+numbers").expect("no error"),
                ],
                false,
                false
            )
            .0,
            HashSet::from([HeaderFilename, LineNumbers])
        );
    }

    #[test]
    pub fn style_component_list_default_builds_on_auto() {
        assert_eq!(
            StyleComponentList::to_components(
                vec![StyleComponentList::from_str("-numbers").expect("no error"),],
                true,
                true
            )
            .0,
            {
                let mut expected: HashSet<StyleComponent> = HashSet::new();
                expected.extend(Auto.components(true));
                expected.remove(&LineNumbers);
                expected
            }
        );
    }
}

use std::borrow::Cow;

use syntect::highlighting::{
    Color, ScopeSelector, ScopeSelectors, StyleModifier, Theme, ThemeItem,
};
use syntect::parsing::{Scope, ScopeStack};

use crate::error::*;

/// A theme color that `bat` actually uses when rendering output.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ThemeSetting {
    Foreground,
    GutterForeground,
    LineHighlight,
}

impl ThemeSetting {
    fn from_name(name: &str) -> Option<Self> {
        match name {
            "foreground" => Some(ThemeSetting::Foreground),
            "gutter-foreground" => Some(ThemeSetting::GutterForeground),
            "line-highlight" => Some(ThemeSetting::LineHighlight),
            _ => None,
        }
    }

    fn apply(self, theme: &mut Theme, color: Color) {
        match self {
            ThemeSetting::Foreground => theme.settings.foreground = Some(color),
            ThemeSetting::GutterForeground => theme.settings.gutter_foreground = Some(color),
            ThemeSetting::LineHighlight => theme.settings.line_highlight = Some(color),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Target {
    Setting(ThemeSetting),
    Scope(Scope),
}

/// Color overrides that are applied on top of the selected theme.
#[derive(Debug, Clone, Default)]
pub struct ColorOverrides {
    overrides: Vec<(Target, Color)>,
}

impl ColorOverrides {
    pub fn is_empty(&self) -> bool {
        self.overrides.is_empty()
    }

    /// Add the overrides of a single `--colors` specification, e.g.
    /// `gutter-foreground=#f5f5f5,scope:comment=#808080`.
    ///
    /// Later overrides take precedence over earlier ones.
    pub fn add(&mut self, spec: &str) -> Result<()> {
        for entry in spec.split(',') {
            let entry = entry.trim();
            if entry.is_empty() {
                continue;
            }

            let (key, value) = entry
                .split_once('=')
                .ok_or_else(|| Error::InvalidColorOverride(entry.to_owned()))?;

            let target = parse_target(key.trim())?;
            let color = parse_color(value.trim())?;

            self.overrides.push((target, color));
        }

        Ok(())
    }

    /// Apply the overrides to `theme`, leaving it untouched if there are none.
    pub(crate) fn apply<'a>(&self, theme: &'a Theme) -> Cow<'a, Theme> {
        if self.is_empty() {
            return Cow::Borrowed(theme);
        }

        let mut theme = theme.clone();
        for &(target, color) in &self.overrides {
            match target {
                Target::Setting(setting) => setting.apply(&mut theme, color),
                Target::Scope(scope) => override_scope(&mut theme, scope, color),
            }
        }

        Cow::Owned(theme)
    }
}

fn parse_target(key: &str) -> Result<Target> {
    if let Some(scope) = key.strip_prefix("scope:") {
        let scope = scope.trim();
        if scope.is_empty() {
            return Err(Error::UnknownColorOverrideTarget(key.to_owned()));
        }

        return Scope::new(scope)
            .map(Target::Scope)
            .map_err(|_| Error::UnknownColorOverrideTarget(key.to_owned()));
    }

    ThemeSetting::from_name(key)
        .map(Target::Setting)
        .ok_or_else(|| Error::UnknownColorOverrideTarget(key.to_owned()))
}

fn parse_color(value: &str) -> Result<Color> {
    let digits = value.strip_prefix('#').unwrap_or(value);
    if !digits.chars().all(|c| c.is_ascii_hexdigit()) {
        return Err(Error::InvalidColorValue(value.to_owned()));
    }

    // Both the short (#rgb) and the long (#rrggbb) CSS notation are accepted.
    let component = |i: usize| match digits.len() {
        3 => u8::from_str_radix(&digits[i..i + 1].repeat(2), 16).ok(),
        6 => u8::from_str_radix(&digits[2 * i..2 * i + 2], 16).ok(),
        _ => None,
    };

    match (component(0), component(1), component(2)) {
        (Some(r), Some(g), Some(b)) => Ok(Color { r, g, b, a: 0xFF }),
        _ => Err(Error::InvalidColorValue(value.to_owned())),
    }
}

/// Recolor every token that `theme` styles under `scope`.
///
/// Adding a single rule is not enough: syntect resolves a token's style by
/// picking the most specific matching rule, so a rule for `comment` would still
/// lose against a rule for `comment.line.double-slash` that the theme already
/// defines. The existing rules are rewritten so that the override applies to
/// the nested scopes as well.
fn override_scope(theme: &mut Theme, scope: Scope, color: Color) {
    let mut scopes = Vec::with_capacity(theme.scopes.len() + 1);

    for item in theme.scopes.drain(..) {
        let (overridden, kept): (Vec<ScopeSelector>, Vec<ScopeSelector>) = item
            .scope
            .selectors
            .into_iter()
            .partition(|selector| selector_is_below(selector, scope));

        if !kept.is_empty() {
            scopes.push(ThemeItem {
                scope: ScopeSelectors { selectors: kept },
                style: item.style,
            });
        }

        if !overridden.is_empty() {
            scopes.push(ThemeItem {
                scope: ScopeSelectors {
                    selectors: overridden,
                },
                style: StyleModifier {
                    foreground: Some(color),
                    ..item.style
                },
            });
        }
    }

    // Tokens which the theme does not style at all are covered by a new rule.
    scopes.push(ThemeItem {
        scope: ScopeSelectors {
            selectors: vec![ScopeSelector {
                path: ScopeStack::from_vec(vec![scope]),
                excludes: Vec::new(),
            }],
        },
        style: StyleModifier {
            foreground: Some(color),
            background: None,
            font_style: None,
        },
    });

    theme.scopes = scopes;
}

fn selector_is_below(selector: &ScopeSelector, scope: Scope) -> bool {
    selector
        .path
        .scopes
        .iter()
        .any(|&selector_scope| scope.is_prefix_of(selector_scope))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn color(r: u8, g: u8, b: u8) -> Color {
        Color { r, g, b, a: 0xFF }
    }

    fn overrides(spec: &str) -> ColorOverrides {
        let mut overrides = ColorOverrides::default();
        overrides.add(spec).expect("spec should be valid");
        overrides
    }

    #[test]
    fn default_overrides_are_empty() {
        assert!(ColorOverrides::default().is_empty());
    }

    #[test]
    fn long_and_short_notation_are_equivalent() {
        assert_eq!(parse_color("#ff0000").unwrap(), color(0xFF, 0x00, 0x00));
        assert_eq!(parse_color("#f00").unwrap(), color(0xFF, 0x00, 0x00));
        assert_eq!(parse_color("f5f5f5").unwrap(), color(0xF5, 0xF5, 0xF5));
    }

    #[test]
    fn invalid_colors_are_rejected() {
        for value in ["", "#", "#12345", "#gg0000", "red", "#ff00000"] {
            assert!(parse_color(value).is_err(), "{value} should be rejected");
        }
    }

    #[test]
    fn unknown_targets_are_rejected() {
        for spec in ["gutter-forground=#fff", "=#fff", "scope:=#fff", "#fff"] {
            assert!(
                ColorOverrides::default().add(spec).is_err(),
                "{spec} should be rejected"
            );
        }
    }

    #[test]
    fn settings_are_overridden() {
        let theme = overrides("foreground=#010203,gutter-foreground=#f5f5f5")
            .apply(&Theme::default())
            .into_owned();

        assert_eq!(theme.settings.foreground, Some(color(0x01, 0x02, 0x03)));
        assert_eq!(
            theme.settings.gutter_foreground,
            Some(color(0xF5, 0xF5, 0xF5))
        );
    }

    #[test]
    fn last_override_of_a_target_wins() {
        let theme = overrides("gutter-foreground=#111111,gutter-foreground=#222222")
            .apply(&Theme::default())
            .into_owned();

        assert_eq!(
            theme.settings.gutter_foreground,
            Some(color(0x22, 0x22, 0x22))
        );
    }

    #[test]
    fn theme_without_overrides_is_not_cloned() {
        let theme = Theme::default();
        assert!(matches!(
            ColorOverrides::default().apply(&theme),
            Cow::Borrowed(_)
        ));
    }

    fn item(selector: &str, foreground: Color) -> ThemeItem {
        ThemeItem {
            scope: selector.parse().unwrap(),
            style: StyleModifier {
                foreground: Some(foreground),
                background: None,
                font_style: None,
            },
        }
    }

    fn foreground_of(theme: &Theme, selector: &str) -> Vec<Option<Color>> {
        theme
            .scopes
            .iter()
            .filter(|item| {
                item.scope.selectors.iter().any(|s| {
                    s.path.scopes
                        == ScopeStack::from_vec(vec![Scope::new(selector).unwrap()]).scopes
                })
            })
            .map(|item| item.style.foreground)
            .collect()
    }

    fn theme_with(scopes: Vec<ThemeItem>) -> Theme {
        Theme {
            scopes,
            ..Default::default()
        }
    }

    #[test]
    fn nested_scopes_are_recolored() {
        let theme = theme_with(vec![
            item("comment", color(0x11, 0x11, 0x11)),
            item("comment.line.double-slash", color(0x22, 0x22, 0x22)),
            item("keyword", color(0x33, 0x33, 0x33)),
        ]);

        let theme = overrides("scope:comment=#808080")
            .apply(&theme)
            .into_owned();

        let grey = Some(color(0x80, 0x80, 0x80));
        assert_eq!(foreground_of(&theme, "comment"), vec![grey, grey]);
        assert_eq!(
            foreground_of(&theme, "comment.line.double-slash"),
            vec![grey]
        );
        assert_eq!(
            foreground_of(&theme, "keyword"),
            vec![Some(color(0x33, 0x33, 0x33))]
        );
    }

    #[test]
    fn unrelated_selectors_of_a_rule_keep_their_color() {
        let theme = theme_with(vec![item("comment, keyword", color(0x11, 0x11, 0x11))]);

        let theme = overrides("scope:comment=#808080")
            .apply(&theme)
            .into_owned();

        assert_eq!(
            foreground_of(&theme, "keyword"),
            vec![Some(color(0x11, 0x11, 0x11))]
        );
        assert_eq!(
            foreground_of(&theme, "comment"),
            vec![Some(color(0x80, 0x80, 0x80)); 2]
        );
    }

    #[test]
    fn overriding_a_scope_keeps_the_font_style() {
        let theme = theme_with(vec![ThemeItem {
            scope: "comment".parse().unwrap(),
            style: StyleModifier {
                foreground: Some(color(0x11, 0x11, 0x11)),
                background: Some(color(0x22, 0x22, 0x22)),
                font_style: Some(syntect::highlighting::FontStyle::ITALIC),
            },
        }]);

        let theme = overrides("scope:comment=#808080")
            .apply(&theme)
            .into_owned();
        let overridden = &theme.scopes[0];

        assert_eq!(overridden.style.foreground, Some(color(0x80, 0x80, 0x80)));
        assert_eq!(overridden.style.background, Some(color(0x22, 0x22, 0x22)));
        assert_eq!(
            overridden.style.font_style,
            Some(syntect::highlighting::FontStyle::ITALIC)
        );
    }
}

//! Utilities for choosing an appropriate theme for syntax highlighting.

use std::convert::Infallible;
use std::fmt;
use std::io::IsTerminal as _;
use std::str::FromStr;

/// Environment variable names.
pub mod env {
    /// See [`crate::theme::ThemeOptions::theme`].
    pub const BAT_THEME: &str = "BAT_THEME";
    /// See [`crate::theme::ThemeOptions::theme_dark`].
    pub const BAT_THEME_DARK: &str = "BAT_THEME_DARK";
    /// See [`crate::theme::ThemeOptions::theme_light`].
    pub const BAT_THEME_LIGHT: &str = "BAT_THEME_LIGHT";
}

/// Chooses an appropriate theme or falls back to a default theme
/// based on the user-provided options and the color scheme of the terminal.
///
/// Intentionally returns a [`ThemeResult`] instead of a simple string so
/// that downstream consumers such as `delta` can easily apply their own
/// default theme and can use the detected color scheme elsewhere.
pub fn theme(options: ThemeOptions) -> ThemeResult {
    theme_impl(options, &TerminalColorSchemeDetector)
}

/// The default theme, suitable for the given color scheme.
/// Use [`theme`] if you want to automatically detect the color scheme from the terminal.
pub const fn default_theme(color_scheme: ColorScheme) -> &'static str {
    match color_scheme {
        ColorScheme::Dark => "Monokai Extended",
        ColorScheme::Light => "Monokai Extended Light",
    }
}

/// Detects the color scheme from the terminal.
pub fn color_scheme(when: DetectColorScheme) -> Option<ColorScheme> {
    color_scheme_impl(when, &TerminalColorSchemeDetector)
}

/// Options for configuring the theme used for syntax highlighting.
/// Used together with [`theme`].
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ThemeOptions {
    /// Configures how the theme is chosen. If set to a [`ThemePreference::Fixed`] value,
    /// then the given theme is used regardless of the terminal's background color.
    /// This corresponds with the `BAT_THEME` environment variable and the `--theme` option.
    pub theme: ThemePreference,
    /// The theme to use in case the terminal uses a dark background with light text.
    /// This corresponds with the `BAT_THEME_DARK` environment variable and the `--theme-dark` option.
    pub theme_dark: Option<ThemeName>,
    /// The theme to use in case the terminal uses a light background with dark text.
    /// This corresponds with the `BAT_THEME_LIGHT` environment variable and the `--theme-light` option.
    pub theme_light: Option<ThemeName>,
}

/// What theme should `bat` use?
///
/// The easiest way to construct this is from a string:
/// ```
/// # use bat::theme::{ThemePreference, DetectColorScheme};
/// let preference = ThemePreference::new("auto:system");
/// assert_eq!(ThemePreference::Auto(DetectColorScheme::System), preference);
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ThemePreference {
    /// Choose between [`ThemeOptions::theme_dark`] and [`ThemeOptions::theme_light`]
    /// based on the terminal's color scheme.
    Auto(DetectColorScheme),
    /// Always use the same theme regardless of the terminal's color scheme.
    Fixed(ThemeName),
    /// Use a dark theme.
    Dark,
    /// Use a light theme.
    Light,
}

impl Default for ThemePreference {
    fn default() -> Self {
        ThemePreference::Auto(Default::default())
    }
}

impl ThemePreference {
    /// Creates a theme preference from a string.
    pub fn new(s: impl Into<String>) -> Self {
        use ThemePreference::*;
        let s = s.into();
        match s.as_str() {
            "auto" => Auto(Default::default()),
            "auto:always" => Auto(DetectColorScheme::Always),
            "auto:system" => Auto(DetectColorScheme::System),
            "dark" => Dark,
            "light" => Light,
            _ => Fixed(ThemeName::new(s)),
        }
    }
}

impl FromStr for ThemePreference {
    type Err = Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(ThemePreference::new(s))
    }
}

impl fmt::Display for ThemePreference {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use ThemePreference::*;
        match self {
            Auto(DetectColorScheme::Auto) => f.write_str("auto"),
            Auto(DetectColorScheme::Always) => f.write_str("auto:always"),
            Auto(DetectColorScheme::System) => f.write_str("auto:system"),
            Fixed(theme) => theme.fmt(f),
            Dark => f.write_str("dark"),
            Light => f.write_str("light"),
        }
    }
}

/// The name of a theme or the default theme.
///
/// ```
/// # use bat::theme::ThemeName;
/// assert_eq!(ThemeName::Default, ThemeName::new("default"));
/// assert_eq!(ThemeName::Named("example".to_string()), ThemeName::new("example"));
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ThemeName {
    Named(String),
    Default,
}

impl ThemeName {
    /// Creates a theme name from a string.
    pub fn new(s: impl Into<String>) -> Self {
        let s = s.into();
        if s == "default" {
            ThemeName::Default
        } else {
            ThemeName::Named(s)
        }
    }
}

impl FromStr for ThemeName {
    type Err = Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(ThemeName::new(s))
    }
}

impl fmt::Display for ThemeName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ThemeName::Named(t) => f.write_str(t),
            ThemeName::Default => f.write_str("default"),
        }
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DetectColorScheme {
    /// Only query the terminal for its colors when appropriate (i.e. when the output is not redirected).
    #[default]
    Auto,
    /// Always query the terminal for its colors.
    Always,
    /// Detect the system-wide dark/light preference (macOS only).
    System,
}

/// The color scheme used to pick a fitting theme. Defaults to [`ColorScheme::Dark`].
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ColorScheme {
    #[default]
    Dark,
    Light,
}

/// The resolved theme and the color scheme as determined from
/// the terminal, OS or fallback.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ThemeResult {
    /// The theme selected according to the [`ThemeOptions`].
    pub theme: ThemeName,
    /// Either the user's chosen color scheme, the terminal's color scheme, the OS's
    /// color scheme or `None` if the color scheme was not detected because the user chose a fixed theme.
    pub color_scheme: Option<ColorScheme>,
}

impl fmt::Display for ThemeResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.theme {
            ThemeName::Named(name) => f.write_str(name),
            ThemeName::Default => f.write_str(default_theme(self.color_scheme.unwrap_or_default())),
        }
    }
}

fn theme_impl(options: ThemeOptions, detector: &dyn ColorSchemeDetector) -> ThemeResult {
    // Implementation note: This function is mostly pure (i.e. it has no side effects) for the sake of testing.
    // All the side effects (e.g. querying the terminal for its colors) are performed in the detector.
    match options.theme {
        ThemePreference::Fixed(theme) => ThemeResult {
            theme,
            color_scheme: None,
        },
        ThemePreference::Dark => choose_theme_opt(Some(ColorScheme::Dark), options),
        ThemePreference::Light => choose_theme_opt(Some(ColorScheme::Light), options),
        ThemePreference::Auto(when) => choose_theme_opt(color_scheme_impl(when, detector), options),
    }
}

fn choose_theme_opt(color_scheme: Option<ColorScheme>, options: ThemeOptions) -> ThemeResult {
    ThemeResult {
        color_scheme,
        theme: color_scheme
            .and_then(|c| choose_theme(options, c))
            .unwrap_or(ThemeName::Default),
    }
}

fn choose_theme(options: ThemeOptions, color_scheme: ColorScheme) -> Option<ThemeName> {
    match color_scheme {
        ColorScheme::Dark => options.theme_dark,
        ColorScheme::Light => options.theme_light,
    }
}

fn color_scheme_impl(
    when: DetectColorScheme,
    detector: &dyn ColorSchemeDetector,
) -> Option<ColorScheme> {
    let should_detect = match when {
        DetectColorScheme::Auto => detector.should_detect(),
        DetectColorScheme::Always => true,
        DetectColorScheme::System => return color_scheme_from_system(),
    };
    should_detect.then(|| detector.detect()).flatten()
}

trait ColorSchemeDetector {
    fn should_detect(&self) -> bool;

    fn detect(&self) -> Option<ColorScheme>;
}

struct TerminalColorSchemeDetector;

impl ColorSchemeDetector for TerminalColorSchemeDetector {
    fn should_detect(&self) -> bool {
        // Querying the terminal for its colors via OSC 10 / OSC 11 requires "exclusive" access
        // since we read/write from the terminal and enable/disable raw mode.
        // This causes race conditions with pagers such as less when they are attached to the
        // same terminal as us.
        //
        // This is usually only an issue when the output is manually piped to a pager.
        // For example: `bat Cargo.toml | less`.
        // Otherwise, if we start the pager ourselves, then there's no race condition
        // since the pager is started *after* the color is detected.
        std::io::stdout().is_terminal()
    }

    fn detect(&self) -> Option<ColorScheme> {
        use terminal_colorsaurus::{theme_mode, QueryOptions, ThemeMode};
        match theme_mode(QueryOptions::default()).ok()? {
            ThemeMode::Dark => Some(ColorScheme::Dark),
            ThemeMode::Light => Some(ColorScheme::Light),
        }
    }
}

#[cfg(not(target_os = "macos"))]
fn color_scheme_from_system() -> Option<ColorScheme> {
    crate::bat_warning!(
        "Theme 'auto:system' is only supported on macOS, \
        using default."
    );
    None
}

#[cfg(target_os = "macos")]
fn color_scheme_from_system() -> Option<ColorScheme> {
    const PREFERENCES_FILE: &str = "Library/Preferences/.GlobalPreferences.plist";
    const STYLE_KEY: &str = "AppleInterfaceStyle";

    let preferences_file = home::home_dir()
        .map(|home| home.join(PREFERENCES_FILE))
        .expect("Could not get home directory");

    match plist::Value::from_file(preferences_file).map(|file| file.into_dictionary()) {
        Ok(Some(preferences)) => match preferences.get(STYLE_KEY).and_then(|val| val.as_string()) {
            Some("Dark") => Some(ColorScheme::Dark),
            // If the key does not exist, then light theme is currently in use.
            Some(_) | None => Some(ColorScheme::Light),
        },
        // Unreachable, in theory. All macOS users have a home directory and preferences file setup.
        Ok(None) | Err(_) => None,
    }
}

#[cfg(test)]
impl ColorSchemeDetector for Option<ColorScheme> {
    fn should_detect(&self) -> bool {
        true
    }

    fn detect(&self) -> Option<ColorScheme> {
        *self
    }
}

#[cfg(test)]
mod tests {
    use super::ColorScheme::*;
    use super::*;
    use std::cell::Cell;
    use std::iter;

    mod color_scheme_detection {
        use super::*;

        #[test]
        fn not_called_for_dark_or_light() {
            for theme in [ThemePreference::Dark, ThemePreference::Light] {
                let detector = DetectorStub::should_detect(Some(Dark));
                let options = ThemeOptions {
                    theme,
                    ..Default::default()
                };
                _ = theme_impl(options, &detector);
                assert!(!detector.was_called.get());
            }
        }

        #[test]
        fn called_for_always() {
            let detectors = [
                DetectorStub::should_detect(Some(Dark)),
                DetectorStub::should_not_detect(),
            ];
            for detector in detectors {
                let options = ThemeOptions {
                    theme: ThemePreference::Auto(DetectColorScheme::Always),
                    ..Default::default()
                };
                _ = theme_impl(options, &detector);
                assert!(detector.was_called.get());
            }
        }

        #[test]
        fn called_for_auto_if_should_detect() {
            let detector = DetectorStub::should_detect(Some(Dark));
            _ = theme_impl(ThemeOptions::default(), &detector);
            assert!(detector.was_called.get());
        }

        #[test]
        fn not_called_for_auto_if_not_should_detect() {
            let detector = DetectorStub::should_not_detect();
            _ = theme_impl(ThemeOptions::default(), &detector);
            assert!(!detector.was_called.get());
        }
    }

    mod precedence {
        use super::*;

        #[test]
        fn theme_is_preferred_over_light_or_dark_themes() {
            for color_scheme in optional(color_schemes()) {
                for options in [
                    ThemeOptions {
                        theme: ThemePreference::Fixed(ThemeName::Named("Theme".to_string())),
                        ..Default::default()
                    },
                    ThemeOptions {
                        theme: ThemePreference::Fixed(ThemeName::Named("Theme".to_string())),
                        theme_dark: Some(ThemeName::Named("Dark Theme".to_string())),
                        theme_light: Some(ThemeName::Named("Light Theme".to_string())),
                    },
                ] {
                    let detector = ConstantDetector(color_scheme);
                    assert_eq!("Theme", theme_impl(options, &detector).to_string());
                }
            }
        }

        #[test]
        fn detector_is_not_called_if_theme_is_present() {
            let options = ThemeOptions {
                theme: ThemePreference::Fixed(ThemeName::Named("Theme".to_string())),
                ..Default::default()
            };
            let detector = DetectorStub::should_detect(Some(Dark));
            _ = theme_impl(options, &detector);
            assert!(!detector.was_called.get());
        }
    }

    mod default_theme {
        use super::*;

        #[test]
        fn default_dark_if_unable_to_detect_color_scheme() {
            let detector = ConstantDetector(None);
            assert_eq!(
                default_theme(ColorScheme::Dark),
                theme_impl(ThemeOptions::default(), &detector).to_string()
            );
        }

        // For backwards compatibility, if the default theme is requested
        // explicitly through BAT_THEME, we always pick the default dark theme.
        #[test]
        fn default_dark_if_requested_explicitly_through_theme() {
            for color_scheme in optional(color_schemes()) {
                let options = ThemeOptions {
                    theme: ThemePreference::Fixed(ThemeName::Default),
                    ..Default::default()
                };
                let detector = ConstantDetector(color_scheme);
                assert_eq!(
                    default_theme(ColorScheme::Dark),
                    theme_impl(options, &detector).to_string()
                );
            }
        }

        #[test]
        fn varies_depending_on_color_scheme() {
            for color_scheme in color_schemes() {
                for options in [
                    ThemeOptions::default(),
                    ThemeOptions {
                        theme_dark: Some(ThemeName::Default),
                        theme_light: Some(ThemeName::Default),
                        ..Default::default()
                    },
                ] {
                    let detector = ConstantDetector(Some(color_scheme));
                    assert_eq!(
                        default_theme(color_scheme),
                        theme_impl(options, &detector).to_string()
                    );
                }
            }
        }
    }

    mod choosing {
        use super::*;

        #[test]
        fn chooses_default_theme_if_unknown() {
            let options = ThemeOptions {
                theme_dark: Some(ThemeName::Named("Dark".to_string())),
                theme_light: Some(ThemeName::Named("Light".to_string())),
                ..Default::default()
            };
            let detector = ConstantDetector(None);
            assert_eq!(
                default_theme(ColorScheme::default()),
                theme_impl(options, &detector).to_string()
            );
        }

        #[test]
        fn chooses_dark_theme_if_dark_or_unknown() {
            let options = ThemeOptions {
                theme_dark: Some(ThemeName::Named("Dark".to_string())),
                theme_light: Some(ThemeName::Named("Light".to_string())),
                ..Default::default()
            };
            let detector = ConstantDetector(Some(ColorScheme::Dark));
            assert_eq!("Dark", theme_impl(options, &detector).to_string());
        }

        #[test]
        fn chooses_light_theme_if_light() {
            let options = ThemeOptions {
                theme_dark: Some(ThemeName::Named("Dark".to_string())),
                theme_light: Some(ThemeName::Named("Light".to_string())),
                ..Default::default()
            };
            let detector = ConstantDetector(Some(ColorScheme::Light));
            assert_eq!("Light", theme_impl(options, &detector).to_string());
        }
    }

    mod theme_preference {
        use super::*;

        #[test]
        fn values_roundtrip_via_display() {
            let prefs = [
                ThemePreference::Auto(DetectColorScheme::Auto),
                ThemePreference::Auto(DetectColorScheme::Always),
                ThemePreference::Auto(DetectColorScheme::System),
                ThemePreference::Fixed(ThemeName::Default),
                ThemePreference::Fixed(ThemeName::new("foo")),
                ThemePreference::Dark,
                ThemePreference::Light,
            ];
            for pref in prefs {
                assert_eq!(pref, ThemePreference::new(pref.to_string()));
            }
        }
    }

    struct DetectorStub {
        should_detect: bool,
        color_scheme: Option<ColorScheme>,
        was_called: Cell<bool>,
    }

    impl DetectorStub {
        fn should_detect(color_scheme: Option<ColorScheme>) -> Self {
            DetectorStub {
                should_detect: true,
                color_scheme,
                was_called: Cell::default(),
            }
        }

        fn should_not_detect() -> Self {
            DetectorStub {
                should_detect: false,
                color_scheme: None,
                was_called: Cell::default(),
            }
        }
    }

    impl ColorSchemeDetector for DetectorStub {
        fn should_detect(&self) -> bool {
            self.should_detect
        }

        fn detect(&self) -> Option<ColorScheme> {
            self.was_called.set(true);
            self.color_scheme
        }
    }

    struct ConstantDetector(Option<ColorScheme>);

    impl ColorSchemeDetector for ConstantDetector {
        fn should_detect(&self) -> bool {
            true
        }

        fn detect(&self) -> Option<ColorScheme> {
            self.0
        }
    }

    fn optional<T>(value: impl Iterator<Item = T>) -> impl Iterator<Item = Option<T>> {
        value.map(Some).chain(iter::once(None))
    }

    fn color_schemes() -> impl Iterator<Item = ColorScheme> {
        [Dark, Light].into_iter()
    }
}

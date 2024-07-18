use std::convert::Infallible;
use std::str::FromStr;

/// Chooses an appropriate theme or falls back to a default theme
/// based on the user-provided options and the color scheme of the terminal.
pub fn theme(options: ThemeOptions, detector: &dyn ColorSchemeDetector) -> String {
    // Implementation note: This function is mostly pure (i.e. it has no side effects) for the sake of testing.
    // All the side effects (e.g. querying the terminal for its colors) are performed in the detector.
    if let Some(theme) = options.theme {
        theme.into_theme(ColorScheme::default())
    } else {
        let color_scheme = detect(options.detect_color_scheme, detector).unwrap_or_default();
        choose_theme(options, color_scheme)
            .map(|t| t.into_theme(color_scheme))
            .unwrap_or_else(|| default_theme(color_scheme).to_owned())
    }
}

fn choose_theme(options: ThemeOptions, color_scheme: ColorScheme) -> Option<ThemeRequest> {
    match color_scheme {
        ColorScheme::Dark => options.theme_dark,
        ColorScheme::Light => options.theme_light,
    }
}

fn detect(when: DetectColorScheme, detector: &dyn ColorSchemeDetector) -> Option<ColorScheme> {
    let should_detect = match when {
        DetectColorScheme::Auto => detector.should_detect(),
        DetectColorScheme::Always => true,
        DetectColorScheme::Never => false,
    };
    should_detect.then(|| detector.detect()).flatten()
}

pub(crate) const fn default_theme(color_scheme: ColorScheme) -> &'static str {
    match color_scheme {
        ColorScheme::Dark => "Monokai Extended",
        ColorScheme::Light => "Monokai Extended Light",
    }
}

/// Options for configuring the theme used for syntax highlighting.
#[derive(Debug, Default)]
pub struct ThemeOptions {
    /// Always use this theme regardless of the terminal's background color.
    pub theme: Option<ThemeRequest>,
    /// The theme to use in case the terminal uses a dark background with light text.
    pub theme_dark: Option<ThemeRequest>,
    /// The theme to use in case the terminal uses a light background with dark text.
    pub theme_light: Option<ThemeRequest>,
    /// Detect whether or not the terminal is dark or light by querying for its colors.
    pub detect_color_scheme: DetectColorScheme,
}

/// The name of a theme or the default theme.
#[derive(Debug)]
pub enum ThemeRequest {
    Named(String),
    Default,
}

impl FromStr for ThemeRequest {
    type Err = Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s == "default" {
            Ok(ThemeRequest::Default)
        } else {
            Ok(ThemeRequest::Named(s.to_owned()))
        }
    }
}

impl ThemeRequest {
    fn into_theme(self, color_scheme: ColorScheme) -> String {
        match self {
            ThemeRequest::Named(t) => t,
            ThemeRequest::Default => default_theme(color_scheme).to_owned(),
        }
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum DetectColorScheme {
    /// Only query the terminal for its colors when appropriate (e.g. when the the output is not redirected).
    #[default]
    Auto,
    /// Always query the terminal for its colors.
    Always,
    /// Never query the terminal for its colors.
    Never,
}

/// The color scheme used to pick a fitting theme. Defaults to [`ColorScheme::Dark`].
#[derive(Default, Copy, Clone)]
pub enum ColorScheme {
    #[default]
    Dark,
    Light,
}

pub trait ColorSchemeDetector {
    fn should_detect(&self) -> bool;

    fn detect(&self) -> Option<ColorScheme>;
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
    use super::DetectColorScheme::*;
    use super::*;
    use std::cell::Cell;
    use std::iter;

    mod color_scheme_detection {
        use super::*;

        #[test]
        fn not_called_for_never() {
            let detector = DetectorStub::should_detect(Some(Dark));
            let options = ThemeOptions {
                detect_color_scheme: Never,
                ..Default::default()
            };
            _ = theme(options, &detector);
            assert!(!detector.was_called.get());
        }

        #[test]
        fn called_for_always() {
            let detectors = [
                DetectorStub::should_detect(Some(Dark)),
                DetectorStub::should_not_detect(),
            ];
            for detector in detectors {
                let options = ThemeOptions {
                    detect_color_scheme: Always,
                    ..Default::default()
                };
                _ = theme(options, &detector);
                assert!(detector.was_called.get());
            }
        }

        #[test]
        fn called_for_auto_if_should_detect() {
            let detector = DetectorStub::should_detect(Some(Dark));
            _ = theme(ThemeOptions::default(), &detector);
            assert!(detector.was_called.get());
        }

        #[test]
        fn not_called_for_auto_if_not_should_detect() {
            let detector = DetectorStub::should_not_detect();
            _ = theme(ThemeOptions::default(), &detector);
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
                        theme: Some(ThemeRequest::Named("Theme".to_string())),
                        ..Default::default()
                    },
                    ThemeOptions {
                        theme: Some(ThemeRequest::Named("Theme".to_string())),
                        theme_dark: Some(ThemeRequest::Named("Dark Theme".to_string())),
                        theme_light: Some(ThemeRequest::Named("Light Theme".to_string())),
                        ..Default::default()
                    },
                ] {
                    let detector = ConstantDetector(color_scheme);
                    assert_eq!("Theme", theme(options, &detector));
                }
            }
        }

        #[test]
        fn detector_is_not_called_if_theme_is_present() {
            let options = ThemeOptions {
                theme: Some(ThemeRequest::Named("Theme".to_string())),
                ..Default::default()
            };
            let detector = DetectorStub::should_detect(Some(Dark));
            _ = theme(options, &detector);
            assert!(!detector.was_called.get());
        }
    }

    mod default_theme {
        use super::*;

        #[test]
        fn dark_if_unable_to_detect_color_scheme() {
            let detector = ConstantDetector(None);
            assert_eq!(
                default_theme(ColorScheme::Dark),
                theme(ThemeOptions::default(), &detector)
            );
        }

        // For backwards compatibility, if the default theme is requested
        // explicitly through BAT_THEME, we always pick the default dark theme.
        #[test]
        fn dark_if_requested_explicitly_through_theme() {
            for color_scheme in optional(color_schemes()) {
                let options = ThemeOptions {
                    theme: Some(ThemeRequest::Default),
                    ..Default::default()
                };
                let detector = ConstantDetector(color_scheme);
                assert_eq!(default_theme(ColorScheme::Dark), theme(options, &detector));
            }
        }

        #[test]
        fn varies_depending_on_color_scheme() {
            for color_scheme in color_schemes() {
                for options in [
                    ThemeOptions::default(),
                    ThemeOptions {
                        theme_dark: Some(ThemeRequest::Default),
                        theme_light: Some(ThemeRequest::Default),
                        ..Default::default()
                    },
                ] {
                    let detector = ConstantDetector(Some(color_scheme));
                    assert_eq!(default_theme(color_scheme), theme(options, &detector));
                }
            }
        }
    }

    mod choosing {
        use super::*;

        #[test]
        fn chooses_dark_theme_if_dark_or_unknown() {
            for color_scheme in [Some(Dark), None] {
                let options = ThemeOptions {
                    theme_dark: Some(ThemeRequest::Named("Dark".to_string())),
                    theme_light: Some(ThemeRequest::Named("Light".to_string())),
                    ..Default::default()
                };
                let detector = ConstantDetector(color_scheme);
                assert_eq!("Dark", theme(options, &detector));
            }
        }

        #[test]
        fn chooses_light_theme_if_light() {
            let options = ThemeOptions {
                theme_dark: Some(ThemeRequest::Named("Dark".to_string())),
                theme_light: Some(ThemeRequest::Named("Light".to_string())),
                ..Default::default()
            };
            let detector = ConstantDetector(Some(ColorScheme::Light));
            assert_eq!("Light", theme(options, &detector));
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

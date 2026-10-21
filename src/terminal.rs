use nu_ansi_term::Color::{self, Fixed, Rgb};
use nu_ansi_term::{self, Style};

use syntect::highlighting::{self, FontStyle};

pub fn to_ansi_color(color: highlighting::Color, true_color: bool) -> Option<nu_ansi_term::Color> {
    if color.a == 0 {
        // Themes can specify one of the user-configurable terminal colors by
        // encoding them as #RRGGBBAA with AA set to 00 (transparent) and RR set
        // to the 8-bit color palette number. The built-in themes ansi, base16,
        // and base16-256 use this.
        Some(match color.r {
            // For the first 8 colors, use the Color enum to produce ANSI escape
            // sequences using codes 30-37 (foreground) and 40-47 (background).
            // For example, red foreground is \x1b[31m. This works on terminals
            // without 256-color support.
            0x00 => Color::Black,
            0x01 => Color::Red,
            0x02 => Color::Green,
            0x03 => Color::Yellow,
            0x04 => Color::Blue,
            0x05 => Color::Purple,
            0x06 => Color::Cyan,
            0x07 => Color::White,
            // For all other colors, use Fixed to produce escape sequences using
            // codes 38;5 (foreground) and 48;5 (background). For example,
            // bright red foreground is \x1b[38;5;9m. This only works on
            // terminals with 256-color support.
            //
            // TODO: When ansi_term adds support for bright variants using codes
            // 90-97 (foreground) and 100-107 (background), we should use those
            // for values 0x08 to 0x0f and only use Fixed for 0x10 to 0xff.
            n => Fixed(n),
        })
    } else if color.a == 1 {
        // Themes can specify the terminal's default foreground/background color
        // (i.e. no escape sequence) using the encoding #RRGGBBAA with AA set to
        // 01. The built-in theme ansi uses this.
        None
    } else if true_color {
        Some(Rgb(color.r, color.g, color.b))
    } else {
        Some(Fixed(ansi_colours::ansi256_from_rgb((
            color.r, color.g, color.b,
        ))))
    }
}

pub(crate) fn to_ansi_color_filtered(
    color: highlighting::Color,
    true_color: bool,
    grayscale: bool,
) -> Option<nu_ansi_term::Color> {
    if !grayscale || color.a == 1 {
        return to_ansi_color(color, true_color);
    }
    let (r, g, b) = if color.a == 0 {
        ansi_colours::rgb_from_ansi256(color.r)
    } else {
        (color.r, color.g, color.b)
    };
    let gray =
        ((2126 * u32::from(r) + 7152 * u32::from(g) + 722 * u32::from(b) + 5000) / 10000) as u8;
    to_ansi_color(
        highlighting::Color {
            r: gray,
            g: gray,
            b: gray,
            a: 255,
        },
        true_color,
    )
}

pub(crate) fn as_terminal_escaped(
    style: highlighting::Style,
    text: &str,
    true_color: bool,
    colored: bool,
    italics: bool,
    background_color: Option<highlighting::Color>,
    grayscale: bool,
) -> String {
    if text.is_empty() {
        return text.to_string();
    }

    let mut style = if !colored {
        Style::default()
    } else {
        let mut color = Style {
            foreground: to_ansi_color_filtered(style.foreground, true_color, grayscale),
            ..Style::default()
        };
        if style.font_style.contains(FontStyle::BOLD) {
            color = color.bold();
        }
        if style.font_style.contains(FontStyle::UNDERLINE) {
            color = color.underline();
        }
        if italics && style.font_style.contains(FontStyle::ITALIC) {
            color = color.italic();
        }
        color
    };

    style.background =
        background_color.and_then(|c| to_ansi_color_filtered(c, true_color, grayscale));
    style.paint(text).to_string()
}

#[cfg(test)]
mod grayscale_tests {
    use super::*;

    #[test]
    fn converts_rgb_and_palette_colors_to_neutral_output() {
        for index in 0..=255 {
            for color in [
                highlighting::Color {
                    r: index,
                    g: 0,
                    b: 0,
                    a: 0,
                },
                highlighting::Color {
                    r: index,
                    g: 255 - index,
                    b: 73,
                    a: 255,
                },
            ] {
                for true_color in [true, false] {
                    let actual = to_ansi_color_filtered(color, true_color, true).unwrap();
                    let (r, g, b) = match actual {
                        Rgb(r, g, b) => (r, g, b),
                        Fixed(index) => ansi_colours::rgb_from_ansi256(index),
                        _ => panic!("unexpected unfiltered palette color: {actual:?}"),
                    };
                    assert_eq!(r, g);
                    assert_eq!(g, b);
                }
            }
        }
    }

    #[test]
    fn preserves_default_colors_and_grayscale_endpoints() {
        let default = highlighting::Color {
            r: 0,
            g: 0,
            b: 0,
            a: 1,
        };
        assert_eq!(to_ansi_color_filtered(default, true, true), None);
        for value in [0, 127, 255] {
            let color = highlighting::Color {
                r: value,
                g: value,
                b: value,
                a: 255,
            };
            assert_eq!(
                to_ansi_color_filtered(color, true, true),
                Some(Rgb(value, value, value))
            );
        }
    }

    #[test]
    fn preserves_font_styles_and_filters_backgrounds() {
        let style = highlighting::Style {
            foreground: highlighting::Color {
                r: 255,
                g: 0,
                b: 0,
                a: 255,
            },
            background: highlighting::Color {
                r: 0,
                g: 255,
                b: 0,
                a: 255,
            },
            font_style: FontStyle::BOLD | FontStyle::ITALIC | FontStyle::UNDERLINE,
        };
        let output = as_terminal_escaped(
            style,
            "text",
            true,
            true,
            true,
            Some(style.background),
            true,
        );
        assert!(output.contains("1;3;4;"), "{output:?}");
        assert!(output.contains("38;2;54;54;54"), "{output:?}");
        assert!(output.contains("48;2;182;182;182"), "{output:?}");
    }
}

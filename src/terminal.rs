use ansi_term::Color::{self, Fixed, RGB};
use ansi_term::{self, Style};

use syntect::highlighting::{self, FontStyle};

pub fn to_ansi_color(color: highlighting::Color, true_color: bool) -> ansi_term::Color {
    if color.a == 0 {
        // Themes can specify one of the user-configurable terminal colors by
        // encoding them as #RRGGBBAA with AA set to 00 (transparent) and RR set
        // to the 8-bit color palette number. The built-in themes ansi-light,
        // ansi-dark, base16, and base16-256 use this.
        match color.r {
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
        }
    } else if true_color {
        RGB(color.r, color.g, color.b)
    } else {
        Fixed(ansi_colours::ansi256_from_rgb((color.r, color.g, color.b)))
    }
}

pub fn as_terminal_escaped(
    style: highlighting::Style,
    text: &str,
    true_color: bool,
    colored: bool,
    italics: bool,
    background_color: Option<highlighting::Color>,
) -> String {
    if text.is_empty() {
        return text.to_string();
    }

    let mut style = if !colored {
        Style::default()
    } else {
        let mut color = Style::from(to_ansi_color(style.foreground, true_color));
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

    style.background = background_color.map(|c| to_ansi_color(c, true_color));
    style.paint(text).to_string()
}

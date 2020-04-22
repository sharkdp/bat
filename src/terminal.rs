use ansi_term::Color::{self, Fixed, RGB};
use ansi_term::{self, Style};

use syntect::highlighting::{self, FontStyle};

pub fn to_ansi_color(color: highlighting::Color, true_color: bool) -> ansi_term::Color {
    if color.a == 0 {
        // Themes can specify one of the user-configurable terminal colors by
        // encoding them as #RRGGBBAA with AA set to 00 (transparent) and RR set
        // to the color palette number. The built-in themes ansi-light,
        // ansi-dark, and base16 use this.
        Fixed(color.r)
    } else if color.a == 0x0f {
        match color.r {
            0x00 => Color::Black,
            0x01 => Color::Red,
            0x02 => Color::Green,
            0x03 => Color::Yellow,
            0x04 => Color::Blue,
            0x05 => Color::Purple,
            0x06 => Color::Cyan,
            0x07 => Color::White,
            // TODO: the following should be high-intensity variants of
            // these colors ("bright black", "bright red", ...).
            0x08 => Color::Black,
            0x09 => Color::Red,
            0x0a => Color::Green,
            0x0b => Color::Yellow,
            0x0c => Color::Blue,
            0x0d => Color::Purple,
            0x0e => Color::Cyan,
            0x0f => Color::White,
            _ => unreachable!("The 0x0f color encoding does not allow for codes higher than 0x0f"),
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

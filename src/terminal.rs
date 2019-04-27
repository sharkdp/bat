extern crate ansi_colours;

use ansi_term::Colour::{Fixed, RGB};
use ansi_term::{self, Style};

use syntect::highlighting::{self, FontStyle};

pub fn to_ansi_color(color: highlighting::Color, true_color: bool) -> ansi_term::Colour {
    if color.a == 0 {
        // Themes can specify one of the user-configurable terminal colors by
        // encoding them as #RRGGBBAA with AA set to 00 (transparent) and RR set
        // to the color palette number. The built-in themes ansi-light,
        // ansi-dark, and base16 use this.
        Fixed(color.r)
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
    let mut style = if !colored {
        Style::default()
    } else {
        let color = to_ansi_color(style.foreground, true_color);

        if style.font_style.contains(FontStyle::BOLD) {
            color.bold()
        } else if style.font_style.contains(FontStyle::UNDERLINE) {
            color.underline()
        } else if italics && style.font_style.contains(FontStyle::ITALIC) {
            color.italic()
        } else {
            color.normal()
        }
    };

    style.background = background_color.map(|c| to_ansi_color(c, true_color));
    style.paint(text).to_string()
}

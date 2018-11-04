extern crate ansi_colours;

use ansi_term::Colour::{Fixed, RGB};
use ansi_term::{self, Style};

use syntect::highlighting::{self, FontStyle};

pub fn to_ansi_color(color: highlighting::Color, true_color: bool) -> ansi_term::Colour {
    if true_color {
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
) -> String {
    let style = if !colored {
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

    style.paint(text).to_string()
}

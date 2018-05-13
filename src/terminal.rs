use ansi_term::Colour::{Fixed, RGB};
use ansi_term::Style;
use syntect::highlighting::{self, FontStyle};

/// Approximate a 24 bit color value by a 8 bit ANSI code
fn rgb2ansi(r: u8, g: u8, b: u8) -> u8 {
    const BLACK: u8 = 16;
    const WHITE: u8 = 231;

    if r == g && g == b {
        if r < 8 {
            BLACK
        } else if r > 248 {
            WHITE
        } else {
            let fr = f32::from(r);
            (((fr - 8.) / 247.) * 24.) as u8 + 232
        }
    } else {
        let fr = f32::from(r);
        let fg = f32::from(g);
        let fb = f32::from(b);
        16 + (36 * (fr / 255. * 5.) as u8) + (6 * (fg / 255. * 5.) as u8) + (fb / 255. * 5.) as u8
    }
}

pub fn as_terminal_escaped(
    style: highlighting::Style,
    text: &str,
    true_color: bool,
    colored: bool,
) -> String {
    let style = if !colored {
        Style::default()
    } else {
        let color = if true_color {
            RGB(style.foreground.r, style.foreground.g, style.foreground.b)
        } else {
            let ansi = rgb2ansi(style.foreground.r, style.foreground.g, style.foreground.b);
            Fixed(ansi)
        };

        if style.font_style.contains(FontStyle::BOLD) {
            color.bold()
        } else if style.font_style.contains(FontStyle::UNDERLINE) {
            color.underline()
        } else if style.font_style.contains(FontStyle::ITALIC) {
            color.italic()
        } else {
            color.normal()
        }
    };

    style.paint(text).to_string()
}

#[test]
fn test_rgb2ansi_black_white() {
    assert_eq!(16, rgb2ansi(0x00, 0x00, 0x00));
    assert_eq!(231, rgb2ansi(0xff, 0xff, 0xff));
}

#[test]
fn test_rgb2ansi_gray() {
    assert_eq!(241, rgb2ansi(0x6c, 0x6c, 0x6c));
    assert_eq!(233, rgb2ansi(0x1c, 0x1c, 0x1c));
}

#[test]
fn test_rgb2ansi_color() {
    assert_eq!(96, rgb2ansi(0x87, 0x5f, 0x87));
    assert_eq!(141, rgb2ansi(0xaf, 0x87, 0xff));
    assert_eq!(193, rgb2ansi(0xd7, 0xff, 0xaf));
}

#[test]
fn test_rgb2ansi_approx() {
    assert_eq!(231, rgb2ansi(0xfe, 0xfe, 0xfe));
}

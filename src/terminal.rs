use ansi_term::Colour::{Fixed, RGB};
use ansi_term::{self, Style};
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
            ((r - 8) as u16 * 24 / 247) as u8 + 232
        }
    } else {
        36 * (r / 51) + 6 * (g / 51) + (b / 51) + 16
    }
}

pub fn to_ansi_color(color: highlighting::Color, true_color: bool) -> ansi_term::Colour {
    if true_color {
        RGB(color.r, color.g, color.b)
    } else {
        let ansi_code = rgb2ansi(color.r, color.g, color.b);
        Fixed(ansi_code)
    }
}

/// Converts index on one dimension of the 6×6×6 ANSI colour cube into value in
/// sRGB space.
#[cfg(test)]
fn cube_value(i: u8) -> u8 {
    if i == 0 {
        0
    } else {
        55 + i * 40
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
        let color = to_ansi_color(style.foreground, true_color);

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

/// Calculates distance between two colours.  Tries to balance speed of
/// computation and perceptual correctness.
#[cfg(test)]
fn distance(r1: u8, g1: u8, b1: u8, r2: u8, g2: u8, b2: u8) -> f64 {
    let r_mean = (r1 as i32 + r2 as i32) / 2;
    let r = r1 as i32 - r2 as i32;
    let g = g1 as i32 - g2 as i32;
    let b = b1 as i32 - b2 as i32;
    // See <https://www.compuphase.com/cmetric.htm>.
    let d = ((512 + r_mean) * r * r + 1024 * (g * g) + (767 - r_mean) * b * b) as u32;
    (d as f64 / 2303.).sqrt()
}

#[test]
fn test_distance() {
    fn ansi2rgb(idx: u8) -> (u8, u8, u8) {
        if idx >= 232 {
            let v = (idx - 232) * 10 + 8;
            (v, v, v)
        } else {
            assert!(idx >= 16);
            let idx = idx - 16;
            (
                cube_value(idx / 36),
                cube_value(idx / 6 % 6),
                cube_value(idx % 6),
            )
        }
    }

    let mut max_distance = 0.0;
    let mut total_distance = 0.0;
    for c in 0..0xffffff {
        let r = (c >> 16) as u8;
        let g = (c >> 8) as u8;
        let b = c as u8;
        let (ar, ag, ab) = ansi2rgb(rgb2ansi(r, g, b));
        let dist = distance(r, g, b, ar, ag, ab);
        if dist > max_distance {
            max_distance = dist;
        }
        total_distance += dist;
    }

    let avg_distance = total_distance / 16777216.;
    assert_eq!(49769, (max_distance * 1000.0).round() as u32);
    assert_eq!(20027, (avg_distance * 1000.0).round() as u32);
}

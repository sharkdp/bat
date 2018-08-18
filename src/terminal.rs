use ansi_term::Colour::{Fixed, RGB};
use ansi_term::{self, Style};
use syntect::highlighting::{self, FontStyle};

/// Approximate a 24-bit color value by a 8 bit ANSI code
fn rgb2ansi(r: u8, g: u8, b: u8) -> u8 {
    if r == g && g == b {
        rgb2ansi_grey(r)
    } else {
        rgb2ansi_cube(r, g, b)
    }
}

/// Approximate a 24-bit colour as an index in greyscale ramp of the 256-colour
/// ANSI palette.
#[inline]
fn rgb2ansi_grey(y: u8) -> u8 {
    // In the 256-colour ANSI palette grey colours are included in the greyscale
    // ramp as well as in the colour cube.  Because of this we’re trying to
    // approximate grey in both and choose whichever gives better result.

    const BLACK: u8 = 16;
    const WHITE: u8 = 231;

    // The greyscale ramp starts at rgb(8, 8, 8) and steps every rgb(10, 10, 10)
    // until rgb(238, 238, 238).  Due to the asymmetry, edges need to be handled
    // separately.
    if y < 4 {
        return BLACK;
    } else if y >= 247 {
        return WHITE;
    } else if y >= 234 {
        return 255;
    }

    // We’re adding 6 so that division rounds to nearest rather than truncating.
    let gi = (y + 6) / 10;

    // There’s only a few values in which using colour cube for grey colours is
    // better.
    if y >= 92 && y <= 216 {
        let grey = (gi * 10 - 2) as i32;
        let yi = cube_index(y);
        if (cube_value(yi) as i32 - y as i32).abs() < (grey - y as i32).abs() {
            return 16 + (36 + 6 + 1) * yi;
        }
    }

    gi + 231
}

/// Approximate a 24-bit colour as an index in 6×6×6 colour cube of the
/// 256-colour ANSI palette.
#[inline]
fn rgb2ansi_cube(r: u8, g: u8, b: u8) -> u8 {
    let ri = cube_index(r);
    let gi = cube_index(g);
    let bi = cube_index(b);
    16 + ri * 36 + gi * 6 + bi
}

/// Approximates single r, g or b value to an index within a single side of the
/// 6×6×6 ANSI colour cube.
fn cube_index(v: u8) -> u8 {
    // Values within the cube are: 0, 95, 135, 175, 215 and 255.  Except for the
    // first jump they are 40 units apart.  Because of this first jump we need
    // a special case for the first two steps.
    if v < 48 {
        0
    } else if v < 115 {
        1
    } else {
        (v - 35) / 40
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
    assert_eq!(232, rgb2ansi(0x08, 0x08, 0x08));
    assert_eq!(234, rgb2ansi(0x1c, 0x1c, 0x1c));
    assert_eq!(242, rgb2ansi(0x6c, 0x6c, 0x6c));
    assert_eq!(255, rgb2ansi(0xee, 0xee, 0xee));
}

#[test]
fn test_rgb2ansi_color() {
    assert_eq!(96, rgb2ansi(0x87, 0x5f, 0x87));
    assert_eq!(141, rgb2ansi(0xaf, 0x87, 0xff));
    assert_eq!(193, rgb2ansi(0xd7, 0xff, 0xaf));
}

#[test]
fn test_rgb2ansi_grey_using_cube() {
    // Even though those are grey colours they have a perfect match in the
    // colour cube so use that rather than greyscale ramp.
    assert_eq!(59, rgb2ansi(0x5f, 0x5f, 0x5f));
    assert_eq!(102, rgb2ansi(0x87, 0x87, 0x87));
    assert_eq!(145, rgb2ansi(0xaf, 0xaf, 0xaf));
}

#[test]
fn test_rgb2ansi_approx() {
    assert_eq!(231, rgb2ansi(0xfe, 0xfe, 0xfe));
    // Approximate #070707 up to #080808 rather than down to #000000.
    assert_eq!(232, rgb2ansi(0x07, 0x07, 0x07));
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
    assert_eq!(47000, (max_distance * 1000.0).round() as u32);
    assert_eq!(17206, (avg_distance * 1000.0).round() as u32);
}

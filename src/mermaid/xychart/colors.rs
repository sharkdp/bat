/// Default chart accent when a theme does not provide a valid accent.
pub const CHART_ACCENT_FALLBACK: &str = "#3b82f6";

/// Return whether `color` is exactly `#` followed by six ASCII hexadecimal
/// digits.
pub fn is_valid_hex(color: &str) -> bool {
    color.len() == 7
        && color.starts_with('#')
        && color.as_bytes()[1..].iter().all(u8::is_ascii_hexdigit)
}

/// Return whether a hex color has HSL lightness below 50 percent.
pub fn is_dark_background(background: &str) -> bool {
    hex_to_hsl(background).2 < 50.0
}

/// Mix foreground over background in RGB space at `ratio` opacity.
#[allow(clippy::suboptimal_flops)]
pub fn mix_hex_colors(background: &str, foreground: &str, ratio: f64) -> String {
    let (br, bg, bb) = hex_to_rgb(background);
    let (fr, fg, fb) = hex_to_rgb(foreground);
    let inverse = 1.0 - ratio;
    rgb_to_hex(
        br * inverse + fr * ratio,
        bg * inverse + fg * ratio,
        bb * inverse + fb * ratio,
    )
}

/// Select the background-aware monochromatic palette color for a series.
#[allow(clippy::suboptimal_flops)]
pub fn get_series_color(index: usize, accent_color: &str, background: Option<&str>) -> String {
    if index == 0 {
        return accent_color.to_owned();
    }
    let safe_accent = if is_valid_hex(accent_color) {
        accent_color
    } else {
        CHART_ACCENT_FALLBACK
    };
    let safe_background = background.filter(|color| is_valid_hex(color));
    let (hue, saturation, _) = hex_to_hsl(safe_accent);
    let chart_saturation = saturation.clamp(55.0, 85.0);
    let tier = index.div_ceil(2) as f64;
    let odd_index = index % 2 == 1;
    let dark = safe_background.is_some_and(is_dark_background) != odd_index;
    let lightness = if dark {
        (48.0 - tier * 13.0).max(25.0)
    } else {
        (55.0 + tier * 11.0).min(78.0)
    };
    let hue_shift = if dark { -8.0 } else { 12.0 } * tier;
    let new_hue = (hue + hue_shift).rem_euclid(360.0);
    hsl_to_hex(new_hue, chart_saturation, lightness)
}

#[allow(clippy::manual_midpoint)]
fn hex_to_hsl(hex: &str) -> (f64, f64, f64) {
    let (red, green, blue) = hex_to_rgb(hex);
    let red = red / 255.0;
    let green = green / 255.0;
    let blue = blue / 255.0;
    if red.is_nan() || green.is_nan() || blue.is_nan() {
        return (f64::NAN, f64::NAN, f64::NAN);
    }
    let max = red.max(green).max(blue);
    let min = red.min(green).min(blue);
    let lightness = (max + min) / 2.0;
    if max == min {
        return (0.0, 0.0, lightness * 100.0);
    }
    let delta = max - min;
    let saturation = if lightness > 0.5 {
        delta / (2.0 - max - min)
    } else {
        delta / (max + min)
    };
    let hue = if max == red {
        ((green - blue) / delta + if green < blue { 6.0 } else { 0.0 }) / 6.0
    } else if max == green {
        ((blue - red) / delta + 2.0) / 6.0
    } else {
        ((red - green) / delta + 4.0) / 6.0
    };
    (hue * 360.0, saturation * 100.0, lightness * 100.0)
}

#[allow(clippy::suboptimal_flops)]
fn hsl_to_hex(hue: f64, saturation: f64, lightness: f64) -> String {
    let saturation = saturation / 100.0;
    let lightness = lightness / 100.0;
    let chroma = (1.0 - (2.0 * lightness - 1.0).abs()) * saturation;
    let x = chroma * (1.0 - ((hue / 60.0) % 2.0 - 1.0).abs());
    let m = lightness - chroma / 2.0;
    let (red, green, blue) = if hue < 60.0 {
        (chroma, x, 0.0)
    } else if hue < 120.0 {
        (x, chroma, 0.0)
    } else if hue < 180.0 {
        (0.0, chroma, x)
    } else if hue < 240.0 {
        (0.0, x, chroma)
    } else if hue < 300.0 {
        (x, 0.0, chroma)
    } else {
        (chroma, 0.0, x)
    };
    rgb_to_hex((red + m) * 255.0, (green + m) * 255.0, (blue + m) * 255.0)
}

fn hex_to_rgb(hex: &str) -> (f64, f64, f64) {
    let raw = hex.strip_prefix('#').unwrap_or(hex);
    let channel = |start: usize| {
        let Some(value) = raw.get(start..start.saturating_add(2).min(raw.len())) else {
            return f64::NAN;
        };
        let prefix = value
            .find(|character: char| !character.is_ascii_hexdigit())
            .unwrap_or(value.len());
        if prefix == 0 {
            f64::NAN
        } else {
            u8::from_str_radix(&value[..prefix], 16).map_or(f64::NAN, f64::from)
        }
    };
    (channel(0), channel(2), channel(4))
}

fn rgb_to_hex(red: f64, green: f64, blue: f64) -> String {
    let channel = |value: f64| {
        if value.is_nan() {
            "NaN".to_owned()
        } else {
            format!("{:02x}", value.clamp(0.0, 255.0).round() as u8)
        }
    };
    format!("#{}{}{}", channel(red), channel(green), channel(blue))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn palette_keeps_accent_and_flips_shades_on_dark_background() {
        assert_eq!(get_series_color(0, "#3b82f6", None), "#3b82f6");
        assert_eq!(get_series_color(1, "#3b82f6", Some("#ffffff")), "#0d5ba5");
        assert_eq!(get_series_color(1, "#3b82f6", Some("#000000")), "#5f79f2");
    }

    #[test]
    fn rgb_mixing_matches_alpha_compositing() {
        assert_eq!(mix_hex_colors("#000000", "#ffffff", 0.5), "#808080");
        assert_eq!(mix_hex_colors("bad", "also-bad", 0.5), "#62NaNNaN");
        assert!(!is_dark_background("#000"));
    }
}

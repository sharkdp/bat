//! Recognize syntax names in the already-buffered first line. Modelines are data:
//! no editor options, expressions, or commands are evaluated.

pub(crate) fn syntax_names(line: &str) -> Vec<(usize, &str)> {
    let mut names = Vec::new();
    if let Some(start) = line.find("-*-") {
        let rest = &line[start + 3..];
        if let Some(end) = rest.find("-*-") {
            let options = rest[..end].trim();
            if !options.contains([':', ';']) {
                if !options.is_empty() {
                    names.push((start, options));
                }
            } else {
                for option in options.split(';') {
                    if let Some((key, value)) = option.split_once(':') {
                        if key.trim().eq_ignore_ascii_case("mode") && !value.trim().is_empty() {
                            names.push((start, value.trim()));
                            break;
                        }
                    }
                }
            }
        }
    }

    for marker in ["vim:", "vi:", "ex:"] {
        for (start, _) in line.match_indices(marker) {
            if start > 0 && !line[..start].ends_with(char::is_whitespace) {
                continue;
            }
            let options = &line[start + marker.len()..];
            for option in options.split(|c: char| c.is_whitespace() || c == ':') {
                if let Some((key, value)) = option.split_once('=') {
                    if matches!(key, "ft" | "filetype" | "syntax") && !value.is_empty() {
                        names.push((start, value));
                        break;
                    }
                }
            }
        }
    }
    names.sort_by_key(|(offset, _)| *offset);
    names
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn recognize_only_complete_modelines() {
        for line in [
            "# -*- python",
            "# mode: python",
            "# notvim: ft=python",
            "vim: setting=python",
            "# -*- -*-",
        ] {
            assert!(syntax_names(line).is_empty(), "{line}");
        }
        assert_eq!(
            syntax_names("# -*- coding: utf-8; Mode: python; -*-"),
            vec![(2, "python")]
        );
        assert_eq!(
            syntax_names("/* vim: set ts=4 ft=cpp: */"),
            vec![(3, "cpp")]
        );
        assert_eq!(
            syntax_names("# -*- rust -*- vim: ft=python"),
            vec![(2, "rust"), (15, "python")]
        );
    }
}

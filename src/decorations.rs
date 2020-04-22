#[cfg(feature = "git")]
use crate::diff::LineChange;
use crate::printer::{Colors, InteractivePrinter};
use ansi_term::Style;

#[derive(Debug, Clone)]
pub(crate) struct DecorationText {
    pub width: usize,
    pub text: String,
}

pub(crate) trait Decoration {
    fn generate(
        &self,
        line_number: usize,
        continuation: bool,
        printer: &InteractivePrinter,
    ) -> DecorationText;
    fn width(&self) -> usize;
}

pub(crate) struct LineNumberDecoration {
    color: Style,
    cached_wrap: DecorationText,
    cached_wrap_invalid_at: usize,
}

impl LineNumberDecoration {
    pub(crate) fn new(colors: &Colors) -> Self {
        LineNumberDecoration {
            color: colors.line_number,
            cached_wrap_invalid_at: 10000,
            cached_wrap: DecorationText {
                text: colors.line_number.paint(" ".repeat(4)).to_string(),
                width: 4,
            },
        }
    }
}

impl Decoration for LineNumberDecoration {
    fn generate(
        &self,
        line_number: usize,
        continuation: bool,
        _printer: &InteractivePrinter,
    ) -> DecorationText {
        if continuation {
            if line_number > self.cached_wrap_invalid_at {
                let new_width = self.cached_wrap.width + 1;
                return DecorationText {
                    text: self.color.paint(" ".repeat(new_width)).to_string(),
                    width: new_width,
                };
            }

            self.cached_wrap.clone()
        } else {
            let plain: String = format!("{:4}", line_number);
            DecorationText {
                width: plain.len(),
                text: self.color.paint(plain).to_string(),
            }
        }
    }

    fn width(&self) -> usize {
        4
    }
}

#[cfg(feature = "git")]
pub(crate) struct LineChangesDecoration {
    cached_none: DecorationText,
    cached_added: DecorationText,
    cached_removed_above: DecorationText,
    cached_removed_below: DecorationText,
    cached_modified: DecorationText,
}

#[cfg(feature = "git")]
impl LineChangesDecoration {
    #[inline]
    fn generate_cached(style: Style, text: &str) -> DecorationText {
        DecorationText {
            text: style.paint(text).to_string(),
            width: text.chars().count(),
        }
    }

    pub(crate) fn new(colors: &Colors) -> Self {
        LineChangesDecoration {
            cached_none: Self::generate_cached(Style::default(), " "),
            cached_added: Self::generate_cached(colors.git_added, "+"),
            cached_removed_above: Self::generate_cached(colors.git_removed, "‾"),
            cached_removed_below: Self::generate_cached(colors.git_removed, "_"),
            cached_modified: Self::generate_cached(colors.git_modified, "~"),
        }
    }
}

#[cfg(feature = "git")]
impl Decoration for LineChangesDecoration {
    fn generate(
        &self,
        line_number: usize,
        continuation: bool,
        printer: &InteractivePrinter,
    ) -> DecorationText {
        if !continuation {
            if let Some(ref changes) = printer.line_changes {
                return match changes.get(&(line_number as u32)) {
                    Some(&LineChange::Added) => self.cached_added.clone(),
                    Some(&LineChange::RemovedAbove) => self.cached_removed_above.clone(),
                    Some(&LineChange::RemovedBelow) => self.cached_removed_below.clone(),
                    Some(&LineChange::Modified) => self.cached_modified.clone(),
                    _ => self.cached_none.clone(),
                };
            }
        }

        self.cached_none.clone()
    }

    fn width(&self) -> usize {
        self.cached_none.width
    }
}

pub(crate) struct GridBorderDecoration {
    cached: DecorationText,
}

impl GridBorderDecoration {
    pub(crate) fn new(colors: &Colors) -> Self {
        GridBorderDecoration {
            cached: DecorationText {
                text: colors.grid.paint("│").to_string(),
                width: 1,
            },
        }
    }
}

impl Decoration for GridBorderDecoration {
    fn generate(
        &self,
        _line_number: usize,
        _continuation: bool,
        _printer: &InteractivePrinter,
    ) -> DecorationText {
        self.cached.clone()
    }

    fn width(&self) -> usize {
        self.cached.width
    }
}

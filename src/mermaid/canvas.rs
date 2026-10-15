//! Column-major 2D text canvas plus the parallel role canvas used for
//! colored output.
//!
//! [`Canvas`] cells hold one terminal column each: a single code point in the
//! common case, or a whole multi-code-point grapheme cluster. A two-column
//! glyph occupies its lead cell followed by a [`WIDE_PAD`] cell; every write
//! path keeps that pair atomic so serialized rows occupy exactly one terminal
//! column per cell.

use std::{collections::BTreeMap, fmt, rc::Rc};

use unicode_segmentation::UnicodeSegmentation;

use super::{
    ansi::{colorize_line, CharRole, ColorMode, RoleCell, Theme},
    text::{char_width, display_width, grapheme_width, split_lines, LABEL_SPACE, WIDE_PAD},
};

/// Character-level coordinate on the text canvas. Signed so intermediate
/// layout math can dip below zero without wrapping; out-of-range reads yield
/// `None` and out-of-range writes are ignored.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub struct DrawingCoord {
    pub x: i32,
    pub y: i32,
}

impl DrawingCoord {
    pub const fn new(x: i32, y: i32) -> Self {
        Self { x, y }
    }
}

/// One canvas column: a single code point stored inline, or a multi-code-point
/// grapheme cluster (emoji ZWJ sequence, flag, combining marks).
#[derive(Clone, PartialEq, Eq)]
pub enum Cell {
    Char { buf: [u8; 4], len: u8 },
    Cluster(Rc<str>),
}

impl Default for Cell {
    fn default() -> Self {
        Self::SPACE
    }
}

impl fmt::Debug for Cell {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self.as_str(), f)
    }
}

impl From<char> for Cell {
    fn from(c: char) -> Self {
        let mut buf = [0u8; 4];
        let len = c.encode_utf8(&mut buf).len() as u8;
        Self::Char { buf, len }
    }
}

impl PartialEq<char> for Cell {
    fn eq(&self, other: &char) -> bool {
        self.as_char() == Some(*other)
    }
}

impl Cell {
    pub const LABEL_SPACE: Self = Self::Char {
        buf: [LABEL_SPACE as u8, 0, 0, 0],
        len: 1,
    };
    pub const SPACE: Self = Self::Char {
        buf: [b' ', 0, 0, 0],
        len: 1,
    };
    pub const WIDE_PAD: Self = Self::Char {
        buf: [WIDE_PAD as u8, 0, 0, 0],
        len: 1,
    };

    /// Cell holding one grapheme cluster. Single code points stay inline.
    pub fn new(cluster: &str) -> Self {
        let mut chars = cluster.chars();
        match (chars.next(), chars.next()) {
            (Some(c), None) => Self::from(c),
            _ => Self::Cluster(Rc::from(cluster)),
        }
    }

    #[inline]
    pub fn as_str(&self) -> &str {
        match self {
            Self::Char { buf, len } => {
                std::str::from_utf8(&buf[..*len as usize]).expect("valid utf-8 char")
            }
            Self::Cluster(s) => s,
        }
    }

    /// The cell's code point when it holds exactly one.
    #[inline]
    pub fn as_char(&self) -> Option<char> {
        match self {
            Self::Char { .. } => self.as_str().chars().next(),
            Self::Cluster(_) => None,
        }
    }

    #[inline]
    pub fn is_space(&self) -> bool {
        *self == ' '
    }

    #[inline]
    pub fn is_wide_pad(&self) -> bool {
        *self == WIDE_PAD
    }

    /// Display width in terminal columns (1 or 2).
    pub fn display_width(&self) -> i32 {
        match self.as_char() {
            Some(c) => char_width(c) as i32,
            None => grapheme_width(self.as_str()) as i32,
        }
    }

    /// Whether the cell holds label content for first-label-wins collision
    /// handling: letters/digits in any script, the continuation cell of a
    /// wide glyph, or any 2-column glyph (only labels produce wide glyphs).
    pub fn is_label(&self) -> bool {
        match self.as_char() {
            Some(c) => {
                c == LABEL_SPACE || c == WIDE_PAD || c.is_alphanumeric() || char_width(c) == 2
            }
            None => self.display_width() == 2 || self.as_str().chars().any(char::is_alphanumeric),
        }
    }
}

/// Expand text into canvas cells: each 2-column grapheme cluster is stored
/// whole followed by [`Cell::WIDE_PAD`], so `cells.len() ==
/// display_width(text)`.
pub fn to_cells(text: &str) -> Vec<Cell> {
    let mut cells = Vec::with_capacity(text.len());
    if text.is_ascii() {
        cells.extend(text.chars().map(Cell::from));
        return cells;
    }
    for cluster in text.graphemes(true) {
        cells.push(Cell::new(cluster));
        if grapheme_width(cluster) == 2 {
            cells.push(Cell::WIDE_PAD);
        }
    }
    cells
}

/// Column-major grid: `(x, y)` addresses column `x`, row `y`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Grid<T> {
    width: i32,
    height: i32,
    cells: Vec<T>,
}

/// Text canvas.
pub type Canvas = Grid<Cell>;
/// Role canvas parallel to a [`Canvas`]; `None` marks whitespace.
pub type RoleCanvas = Grid<Option<CharRole>>;

impl<T: Clone + Default> Grid<T> {
    /// Blank grid of `width × height` cells. Negative dimensions clamp to 0.
    pub fn new(width: i32, height: i32) -> Self {
        let width = width.max(0);
        let height = height.max(0);
        Self {
            width,
            height,
            cells: vec![T::default(); (width * height) as usize],
        }
    }

    /// Blank grid with the same dimensions as `self`.
    pub fn blank_like(&self) -> Self {
        Self::new(self.width, self.height)
    }

    #[inline]
    pub const fn width(&self) -> i32 {
        self.width
    }

    #[inline]
    pub const fn height(&self) -> i32 {
        self.height
    }

    #[inline]
    pub const fn in_bounds(&self, x: i32, y: i32) -> bool {
        x >= 0 && y >= 0 && x < self.width && y < self.height
    }

    #[inline]
    pub fn get(&self, x: i32, y: i32) -> Option<&T> {
        self.in_bounds(x, y)
            .then(|| &self.cells[(x * self.height + y) as usize])
    }

    #[inline]
    pub fn get_mut(&mut self, x: i32, y: i32) -> Option<&mut T> {
        self.in_bounds(x, y)
            .then(|| &mut self.cells[(x * self.height + y) as usize])
    }

    /// Write a cell; writes outside the grid are dropped.
    #[inline]
    pub fn set(&mut self, x: i32, y: i32, value: T) {
        if let Some(slot) = self.get_mut(x, y) {
            *slot = value;
        }
    }

    /// Grow to at least `width × height`, preserving existing content.
    pub fn ensure_size(&mut self, width: i32, height: i32) {
        let width = width.max(self.width);
        let height = height.max(self.height);
        if width == self.width && height == self.height {
            return;
        }
        if height == self.height {
            // Column-major: new columns append after the existing ones.
            self.cells.resize((width * height) as usize, T::default());
            self.width = width;
            return;
        }
        let mut grown = Self::new(width, height);
        for x in 0..self.width {
            for y in 0..self.height {
                grown.cells[(x * height + y) as usize] =
                    self.cells[(x * self.height + y) as usize].clone();
            }
        }
        *self = grown;
    }

    /// Grow so `(x, y)` is addressable, then write it.
    pub fn set_growing(&mut self, x: i32, y: i32, value: T) {
        if x < 0 || y < 0 {
            return;
        }
        self.ensure_size(x + 1, y + 1);
        self.set(x, y, value);
    }

    /// Mirror across the horizontal center (reverse each column).
    pub fn flip_vertical(&mut self) {
        for column in self.cells.chunks_mut(self.height.max(1) as usize) {
            column.reverse();
        }
    }

    /// Grow to fit every overlay at `offset`; overlays are not yet applied.
    fn grow_for_overlays(&mut self, offset: DrawingCoord, overlays: &[&Self]) {
        let mut width = self.width;
        let mut height = self.height;
        for overlay in overlays {
            width = width.max(overlay.width + offset.x);
            height = height.max(overlay.height + offset.y);
        }
        self.ensure_size(width, height);
    }
}

impl RoleCanvas {
    /// Set a role, growing the canvas when `(x, y)` lies outside it.
    pub fn set_role(&mut self, x: i32, y: i32, role: CharRole) {
        self.set_growing(x, y, Some(role));
    }

    /// Overlay role canvases at `offset`; `Some` roles overwrite the base.
    pub fn merge(&mut self, offset: DrawingCoord, overlays: &[&Self]) {
        self.grow_for_overlays(offset, overlays);
        for overlay in overlays {
            for x in 0..overlay.width {
                for y in 0..overlay.height {
                    if let Some(role) = overlay.get(x, y).copied().flatten() {
                        self.set(x + offset.x, y + offset.y, Some(role));
                    }
                }
            }
        }
    }
}

/// Box-drawing characters that participate in junction merging.
pub const fn is_junction_char(c: char) -> bool {
    matches!(
        c,
        '─' | '│'
            | '┌'
            | '┐'
            | '└'
            | '┘'
            | '├'
            | '┤'
            | '┬'
            | '┴'
            | '┼'
            | '╴'
            | '╵'
            | '╶'
            | '╷'
    )
}

/// Resolve two overlapping junction characters to their combined form
/// (`─` over `│` becomes `┼`). Unlisted pairs keep `existing`.
pub fn merge_junctions(existing: char, incoming: char) -> char {
    const ROW_CHARS: [char; 10] = ['─', '│', '┌', '┐', '└', '┘', '├', '┤', '┬', '┴'];
    const TABLE: [[char; 10]; 10] = [
        ['─', '┼', '┬', '┬', '┴', '┴', '┼', '┼', '┬', '┴'],
        ['┼', '│', '├', '┤', '├', '┤', '├', '┤', '┼', '┼'],
        ['┬', '├', '┌', '┬', '├', '┼', '├', '┼', '┬', '┼'],
        ['┬', '┤', '┬', '┐', '┼', '┤', '┼', '┤', '┬', '┼'],
        ['┴', '├', '├', '┼', '└', '┴', '├', '┼', '┼', '┴'],
        ['┴', '┤', '┼', '┤', '┴', '┘', '┼', '┤', '┼', '┴'],
        ['┼', '├', '├', '┼', '├', '┼', '├', '┼', '┼', '┼'],
        ['┼', '┤', '┼', '┤', '┼', '┤', '┼', '┤', '┼', '┼'],
        ['┬', '┼', '┬', '┬', '┼', '┼', '┼', '┼', '┬', '┼'],
        ['┴', '┼', '┼', '┼', '┴', '┴', '┼', '┼', '┼', '┴'],
    ];
    let index = |c: char| ROW_CHARS.iter().position(|&r| r == c);
    match (index(existing), index(incoming)) {
        (Some(a), Some(b)) if a != b => TABLE[a][b],
        _ => existing,
    }
}

/// Cell storage the drawing primitives write through: a dense [`Canvas`] or
/// a sparse [`Layer`].
pub trait CellSurface {
    /// The cell at `(x, y)`; `None` outside the surface.
    fn cell(&self, x: i32, y: i32) -> Option<&Cell>;
    /// Write a cell; writes outside the surface are dropped.
    fn put(&mut self, x: i32, y: i32, cell: Cell);
    /// Grow to at least `width × height`, preserving content.
    fn grow(&mut self, width: i32, height: i32);
}

impl CellSurface for Canvas {
    #[inline]
    fn cell(&self, x: i32, y: i32) -> Option<&Cell> {
        self.get(x, y)
    }

    #[inline]
    fn put(&mut self, x: i32, y: i32, cell: Cell) {
        self.set(x, y, cell);
    }

    fn grow(&mut self, width: i32, height: i32) {
        self.ensure_size(width, height);
    }
}

/// [`Canvas::write_cell`] on any surface.
fn write_cell_on(surface: &mut impl CellSurface, x: i32, y: i32, cell: Cell) {
    let Some(current) = surface.cell(x, y) else {
        return;
    };
    if current.is_wide_pad() && x > 0 && !cell.is_wide_pad() {
        surface.put(x - 1, y, Cell::SPACE);
    } else if !current.is_wide_pad()
        && surface.cell(x + 1, y).is_some_and(Cell::is_wide_pad)
        && cell != *current
    {
        surface.put(x + 1, y, Cell::SPACE);
    }
    surface.put(x, y, cell);
}

/// Whether `(x, y)` holds a plain space; outside the surface counts as
/// occupied.
fn is_space_on(surface: &impl CellSurface, x: i32, y: i32) -> bool {
    surface.cell(x, y).is_some_and(Cell::is_space)
}

/// [`Canvas::draw_text`] on any surface.
fn draw_text_on(surface: &mut impl CellSurface, start: DrawingCoord, text: &str, force: bool) {
    let cells = to_cells(text);
    surface.grow(start.x + cells.len() as i32 + 1, start.y + 1);
    for (i, cell) in cells.iter().enumerate() {
        if cell.is_wide_pad() {
            continue;
        }
        let x = start.x + i as i32;
        let y = start.y;
        if cells.get(i + 1).is_some_and(Cell::is_wide_pad) {
            let pair_free = is_space_on(surface, x, y) && is_space_on(surface, x + 1, y);
            if force || pair_free {
                write_cell_on(surface, x, y, cell.clone());
                write_cell_on(surface, x + 1, y, Cell::WIDE_PAD);
            }
        } else if force || is_space_on(surface, x, y) {
            write_cell_on(surface, x, y, cell.clone());
        }
    }
}

/// The cells one drawing step writes over a canvas, kept sparse.
///
/// Reads and writes follow [`Canvas`] semantics over the logical
/// `width × height` extent, absent cells reading as spaces, so
/// [`Canvas::overlay_layers`] composites a layer exactly as
/// [`Canvas::merge`] composites the full canvas it stands in for, without
/// allocating and scanning a canvas per edge and layer kind.
#[derive(Debug)]
pub struct Layer {
    width: i32,
    height: i32,
    /// Keyed `(x, y)`: iteration is column-major, the order
    /// [`Canvas::merge`] visits cells in.
    cells: BTreeMap<(i32, i32), Cell>,
    /// What absent cells read as.
    blank: Cell,
}

impl Layer {
    /// An empty layer with the extent of `canvas`.
    pub const fn over(canvas: &Canvas) -> Self {
        Self {
            width: canvas.width,
            height: canvas.height,
            cells: BTreeMap::new(),
            blank: Cell::SPACE,
        }
    }

    /// Write a cell; writes outside the layer's extent are dropped.
    pub fn set(&mut self, x: i32, y: i32, cell: Cell) {
        if x >= 0 && y >= 0 && x < self.width && y < self.height {
            self.cells.insert((x, y), cell);
        }
    }

    /// Draw `text` as [`Canvas::draw_text`] does.
    pub fn draw_text(&mut self, start: DrawingCoord, text: &str, force: bool) {
        draw_text_on(self, start, text, force);
    }

    /// The written cells, in column-major order.
    pub fn cells(&self) -> impl Iterator<Item = (i32, i32, &Cell)> {
        self.cells.iter().map(|(&(x, y), cell)| (x, y, cell))
    }
}

impl CellSurface for Layer {
    fn cell(&self, x: i32, y: i32) -> Option<&Cell> {
        (x >= 0 && y >= 0 && x < self.width && y < self.height)
            .then(|| self.cells.get(&(x, y)).unwrap_or(&self.blank))
    }

    fn put(&mut self, x: i32, y: i32, cell: Cell) {
        self.set(x, y, cell);
    }

    fn grow(&mut self, width: i32, height: i32) {
        self.width = self.width.max(width);
        self.height = self.height.max(height);
    }
}

impl Canvas {
    /// Write one cell, dissolving any wide-glyph pair the write would split:
    /// overwriting a pad orphans its lead and overwriting a lead orphans its
    /// pad, so the orphaned half becomes a space.
    pub fn write_cell(&mut self, x: i32, y: i32, cell: Cell) {
        write_cell_on(self, x, y, cell);
    }

    /// Write `text` starting at `(x, y)`, growing the canvas to fit `text`
    /// without adding trailing padding.
    pub fn write_str(&mut self, x: i32, y: i32, text: &str) {
        if x < 0 || y < 0 {
            return;
        }
        let cells = to_cells(text);
        self.ensure_size(x + cells.len() as i32, y + 1);
        for (i, cell) in cells.iter().enumerate() {
            if cell.is_wide_pad() {
                continue;
            }
            let cx = x + i as i32;
            if cells.get(i + 1).is_some_and(Cell::is_wide_pad) {
                self.write_cell(cx, y, cell.clone());
                self.write_cell(cx + 1, y, Cell::WIDE_PAD);
            } else {
                self.write_cell(cx, y, cell.clone());
            }
        }
    }

    /// Whether the cell at `(x, y)` is a plain space (out of range counts as
    /// occupied).
    #[inline]
    pub fn is_space_at(&self, x: i32, y: i32) -> bool {
        is_space_on(self, x, y)
    }

    /// Overlay canvases at `offset`, growing to fit them. Spaces are
    /// transparent; overlapping Unicode junction characters merge (unless
    /// `use_ascii`); label content never overwrites existing label content
    /// (first label wins); wide glyphs land or yield as a whole pair.
    pub fn merge(&mut self, offset: DrawingCoord, use_ascii: bool, overlays: &[&Self]) {
        self.grow_for_overlays(offset, overlays);
        for overlay in overlays {
            for x in 0..overlay.width {
                for y in 0..overlay.height {
                    let cell = &overlay.cells[(x * overlay.height + y) as usize];
                    if cell.is_space() || cell.is_wide_pad() {
                        continue;
                    }
                    let is_wide = overlay.get(x + 1, y).is_some_and(Cell::is_wide_pad);
                    self.overlay_cell(x + offset.x, y + offset.y, cell, is_wide, use_ascii);
                }
            }
        }
    }

    /// Overlay `layers` in order at the origin, under the rules of
    /// [`Canvas::merge`].
    pub fn overlay_layers(&mut self, use_ascii: bool, layers: &[Layer]) {
        let width = layers
            .iter()
            .fold(self.width, |width, layer| width.max(layer.width));
        let height = layers
            .iter()
            .fold(self.height, |height, layer| height.max(layer.height));
        self.ensure_size(width, height);
        for layer in layers {
            for (&(x, y), cell) in &layer.cells {
                if cell.is_space() || cell.is_wide_pad() {
                    continue;
                }
                let is_wide = layer.cells.get(&(x + 1, y)).is_some_and(Cell::is_wide_pad);
                self.overlay_cell(x, y, cell, is_wide, use_ascii);
            }
        }
    }

    /// Apply one non-space overlay cell at `(x, y)`; `is_wide` says the
    /// overlay holds its wide-glyph pad next.
    fn overlay_cell(&mut self, x: i32, y: i32, cell: &Cell, is_wide: bool, use_ascii: bool) {
        let Some(current) = self.get(x, y) else {
            return;
        };
        let junction = match (use_ascii, current.as_char(), cell.as_char()) {
            (false, Some(a), Some(b)) if is_junction_char(a) && is_junction_char(b) => Some((a, b)),
            _ => None,
        };
        if let Some((a, b)) = junction {
            self.set(x, y, Cell::from(merge_junctions(a, b)));
        } else if is_wide {
            let next_is_label = self.get(x + 1, y).is_some_and(Cell::is_label);
            if !current.is_label() && !next_is_label {
                self.write_cell(x, y, cell.clone());
                self.write_cell(x + 1, y, Cell::WIDE_PAD);
            }
        } else if current.is_label() && cell.is_label() {
            // First label wins.
        } else {
            self.write_cell(x, y, cell.clone());
        }
    }

    /// Draw `text` starting at `start`, growing the canvas to fit. Existing
    /// non-space cells are preserved unless `force`; wide glyphs need both of
    /// their cells free to land.
    pub fn draw_text(&mut self, start: DrawingCoord, text: &str, force: bool) {
        draw_text_on(self, start, text, force);
    }

    /// Draw multi-line text centered on `(cx, cy)`: lines spread evenly
    /// around `cy`, each centered horizontally on its own width.
    pub fn draw_multiline_centered(&mut self, label: &str, cx: i32, cy: i32) {
        let lines: Vec<&str> = split_lines(label).collect();
        let start_y = cy - (lines.len() as i32 - 1) / 2;
        for (i, line) in lines.iter().enumerate() {
            let start_x = cx - display_width(line) as i32 / 2;
            self.draw_text(DrawingCoord::new(start_x, start_y + i as i32), line, true);
        }
    }

    /// Draw multi-line text left-aligned from `(x, y)`, one row per line.
    pub fn draw_multiline_left(&mut self, label: &str, x: i32, y: i32) {
        for (i, line) in split_lines(label).enumerate() {
            self.draw_text(DrawingCoord::new(x, y + i as i32), line, true);
        }
    }

    /// Flip vertically and remap direction-bearing glyphs (arrows, corners,
    /// T-junctions) so a TD layout reads as BT.
    pub fn flip_vertical_remapped(&mut self) {
        self.flip_vertical();
        for cell in &mut self.cells {
            let Some(c) = cell.as_char() else { continue };
            let flipped = match c {
                '▲' => '▼',
                '▼' => '▲',
                '◤' => '◣',
                '◣' => '◤',
                '◥' => '◢',
                '◢' => '◥',
                '^' => 'v',
                'v' => '^',
                '┌' => '└',
                '└' => '┌',
                '┐' => '┘',
                '┘' => '┐',
                '╭' => '╰',
                '╰' => '╭',
                '╮' => '╯',
                '╯' => '╮',
                '┬' => '┴',
                '┴' => '┬',
                '╵' => '╷',
                '╷' => '╵',
                _ => continue,
            };
            *cell = Cell::from(flipped);
        }
    }

    /// Serialize row by row with optional role-based ANSI/HTML coloring.
    /// Wide-glyph pads are dropped and label spaces restored; with a role
    /// canvas and a color mode other than `None`, each row is colorized by role.
    pub fn render_with_roles(
        &self,
        roles: Option<&RoleCanvas>,
        mode: ColorMode,
        theme: &Theme,
    ) -> String {
        let mut out = String::with_capacity((self.width as usize + 1) * self.height as usize);
        let mut row: Vec<RoleCell<'_>> = Vec::with_capacity(self.width as usize);
        for y in 0..self.height {
            if y > 0 {
                out.push('\n');
            }
            match roles.filter(|_| mode != ColorMode::None) {
                None => {
                    for x in 0..self.width {
                        let cell = &self.cells[(x * self.height + y) as usize];
                        if cell.is_wide_pad() {
                            continue;
                        }
                        out.push_str(if *cell == LABEL_SPACE {
                            " "
                        } else {
                            cell.as_str()
                        });
                    }
                }
                Some(roles) => {
                    row.clear();
                    for x in 0..self.width {
                        let cell = &self.cells[(x * self.height + y) as usize];
                        if cell.is_wide_pad() {
                            continue;
                        }
                        let text = if *cell == LABEL_SPACE {
                            " "
                        } else {
                            cell.as_str()
                        };
                        row.push((text, roles.get(x, y).copied().flatten()));
                    }
                    out.push_str(&colorize_line(&row, theme, mode));
                }
            }
        }
        out
    }

    /// Plain-text serialization (no colors).
    pub fn render(&self) -> String {
        self.render_with_roles(None, ColorMode::None, &Theme::default())
    }

    /// Plain-text serialization (no colors).
    pub fn to_plain_string(&self) -> String {
        self.render()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn merge_resolves_junctions_and_keeps_first_label() {
        let mut base = Canvas::new(3, 1);
        base.draw_text(DrawingCoord::new(0, 0), "─A─", true);
        let mut overlay = Canvas::new(3, 1);
        overlay.draw_text(DrawingCoord::new(0, 0), "│B ", true);
        let mut merged = base.clone();
        merged.merge(DrawingCoord::new(0, 0), false, &[&overlay]);
        // draw_text grows one column past the text, as the reference does.
        assert_eq!(merged.to_plain_string(), "┼A─ ");
        base.merge(DrawingCoord::new(0, 0), true, &[&overlay]);
        assert_eq!(base.to_plain_string(), "│A─ ");
    }

    #[test]
    fn wide_glyphs_stay_paired_and_serialize_to_one_column_each() {
        let mut canvas = Canvas::new(1, 1);
        canvas.draw_text(DrawingCoord::new(0, 0), "日x", true);
        assert_eq!(canvas.width(), 4);
        assert!(canvas.get(1, 0).unwrap().is_wide_pad());
        assert_eq!(canvas.to_plain_string(), "日x ");
        canvas.write_cell(1, 0, Cell::from('-'));
        assert_eq!(canvas.to_plain_string(), " -x ");
    }

    #[test]
    fn flip_remaps_direction_glyphs() {
        let mut canvas = Canvas::new(1, 3);
        canvas.set(0, 0, Cell::from('▼'));
        canvas.set(0, 1, Cell::from('│'));
        canvas.set(0, 2, Cell::from('┘'));
        canvas.flip_vertical_remapped();
        assert_eq!(canvas.to_plain_string(), "┐\n│\n▲");
    }

    #[test]
    fn role_canvas_grows_on_set() {
        let mut roles = RoleCanvas::new(1, 1);
        roles.set_role(3, 2, CharRole::Text);
        assert_eq!((roles.width(), roles.height()), (4, 3));
        assert_eq!(roles.get(3, 2), Some(&Some(CharRole::Text)));
    }
}

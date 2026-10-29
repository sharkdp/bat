pub mod colors;
pub mod parser;

use colors::{get_series_color, CHART_ACCENT_FALLBACK};
use parser::parse_xy_chart;

use super::{
    ansi::{colorize_text, CharRole, ColorMode, Theme},
    canvas::{to_cells, Canvas, Cell, Grid, RoleCanvas},
    flowchart::AsciiConfig,
    text::display_width,
};

const PLOT_WIDTH: i32 = 60;
const PLOT_HEIGHT: i32 = 20;

/// Inclusive numeric bounds for an axis.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct AxisRange {
    /// Lower axis bound.
    pub min: f64,
    /// Upper axis bound.
    pub max: f64,
}

/// Categorical or numeric axis configuration.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct XyAxis {
    /// Optional axis title.
    pub title: Option<String>,
    /// Optional categorical labels.
    pub categories: Option<Vec<String>>,
    /// Optional numeric bounds.
    pub range: Option<AxisRange>,
}

/// Kind of data series drawn on an XY chart.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SeriesType {
    /// Filled bars.
    Bar,
    /// Staircase line.
    Line,
}

/// One bar or line data series.
#[derive(Clone, Debug, PartialEq)]
pub struct XyChartSeries {
    /// Rendering kind of this series.
    pub kind: SeriesType,
    /// Values in category order.
    pub data: Vec<f64>,
}

/// Parsed Mermaid XY chart.
#[derive(Clone, Debug, PartialEq)]
pub struct XyChart {
    /// Optional centered chart title.
    pub title: Option<String>,
    /// Whether axes are rotated into horizontal orientation.
    pub horizontal: bool,
    /// Category or numeric x-axis.
    pub x_axis: XyAxis,
    /// Value y-axis.
    pub y_axis: XyAxis,
    /// Data series in source order.
    pub series: Vec<XyChartSeries>,
}

#[derive(Clone, Copy)]
struct ChartChars {
    h_line: char,
    v_line: char,
    origin: char,
    y_tick: char,
    x_tick: char,
    bar: char,
    grid: char,
    corner_tl: char,
    corner_tr: char,
    corner_bl: char,
    corner_br: char,
}

const UNICODE: ChartChars = ChartChars {
    h_line: '─',
    v_line: '│',
    origin: '┼',
    y_tick: '┤',
    x_tick: '┬',
    bar: '█',
    grid: '·',
    corner_tl: '╭',
    corner_tr: '╮',
    corner_bl: '╰',
    corner_br: '╯',
};
const ASCII: ChartChars = ChartChars {
    h_line: '-',
    v_line: '|',
    origin: '+',
    y_tick: '+',
    x_tick: '+',
    bar: '#',
    grid: '.',
    corner_tl: '+',
    corner_tr: '+',
    corner_bl: '+',
    corner_br: '+',
};

type HexCanvas = Grid<Option<String>>;

/// Render Mermaid `xychart-beta` source as an ASCII or Unicode chart.
pub fn render(text: &str, config: &AsciiConfig, mode: ColorMode, theme: &Theme) -> String {
    let chart = parse_xy_chart(text);
    let chars = if config.use_ascii { ASCII } else { UNICODE };
    if chart.horizontal {
        render_horizontal(&chart, chars, mode, theme)
    } else {
        render_vertical(&chart, chars, mode, theme)
    }
}

fn render_vertical(chart: &XyChart, chars: ChartChars, mode: ColorMode, theme: &Theme) -> String {
    let data_count = data_count(chart);
    if data_count == 0 {
        return String::new();
    }
    let y_range = chart.y_axis.range.unwrap_or(AxisRange {
        min: 0.0,
        max: 100.0,
    });
    let y_ticks = nice_tick_values(y_range.min, y_range.max);
    let y_labels: Vec<_> = y_ticks
        .iter()
        .map(|&value| format_tick_value(value))
        .collect();
    let y_gutter = y_labels
        .iter()
        .map(|label| display_width(label) as i32)
        .max()
        .unwrap_or(0)
        + 1;
    let plot_width = PLOT_WIDTH.max(data_count * 6);
    let plot_height = PLOT_HEIGHT;
    let band_width = plot_width / data_count;
    let category_labels = category_labels(chart, data_count);
    let has_title = chart.title.is_some();
    let has_x_title = chart.x_axis.title.is_some();
    let has_legend = chart.series.len() > 1;
    let title_row = if has_title { 0 } else { -1 };
    let plot_top = if has_title { 2 } else { 0 } + i32::from(has_legend);
    let plot_left = y_gutter + 1;
    let total_width = plot_left + band_width * data_count + 2;
    let x_axis_row = plot_top + plot_height;
    let x_label_row = x_axis_row + 1;
    let x_title_row = if has_x_title { x_label_row + 1 } else { -1 };
    let total_height = x_label_row + 1 + i32::from(has_x_title);
    let mut canvas = Canvas::new(total_width, total_height);
    let mut roles = RoleCanvas::new(total_width, total_height);
    let mut hex_colors = HexCanvas::new(total_width, total_height);
    let series_colors = series_colors(chart.series.len(), theme);
    let value_to_row = |value: f64| {
        let denominator = y_range.max - y_range.min;
        let t = (value - y_range.min) / if denominator == 0.0 { 1.0 } else { denominator };
        js_round_i32(t * f64::from(plot_height - 1))
    };
    let band_center = |index: i32| {
        plot_left + ((f64::from(band_width) * (f64::from(index) + 0.5)).floor() as i32)
    };

    if let Some(title) = chart.title.as_deref().filter(|_| title_row >= 0) {
        write_text(
            &mut canvas,
            &mut roles,
            title_row,
            centered_start(total_width, title),
            title,
            CharRole::Text,
        );
    }
    if has_legend {
        draw_legend(
            &mut canvas,
            &mut roles,
            &mut hex_colors,
            chart,
            i32::from(has_title),
            total_width,
            chars,
            &series_colors,
        );
    }
    for row in 0..plot_height {
        let display_row = plot_top + plot_height - 1 - row;
        set(
            &mut canvas,
            &mut roles,
            display_row,
            plot_left - 1,
            chars.v_line,
            CharRole::Border,
            None,
            None,
        );
    }
    set(
        &mut canvas,
        &mut roles,
        x_axis_row,
        plot_left - 1,
        chars.origin,
        CharRole::Border,
        None,
        None,
    );
    for (&tick, label) in y_ticks.iter().zip(&y_labels) {
        let row = value_to_row(tick);
        if !(0..plot_height).contains(&row) {
            continue;
        }
        let display_row = plot_top + plot_height - 1 - row;
        set(
            &mut canvas,
            &mut roles,
            display_row,
            plot_left - 1,
            if row == 0 { chars.origin } else { chars.y_tick },
            CharRole::Border,
            None,
            None,
        );
        write_text(
            &mut canvas,
            &mut roles,
            display_row,
            (y_gutter - display_width(label) as i32).max(0),
            label,
            CharRole::Text,
        );
    }
    for column in plot_left..plot_left + band_width * data_count {
        set(
            &mut canvas,
            &mut roles,
            x_axis_row,
            column,
            chars.h_line,
            CharRole::Border,
            None,
            None,
        );
    }
    for index in 0..data_count {
        let center = band_center(index);
        set(
            &mut canvas,
            &mut roles,
            x_axis_row,
            center,
            chars.x_tick,
            CharRole::Border,
            None,
            None,
        );
        let label = category_labels
            .get(index as usize)
            .map_or("", String::as_str);
        write_text(
            &mut canvas,
            &mut roles,
            x_label_row,
            (center - display_width(label) as i32 / 2).max(0),
            label,
            CharRole::Text,
        );
    }
    if let Some(title) = chart.x_axis.title.as_deref().filter(|_| x_title_row >= 0) {
        write_text(
            &mut canvas,
            &mut roles,
            x_title_row,
            centered_start(total_width, title),
            title,
            CharRole::Text,
        );
    }
    for &tick in &y_ticks {
        let row = value_to_row(tick);
        if !(0..plot_height).contains(&row) {
            continue;
        }
        let display_row = plot_top + plot_height - 1 - row;
        for column in plot_left..plot_left + band_width * data_count {
            if get(&canvas, display_row, column) == ' ' {
                set(
                    &mut canvas,
                    &mut roles,
                    display_row,
                    column,
                    chars.grid,
                    CharRole::Line,
                    None,
                    None,
                );
            }
        }
    }
    let bars: Vec<_> = chart
        .series
        .iter()
        .enumerate()
        .filter(|(_, series)| series.kind == SeriesType::Bar)
        .collect();
    if !bars.is_empty() {
        let bar_count = bars.len() as i32;
        let usable = (band_width - 2).max(1);
        let single_width = (usable / bar_count).clamp(1, 8);
        let group_width = single_width * bar_count + bar_count - 1;
        let base_row = value_to_row(y_range.min.max(0.0));
        for (bar_index, (global_index, series)) in bars.iter().enumerate() {
            let color = &series_colors[*global_index];
            for (index, &value) in series.data.iter().enumerate() {
                let center = band_center(index as i32);
                let group_left = center - group_width / 2;
                let bar_x = group_left + bar_index as i32 * (single_width + 1);
                let value_row = value_to_row(value);
                for row in base_row.min(value_row)..=base_row.max(value_row) {
                    let display_row = plot_top + plot_height - 1 - row;
                    for column in bar_x..bar_x + single_width {
                        set(
                            &mut canvas,
                            &mut roles,
                            display_row,
                            column,
                            chars.bar,
                            CharRole::Arrow,
                            Some(&mut hex_colors),
                            Some(color),
                        );
                    }
                }
            }
        }
    }
    for (global_index, series) in chart
        .series
        .iter()
        .enumerate()
        .filter(|(_, series)| series.kind == SeriesType::Line)
    {
        if !series.data.is_empty() {
            draw_staircase_line(
                &mut canvas,
                &mut roles,
                &mut hex_colors,
                &series.data,
                &band_center,
                &value_to_row,
                plot_top,
                plot_height,
                plot_left,
                band_width * data_count,
                chars,
                &series_colors[global_index],
            );
        }
    }
    canvas_to_string(&canvas, &roles, &hex_colors, mode, theme)
}

fn render_horizontal(chart: &XyChart, chars: ChartChars, mode: ColorMode, theme: &Theme) -> String {
    let data_count = data_count(chart);
    if data_count == 0 {
        return String::new();
    }
    let y_range = chart.y_axis.range.unwrap_or(AxisRange {
        min: 0.0,
        max: 100.0,
    });
    let value_ticks = nice_tick_values(y_range.min, y_range.max);
    let category_labels = category_labels(chart, data_count);
    let category_gutter = category_labels
        .iter()
        .map(|label| display_width(label) as i32)
        .max()
        .unwrap_or(0)
        + 1;
    let plot_width = PLOT_WIDTH.max(40);
    let band_height = (PLOT_HEIGHT / data_count).max(2);
    let plot_height = band_height * data_count;
    let has_title = chart.title.is_some();
    let has_y_title = chart.y_axis.title.is_some();
    let has_legend = chart.series.len() > 1;
    let plot_top = if has_title { 2 } else { 0 } + i32::from(has_legend);
    let plot_left = category_gutter + 1;
    let total_width = plot_left + plot_width + 2;
    let total_height = plot_top + plot_height + 2 + i32::from(has_y_title);
    let x_axis_row = plot_top + plot_height;
    let mut canvas = Canvas::new(total_width, total_height);
    let mut roles = RoleCanvas::new(total_width, total_height);
    let mut hex_colors = HexCanvas::new(total_width, total_height);
    let series_colors = series_colors(chart.series.len(), theme);
    let value_to_column = |value: f64| {
        let denominator = y_range.max - y_range.min;
        let t = (value - y_range.min) / if denominator == 0.0 { 1.0 } else { denominator };
        plot_left + js_round_i32(t * f64::from(plot_width - 1))
    };
    let band_middle =
        |index: i32| plot_top + (f64::from(band_height) * (f64::from(index) + 0.5)).floor() as i32;
    if let Some(title) = chart.title.as_deref() {
        write_text(
            &mut canvas,
            &mut roles,
            0,
            centered_start(total_width, title),
            title,
            CharRole::Text,
        );
    }
    if has_legend {
        draw_legend(
            &mut canvas,
            &mut roles,
            &mut hex_colors,
            chart,
            i32::from(has_title),
            total_width,
            chars,
            &series_colors,
        );
    }
    for row in plot_top..plot_top + plot_height {
        set(
            &mut canvas,
            &mut roles,
            row,
            plot_left - 1,
            chars.v_line,
            CharRole::Border,
            None,
            None,
        );
    }
    set(
        &mut canvas,
        &mut roles,
        x_axis_row,
        plot_left - 1,
        chars.origin,
        CharRole::Border,
        None,
        None,
    );
    for index in 0..data_count {
        let middle = band_middle(index);
        let label = category_labels
            .get(index as usize)
            .map_or("", String::as_str);
        write_text(
            &mut canvas,
            &mut roles,
            middle,
            (category_gutter - display_width(label) as i32).max(0),
            label,
            CharRole::Text,
        );
    }
    for column in plot_left..plot_left + plot_width {
        set(
            &mut canvas,
            &mut roles,
            x_axis_row,
            column,
            chars.h_line,
            CharRole::Border,
            None,
            None,
        );
    }
    for &tick in &value_ticks {
        let column = value_to_column(tick);
        if !(plot_left..plot_left + plot_width).contains(&column) {
            continue;
        }
        set(
            &mut canvas,
            &mut roles,
            x_axis_row,
            column,
            chars.x_tick,
            CharRole::Border,
            None,
            None,
        );
        let label = format_tick_value(tick);
        write_text(
            &mut canvas,
            &mut roles,
            x_axis_row + 1,
            column - display_width(&label) as i32 / 2,
            &label,
            CharRole::Text,
        );
    }
    if let Some(title) = chart.y_axis.title.as_deref() {
        write_text(
            &mut canvas,
            &mut roles,
            total_height - 1,
            centered_start(total_width, title),
            title,
            CharRole::Text,
        );
    }
    for &tick in &value_ticks {
        let column = value_to_column(tick);
        if !(plot_left..plot_left + plot_width).contains(&column) {
            continue;
        }
        for row in plot_top..plot_top + plot_height {
            if get(&canvas, row, column) == ' ' {
                set(
                    &mut canvas,
                    &mut roles,
                    row,
                    column,
                    chars.grid,
                    CharRole::Line,
                    None,
                    None,
                );
            }
        }
    }
    let bars: Vec<_> = chart
        .series
        .iter()
        .enumerate()
        .filter(|(_, series)| series.kind == SeriesType::Bar)
        .collect();
    if !bars.is_empty() {
        let bar_count = bars.len() as i32;
        let group_height = bar_count + bar_count - 1;
        let base_column = value_to_column(y_range.min.max(0.0));
        for (bar_index, (global_index, series)) in bars.iter().enumerate() {
            let color = &series_colors[*global_index];
            for (index, &value) in series.data.iter().enumerate() {
                let middle = band_middle(index as i32);
                let group_top = middle - group_height / 2;
                let bar_y = group_top + bar_index as i32 * 2;
                let value_column = value_to_column(value);
                for column in base_column.min(value_column)..=base_column.max(value_column) {
                    set(
                        &mut canvas,
                        &mut roles,
                        bar_y,
                        column,
                        chars.bar,
                        CharRole::Arrow,
                        Some(&mut hex_colors),
                        Some(color),
                    );
                }
            }
        }
    }
    for (global_index, series) in chart
        .series
        .iter()
        .enumerate()
        .filter(|(_, series)| series.kind == SeriesType::Line)
    {
        if !series.data.is_empty() {
            draw_horizontal_staircase_line(
                &mut canvas,
                &mut roles,
                &mut hex_colors,
                &series.data,
                &band_middle,
                &value_to_column,
                plot_top,
                plot_height,
                plot_left,
                plot_width,
                chars,
                &series_colors[global_index],
            );
        }
    }
    canvas_to_string(&canvas, &roles, &hex_colors, mode, theme)
}

#[allow(clippy::too_many_arguments)]
fn draw_staircase_line(
    canvas: &mut Canvas,
    roles: &mut RoleCanvas,
    hex_canvas: &mut HexCanvas,
    data: &[f64],
    band_center: &impl Fn(i32) -> i32,
    value_to_row: &impl Fn(f64) -> i32,
    plot_top: i32,
    plot_height: i32,
    plot_left: i32,
    plot_width: i32,
    chars: ChartChars,
    color: &str,
) {
    let points: Vec<_> = data
        .iter()
        .enumerate()
        .map(|(index, &value)| (band_center(index as i32), value_to_row(value)))
        .collect();
    let mut draw_at = |column: i32, row: i32, character: char| {
        let display_row = plot_top + plot_height - 1 - row;
        if display_row >= 0 && (plot_left..plot_left + plot_width).contains(&column) {
            set(
                canvas,
                roles,
                display_row,
                column,
                character,
                CharRole::Arrow,
                Some(hex_canvas),
                Some(color),
            );
        }
    };
    if let [point] = points.as_slice() {
        draw_at(point.0, point.1, chars.h_line);
        return;
    }
    for index in 0..points.len().saturating_sub(1) {
        let first = points[index];
        let second = points[index + 1];
        if first.1 == second.1 {
            for column in first.0..=second.0 {
                draw_at(column, first.1, chars.h_line);
            }
            continue;
        }
        let middle = js_round_i32(f64::from(first.0 + second.0) / 2.0);
        let going_up = second.1 > first.1;
        for column in first.0..middle {
            draw_at(column, first.1, chars.h_line);
        }
        draw_at(
            middle,
            first.1,
            if going_up {
                chars.corner_br
            } else {
                chars.corner_tr
            },
        );
        for row in first.1.min(second.1) + 1..first.1.max(second.1) {
            draw_at(middle, row, chars.v_line);
        }
        draw_at(
            middle,
            second.1,
            if going_up {
                chars.corner_tl
            } else {
                chars.corner_bl
            },
        );
        for column in middle + 1..=second.0 {
            draw_at(column, second.1, chars.h_line);
        }
        if index == 0 {
            let lead_start = plot_left.max(first.0 - (second.0 - first.0) / 4);
            for column in lead_start..first.0 {
                draw_at(column, first.1, chars.h_line);
            }
        }
        if index == points.len() - 2 {
            let trail_end = (plot_left + plot_width - 1).min(second.0 + (second.0 - first.0) / 4);
            for column in second.0 + 1..=trail_end {
                draw_at(column, second.1, chars.h_line);
            }
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn draw_horizontal_staircase_line(
    canvas: &mut Canvas,
    roles: &mut RoleCanvas,
    hex_canvas: &mut HexCanvas,
    data: &[f64],
    band_middle: &impl Fn(i32) -> i32,
    value_to_column: &impl Fn(f64) -> i32,
    plot_top: i32,
    plot_height: i32,
    plot_left: i32,
    plot_width: i32,
    chars: ChartChars,
    color: &str,
) {
    let points: Vec<_> = data
        .iter()
        .enumerate()
        .map(|(index, &value)| (band_middle(index as i32), value_to_column(value)))
        .collect();
    let mut draw_at = |row: i32, column: i32, character: char| {
        if (plot_top..plot_top + plot_height).contains(&row)
            && (plot_left..plot_left + plot_width).contains(&column)
        {
            set(
                canvas,
                roles,
                row,
                column,
                character,
                CharRole::Arrow,
                Some(hex_canvas),
                Some(color),
            );
        }
    };
    if let [point] = points.as_slice() {
        draw_at(point.0, point.1, chars.v_line);
        return;
    }
    for index in 0..points.len().saturating_sub(1) {
        let first = points[index];
        let second = points[index + 1];
        if first.1 == second.1 {
            for row in first.0..=second.0 {
                draw_at(row, first.1, chars.v_line);
            }
            continue;
        }
        let middle = js_round_i32(f64::from(first.0 + second.0) / 2.0);
        let going_right = second.1 > first.1;
        for row in first.0..middle {
            draw_at(row, first.1, chars.v_line);
        }
        draw_at(
            middle,
            first.1,
            if going_right {
                chars.corner_bl
            } else {
                chars.corner_br
            },
        );
        for column in first.1.min(second.1) + 1..first.1.max(second.1) {
            draw_at(middle, column, chars.h_line);
        }
        draw_at(
            middle,
            second.1,
            if going_right {
                chars.corner_tr
            } else {
                chars.corner_tl
            },
        );
        for row in middle + 1..=second.0 {
            draw_at(row, second.1, chars.v_line);
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn draw_legend(
    canvas: &mut Canvas,
    roles: &mut RoleCanvas,
    hex_canvas: &mut HexCanvas,
    chart: &XyChart,
    row: i32,
    total_width: i32,
    chars: ChartChars,
    colors: &[String],
) {
    let mut bar_index = 0;
    let mut line_index = 0;
    let items: Vec<_> = chart
        .series
        .iter()
        .enumerate()
        .map(|(global_index, series)| match series.kind {
            SeriesType::Bar => {
                bar_index += 1;
                (chars.bar, format!("Bar {bar_index}"), global_index)
            }
            SeriesType::Line => {
                line_index += 1;
                (chars.h_line, format!("Line {line_index}"), global_index)
            }
        })
        .collect();
    let total_length = items
        .iter()
        .enumerate()
        .fold(0, |width, (index, (_, label, _))| {
            width + i32::from(index > 0) * 2 + 2 + display_width(label) as i32
        });
    let mut column =
        ((f64::from(total_width) / 2.0 - f64::from(total_length) / 2.0).floor() as i32).max(0);
    for (index, (symbol, label, global_index)) in items.iter().enumerate() {
        if index > 0 {
            column += 2;
        }
        set(
            canvas,
            roles,
            row,
            column,
            *symbol,
            CharRole::Arrow,
            Some(hex_canvas),
            colors.get(*global_index).map(String::as_str),
        );
        column += 2;
        write_text(canvas, roles, row, column, label, CharRole::Text);
        column += display_width(label) as i32;
    }
}

#[allow(clippy::too_many_arguments)]
fn set(
    canvas: &mut Canvas,
    roles: &mut RoleCanvas,
    row: i32,
    column: i32,
    character: char,
    role: CharRole,
    hex_canvas: Option<&mut HexCanvas>,
    hex: Option<&str>,
) {
    if canvas.in_bounds(column, row) {
        canvas.set(column, row, Cell::from(character));
        roles.set(column, row, Some(role));
        if let (Some(hex_canvas), Some(hex)) = (hex_canvas, hex) {
            hex_canvas.set(column, row, Some(hex.to_owned()));
        }
    }
}

fn get(canvas: &Canvas, row: i32, column: i32) -> char {
    canvas
        .get(column, row)
        .and_then(Cell::as_char)
        .unwrap_or(' ')
}

fn write_text(
    canvas: &mut Canvas,
    roles: &mut RoleCanvas,
    row: i32,
    start_column: i32,
    text: &str,
    role: CharRole,
) {
    let cells = to_cells(text);
    for (index, cell) in cells.iter().enumerate() {
        if cell.is_wide_pad() {
            continue;
        }
        let column = start_column + index as i32;
        let wide = cells.get(index + 1).is_some_and(Cell::is_wide_pad);
        if column < 0 || column + i32::from(wide) >= canvas.width() {
            continue;
        }
        canvas.set(column, row, cell.clone());
        roles.set(column, row, Some(role));
        if wide {
            canvas.set(column + 1, row, Cell::WIDE_PAD);
            roles.set(column + 1, row, Some(role));
        }
    }
}

fn canvas_to_string(
    canvas: &Canvas,
    roles: &RoleCanvas,
    hex_canvas: &HexCanvas,
    mode: ColorMode,
    theme: &Theme,
) -> String {
    if canvas.width() == 0 {
        return String::new();
    }
    let mut lines = Vec::with_capacity(canvas.height() as usize);
    for row in 0..canvas.height() {
        let mut cells = Vec::with_capacity(canvas.width() as usize);
        for column in 0..canvas.width() {
            let Some(cell) = canvas.get(column, row) else {
                continue;
            };
            if cell.is_wide_pad() {
                continue;
            }
            cells.push((
                cell.as_str(),
                roles.get(column, row).copied().flatten(),
                hex_canvas.get(column, row).and_then(Option::as_deref),
            ));
        }
        while cells.last().is_some_and(|(text, ..)| *text == " ") {
            cells.pop();
        }
        lines.push(colorize_row(&cells, theme, mode));
    }
    while lines.last().is_some_and(String::is_empty) {
        lines.pop();
    }
    lines.join("\n")
}

fn colorize_row(
    cells: &[(&str, Option<CharRole>, Option<&str>)],
    theme: &Theme,
    mode: ColorMode,
) -> String {
    if mode == ColorMode::None {
        return cells.iter().map(|(text, ..)| *text).collect();
    }
    let mut output = String::new();
    let mut current_color: Option<&str> = None;
    let mut buffer = String::new();
    let flush = |output: &mut String, buffer: &mut String, color: Option<&str>| {
        if buffer.is_empty() {
            return;
        }
        if let Some(color) = color {
            output.push_str(&colorize_text(buffer, color, mode));
        } else {
            output.push_str(buffer);
        }
        buffer.clear();
    };
    for &(text, role, override_color) in cells {
        if text == " " {
            flush(&mut output, &mut buffer, current_color);
            current_color = None;
            output.push(' ');
            continue;
        }
        let color = override_color.or_else(|| role.map(|role| theme.role_color(role)));
        if color == current_color {
            buffer.push_str(text);
        } else {
            flush(&mut output, &mut buffer, current_color);
            buffer.push_str(text);
            current_color = color;
        }
    }
    flush(&mut output, &mut buffer, current_color);
    output
}

fn series_colors(total: usize, theme: &Theme) -> Vec<String> {
    let accent = theme.accent.as_deref().unwrap_or(CHART_ACCENT_FALLBACK);
    if total <= 1 {
        return vec![accent.to_owned()];
    }
    (0..total)
        .map(|index| get_series_color(index, accent, theme.bg.as_deref()))
        .collect()
}

fn data_count(chart: &XyChart) -> i32 {
    if let Some(categories) = &chart.x_axis.categories {
        return categories.len() as i32;
    }
    chart
        .series
        .iter()
        .find(|series| !series.data.is_empty())
        .map_or(0, |series| series.data.len() as i32)
}

#[allow(clippy::suboptimal_flops)]
fn category_labels(chart: &XyChart, count: i32) -> Vec<String> {
    if let Some(categories) = &chart.x_axis.categories {
        return categories.clone();
    }
    if let Some(range) = chart.x_axis.range {
        let step = if count > 1 {
            (range.max - range.min) / f64::from(count - 1)
        } else {
            0.0
        };
        return (0..count)
            .map(|index| format_tick_value(range.min + step * f64::from(index)))
            .collect();
    }
    (0..count).map(|index| (index + 1).to_string()).collect()
}

#[allow(clippy::suboptimal_flops, clippy::while_float)]
fn nice_tick_values(minimum: f64, maximum: f64) -> Vec<f64> {
    let range = maximum - minimum;
    if range <= 0.0 {
        return vec![minimum];
    }
    let raw_interval = range / 6.0;
    let magnitude = 10_f64.powf(raw_interval.log10().floor());
    let residual = raw_interval / magnitude;
    let interval = if residual <= 1.5 {
        magnitude
    } else if residual <= 3.0 {
        2.0 * magnitude
    } else if residual <= 7.0 {
        5.0 * magnitude
    } else {
        10.0 * magnitude
    };
    let mut value = (minimum / interval).ceil() * interval;
    let mut ticks = Vec::new();
    while value <= maximum + interval * 0.001 {
        ticks.push((value * 1e10).round() / 1e10);
        // Past 2^53 an interval below the value's ULP no longer moves it; the
        // loop would grow `ticks` until memory ran out.
        let next = value + interval;
        if next <= value {
            break;
        }
        value = next;
    }
    ticks
}

fn format_tick_value(value: f64) -> String {
    if value.is_finite() && value.fract() == 0.0 {
        js_number_to_string(value)
    } else {
        js_to_fixed(value, usize::from(value.abs() < 10.0))
    }
}

fn js_number_to_string(value: f64) -> String {
    if value.is_nan() {
        return "NaN".to_owned();
    }
    if value == f64::INFINITY {
        return "Infinity".to_owned();
    }
    if value == f64::NEG_INFINITY {
        return "-Infinity".to_owned();
    }
    if value == 0.0 {
        return "0".to_owned();
    }
    let rendered = value.to_string();
    let absolute = value.abs();
    if (1e-6..1e21).contains(&absolute) {
        return expand_exponent(&rendered);
    }
    to_scientific(&rendered)
}

fn expand_exponent(rendered: &str) -> String {
    let Some((mantissa, exponent)) = rendered.split_once(['e', 'E']) else {
        return rendered.to_owned();
    };
    let exponent: i32 = exponent.parse().unwrap_or(0);
    let negative = mantissa.starts_with('-');
    let unsigned = mantissa.trim_start_matches('-');
    let point = unsigned.find('.').unwrap_or(unsigned.len()) as i32;
    let digits: String = unsigned
        .chars()
        .filter(|&character| character != '.')
        .collect();
    let new_point = point + exponent;
    let body = if new_point <= 0 {
        format!("0.{}{}", "0".repeat((-new_point) as usize), digits)
    } else if new_point as usize >= digits.len() {
        format!(
            "{}{}",
            digits,
            "0".repeat(new_point as usize - digits.len())
        )
    } else {
        format!(
            "{}.{}",
            &digits[..new_point as usize],
            &digits[new_point as usize..]
        )
    };
    if negative {
        format!("-{body}")
    } else {
        body
    }
}

fn to_scientific(rendered: &str) -> String {
    if let Some((mantissa, exponent)) = rendered.split_once(['e', 'E']) {
        let exponent: i32 = exponent.parse().unwrap_or(0);
        return format!(
            "{mantissa}e{}{exponent}",
            if exponent >= 0 { "+" } else { "" }
        );
    }
    let negative = rendered.starts_with('-');
    let unsigned = rendered.trim_start_matches('-');
    let point = unsigned.find('.').unwrap_or(unsigned.len());
    let digits: String = unsigned
        .chars()
        .filter(|&character| character != '.')
        .collect();
    let first = digits.find(|character| character != '0').unwrap_or(0);
    let exponent = point as i32 - first as i32 - 1;
    let significant = digits[first..].trim_end_matches('0');
    let mantissa = if significant.len() <= 1 {
        significant.to_owned()
    } else {
        format!("{}.{}", &significant[..1], &significant[1..])
    };
    format!(
        "{}{mantissa}e{}{exponent}",
        if negative { "-" } else { "" },
        if exponent >= 0 { "+" } else { "" }
    )
}

fn js_to_fixed(value: f64, digits: usize) -> String {
    if !value.is_finite() || value.abs() >= 1e21 {
        return js_number_to_string(value);
    }
    let negative = value < 0.0;
    let integer = exact_scaled_round(value.abs(), digits).map_or_else(
        || format!("{:.0}", (value.abs() * 10_f64.powi(digits as i32)).round()),
        |rounded| rounded.to_string(),
    );
    let body = if digits == 0 {
        integer
    } else if integer.len() <= digits {
        format!("0.{}{}", "0".repeat(digits - integer.len()), integer)
    } else {
        let split = integer.len() - digits;
        format!("{}.{}", &integer[..split], &integer[split..])
    };
    if negative {
        format!("-{body}")
    } else {
        body
    }
}

fn exact_scaled_round(value: f64, digits: usize) -> Option<u128> {
    let bits = value.to_bits();
    let exponent_bits = ((bits >> 52) & 0x7ff) as i32;
    let fraction = bits & ((1_u64 << 52) - 1);
    let (significand, binary_exponent) = if exponent_bits == 0 {
        (fraction, -1074)
    } else {
        (fraction | (1_u64 << 52), exponent_bits - 1023 - 52)
    };
    let five_power = 5_u128.checked_pow(digits as u32)?;
    let numerator = u128::from(significand).checked_mul(five_power)?;
    let exponent = binary_exponent + digits as i32;
    if exponent >= 0 {
        return numerator.checked_shl(exponent as u32);
    }
    let shift = (-exponent) as u32;
    if shift > 128 {
        return Some(0);
    }
    if shift == 128 {
        return Some(u128::from(numerator >= (1_u128 << 127)));
    }
    let quotient = numerator >> shift;
    let remainder_mask = (1_u128 << shift) - 1;
    let remainder = numerator & remainder_mask;
    let half = 1_u128 << (shift - 1);
    Some(quotient + u128::from(remainder >= half))
}

fn js_round_i32(value: f64) -> i32 {
    (value + 0.5).floor() as i32
}

fn centered_start(total_width: i32, text: &str) -> i32 {
    (f64::from(total_width) / 2.0 - display_width(text) as f64 / 2.0).floor() as i32
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::mermaid::flowchart::LayoutDirection;

    fn config(use_ascii: bool) -> AsciiConfig {
        AsciiConfig {
            use_ascii,
            padding_x: 5,
            padding_y: 5,
            box_border_padding: 1,
            direction: LayoutDirection::TD,
        }
    }

    #[test]
    fn parses_every_statement_kind() {
        let chart = parse_xy_chart(
            r#"xychart-beta horizontal
	title "Sales"
	x-axis "Quarter" [Q1, Q2]
	y-axis "Revenue" -10 --> 30
	bar [1, 2]
	line [2, 3]"#,
        );
        assert_eq!(chart.title.as_deref(), Some("Sales"));
        assert!(chart.horizontal);
        assert_eq!(chart.x_axis.title.as_deref(), Some("Quarter"));
        assert_eq!(
            chart.x_axis.categories.as_deref(),
            Some(&["Q1".to_owned(), "Q2".to_owned()][..])
        );
        assert_eq!(chart.y_axis.title.as_deref(), Some("Revenue"));
        assert_eq!(
            chart.y_axis.range,
            Some(AxisRange {
                min: -10.0,
                max: 30.0
            })
        );
        assert_eq!(chart.series.len(), 2);
        assert_eq!(chart.series[0].kind, SeriesType::Bar);
        assert_eq!(chart.series[1].kind, SeriesType::Line);
    }

    #[test]
    fn parses_numeric_x_axis_and_derives_value_range() {
        let chart = parse_xy_chart("xychart-beta\nx-axis \"Time\" 0 --> 10\nbar [10, 20]");
        assert_eq!(
            chart.x_axis.range,
            Some(AxisRange {
                min: 0.0,
                max: 10.0
            })
        );
        assert_eq!(
            chart.y_axis.range,
            Some(AxisRange {
                min: 9.0,
                max: 21.0
            })
        );
    }

    #[test]
    fn javascript_number_formatting_matches_known_outputs() {
        assert_eq!(js_number_to_string(0.1 + 0.2), "0.30000000000000004");
        assert_eq!(js_number_to_string(1e21), "1e+21");
        assert_eq!(js_to_fixed(2.5, 0), "3");
        assert_eq!(js_to_fixed(1.005, 2), "1.00");
        assert_eq!(js_to_fixed(0.15, 1), "0.1");
        assert_eq!(js_number_to_string(-0.0), "0");
    }

    /// An axis whose span is below the spacing of representable values must
    /// still render: a tick step that no longer advances ends the tick list.
    #[test]
    fn axis_finer_than_float_spacing_renders() {
        assert_eq!(nice_tick_values(1e17, 1e17 + 16.0).len(), 1);
        let output = render(
            "xychart-beta\nx-axis [a]\ny-axis 100000000000000000 --> 100000000000000016",
            &config(true),
            ColorMode::None,
            &Theme::default(),
        );
        assert!(output.contains("100000000000000000"), "{output}");
    }

    #[test]
    fn unicode_bar_chart_matches_golden_output() {
        let output = render(
            "xychart-beta\nx-axis [A, B]\ny-axis 0 --> 10\nbar [2, 8]",
            &config(false),
            ColorMode::None,
            &Theme::default(),
        );
        let expected = r" 10┤····························································
   │
   │
   │
  8┤·········································████████···········
   │                                         ████████
   │                                         ████████
   │                                         ████████
  6┤·········································████████···········
   │                                         ████████
   │                                         ████████
  4┤·········································████████···········
   │                                         ████████
   │                                         ████████
   │                                         ████████
  2┤···········████████······················████████···········
   │           ████████                      ████████
   │           ████████                      ████████
   │           ████████                      ████████
  0┼···········████████······················████████···········
   ┼───────────────┬─────────────────────────────┬──────────────
                   A                             B";
        assert_eq!(output, expected);
    }

    #[test]
    fn ascii_line_chart_matches_golden_output() {
        let output = render(
            "xychart-beta\nx-axis 0 --> 1\ny-axis -2.5 --> 2.5\nline [-2.5, 0, 2.5]",
            &config(true),
            ColorMode::None,
            &Theme::default(),
        );
        let expected = r"   |                                        +---------------
   |                                        |
  2+........................................|...................
   |                                        |
   |                                        |
   |                                        |
  1+........................................|...................
   |                                        |
   |                                        |
  0+....................+-------------------+...................
   |                    |
   |                    |
   |                    |
 -1+....................|.......................................
   |                    |
   |                    |
   |                    |
 -2+....................|.......................................
   |                    |
   |     ---------------+
   +----------+-------------------+-------------------+---------
              0                  0.5                  1";
        assert_eq!(output, expected);
    }

    #[test]
    fn renders_ascii_horizontal_mixed_chart() {
        let output = render(
            "xychart-beta horizontal\ntitle \"Scores\"\nx-axis [A, B, C]\nbar [10, 30, 20]\nline \
			 [20, 10, 30]",
            &config(true),
            ColorMode::None,
            &Theme::default(),
        );
        assert!(output.contains("Scores"));
        assert!(output.contains('#'));
        assert!(output.contains("Bar 1"));
        assert!(output.contains("Line 1"));
        assert!(!output.contains('─'));
    }
}

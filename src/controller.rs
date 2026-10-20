use crate::assets::HighlightingAssets;
use crate::config::{Config, MermaidMode, VisibleLines};
#[cfg(feature = "git")]
use crate::diff::{get_git_diff, LineChanges};
use crate::error::*;
use crate::input::{Input, InputReader, OpenedInput};
#[cfg(feature = "lessopen")]
use crate::lessopen::LessOpenPreprocessor;
#[cfg(feature = "git")]
use crate::line_range::LineRange;
use crate::line_range::{LineRanges, MaxBufferedLineNumber, RangeCheckResult};
use crate::mermaid::{ColorMode, RenderOptions, Theme as MermaidTheme};
use crate::output::{OutputHandle, OutputType};
#[cfg(feature = "paging")]
use crate::paging::PagingMode;
use crate::printer::{InteractivePrinter, Printer, SimplePrinter};
use crate::syntax_mapping::MappingTarget;
use std::collections::VecDeque;
use std::io::{self, BufRead, Write};
use std::mem;

use clircle::{Clircle, Identifier};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum MermaidInputKind {
    None,
    Standalone,
    Markdown,
}

fn is_mermaid_token(token: &str) -> bool {
    token.eq_ignore_ascii_case("mermaid") || token.eq_ignore_ascii_case("mmd")
}

fn is_markdown_token(token: &str) -> bool {
    matches!(
        token.to_ascii_lowercase().as_str(),
        "markdown" | "md" | "mdown" | "mkd" | "mkdn" | "mdwn" | "multimarkdown"
    )
}

fn is_mermaid_open_fence(line: &[u8], is_first_line: bool) -> bool {
    let Ok(s) = std::str::from_utf8(line) else {
        return false;
    };
    let s = if is_first_line {
        s.strip_prefix('\u{feff}').unwrap_or(s)
    } else {
        s
    };
    let trimmed = s.trim_end_matches(['\r', '\n']);
    trimmed
        .strip_prefix("```mermaid")
        .is_some_and(|rest| rest.chars().all(char::is_whitespace))
}

fn is_mermaid_close_fence(line: &[u8]) -> bool {
    let Ok(s) = std::str::from_utf8(line) else {
        return false;
    };
    let trimmed = s.trim_end_matches(['\r', '\n']);
    trimmed
        .strip_prefix("```")
        .is_some_and(|rest| rest.chars().all(char::is_whitespace))
}
pub struct Controller<'a> {
    config: &'a Config<'a>,
    assets: &'a HighlightingAssets,
    #[cfg(feature = "lessopen")]
    preprocessor: Option<LessOpenPreprocessor>,
}

impl Controller<'_> {
    pub fn new<'a>(config: &'a Config, assets: &'a HighlightingAssets) -> Controller<'a> {
        Controller {
            config,
            assets,
            #[cfg(feature = "lessopen")]
            preprocessor: LessOpenPreprocessor::new().ok(),
        }
    }

    pub fn run(
        &self,
        inputs: Vec<Input>,
        output_handle: Option<&mut OutputHandle<'_>>,
    ) -> Result<bool> {
        self.run_with_error_handler(inputs, output_handle, default_error_handler)
    }

    pub fn run_with_error_handler(
        &self,
        inputs: Vec<Input>,
        output_handle: Option<&mut OutputHandle<'_>>,
        mut handle_error: impl FnMut(&Error, &mut dyn Write),
    ) -> Result<bool> {
        // only create our own OutputType if no output handle was provided.
        #[allow(unused_mut)]
        let mut output_type_opt: Option<OutputType> = None;

        #[cfg(feature = "paging")]
        if output_handle.is_none() {
            use crate::input::InputKind;
            use std::path::Path;

            // Do not launch the pager if NONE of the input files exist
            let mut paging_mode = self.config.paging_mode;
            if self.config.paging_mode != PagingMode::Never {
                let call_pager = inputs.iter().any(|input| {
                    if let InputKind::OrdinaryFile(ref path) = input.kind {
                        Path::new(path).exists()
                    } else {
                        true
                    }
                });
                if !call_pager {
                    paging_mode = PagingMode::Never;
                }
            }

            let wrapping_mode = self.config.wrapping_mode;

            let has_mermaid_input = self.should_render_mermaid()
                && (self.config.language.is_some_and(|l| {
                    l.eq_ignore_ascii_case("markdown")
                        || l.eq_ignore_ascii_case("md")
                        || l.eq_ignore_ascii_case("mermaid")
                        || l.eq_ignore_ascii_case("mmd")
                }) || inputs.iter().any(|input| match &input.kind {
                    InputKind::OrdinaryFile(path) => path
                        .extension()
                        .and_then(|e| e.to_str())
                        .is_some_and(|ext| {
                            ext.eq_ignore_ascii_case("md")
                                || ext.eq_ignore_ascii_case("markdown")
                                || ext.eq_ignore_ascii_case("mdown")
                                || ext.eq_ignore_ascii_case("mkd")
                                || ext.eq_ignore_ascii_case("mermaid")
                                || ext.eq_ignore_ascii_case("mmd")
                        }),
                    _ => self.config.mermaid_mode == crate::config::MermaidMode::Always,
                }));

            output_type_opt = Some(OutputType::from_mode_with_options(
                paging_mode,
                wrapping_mode,
                self.config.pager,
                has_mermaid_input,
            )?);
        }

        #[cfg(not(feature = "paging"))]
        if output_handle.is_none() {
            output_type_opt = Some(OutputType::stdout());
        }

        let attached_to_pager = match (&output_handle, &output_type_opt) {
            (Some(_), _) => true,
            (None, Some(ot)) => ot.is_pager(),
            (None, None) => false,
        };

        let stdout_identifier = if cfg!(windows) || attached_to_pager {
            None
        } else {
            clircle::Identifier::stdout()
        };

        let mut writer = match (output_handle, &mut output_type_opt) {
            (Some(OutputHandle::FmtWrite(w)), _) => OutputHandle::FmtWrite(w),
            (Some(OutputHandle::IoWrite(w)), _) => OutputHandle::IoWrite(w),
            (None, Some(ot)) => ot.handle()?,
            (None, None) => unreachable!("No output handle and no output type available"),
        };
        let mut no_errors: bool = true;
        let stderr = io::stderr();

        for (index, input) in inputs.into_iter().enumerate() {
            let identifier = stdout_identifier.as_ref();
            let is_first = index == 0;
            let result = if input.is_stdin() {
                self.print_input(input, &mut writer, io::stdin().lock(), identifier, is_first)
            } else {
                // Use dummy stdin since stdin is actually not used (#1902)
                self.print_input(input, &mut writer, io::empty(), identifier, is_first)
            };
            if let Err(error) = result {
                match writer {
                    // It doesn't make much sense to send errors straight to stderr if the user
                    // provided their own buffer, so we just return it.
                    OutputHandle::FmtWrite(_) => return Err(error),
                    OutputHandle::IoWrite(ref mut writer) => {
                        if attached_to_pager {
                            handle_error(&error, writer);
                        } else {
                            handle_error(&error, &mut stderr.lock());
                        }
                    }
                }
                no_errors = false;
            }
        }

        Ok(no_errors)
    }

    fn print_input<R: BufRead>(
        &self,
        input: Input,
        writer: &mut OutputHandle,
        stdin: R,
        stdout_identifier: Option<&Identifier>,
        is_first: bool,
    ) -> Result<()> {
        let mut opened_input = {
            #[cfg(feature = "lessopen")]
            match self.preprocessor {
                Some(ref preprocessor) if self.config.use_lessopen => {
                    preprocessor.open(input, stdin, stdout_identifier)?
                }
                _ => input.open(stdin, stdout_identifier)?,
            }

            #[cfg(not(feature = "lessopen"))]
            input.open(stdin, stdout_identifier)?
        };
        opened_input.reader.unbuffered = self.config.unbuffered;
        #[cfg(feature = "git")]
        let line_changes = if self.config.visible_lines.diff_mode()
            || (!self.config.loop_through && self.config.style_components.changes())
        {
            match opened_input.kind {
                crate::input::OpenedInputKind::OrdinaryFile(ref path) => {
                    let diff = get_git_diff(path);

                    // Skip files without Git modifications
                    if self.config.visible_lines.diff_mode()
                        && diff
                            .as_ref()
                            .map(|changes| changes.is_empty())
                            .unwrap_or(false)
                    {
                        return Ok(());
                    }

                    diff
                }
                _ if self.config.visible_lines.diff_mode() => {
                    // Skip non-file inputs in diff mode
                    return Ok(());
                }
                _ => None,
            }
        } else {
            None
        };

        let printer_config_override = if !self.config.loop_through {
            let unknown_mermaid_token = |token: &str| {
                is_mermaid_token(token)
                    && self
                        .assets
                        .get_syntax_set()
                        .ok()
                        .and_then(|ss| ss.find_syntax_by_token(token))
                        .is_none()
            };
            let lang_override = self.config.language.is_some_and(unknown_mermaid_token);
            let fallback_override = self
                .config
                .fallback_syntax
                .is_some_and(unknown_mermaid_token);
            if lang_override || fallback_override {
                let mut cfg = self.config.clone();
                if lang_override {
                    cfg.language = Some("txt");
                }
                if fallback_override {
                    cfg.fallback_syntax = Some("txt");
                }
                Some(cfg)
            } else {
                None
            }
        } else {
            None
        };
        let printer_config = printer_config_override.as_ref().unwrap_or(self.config);

        let mut printer: Box<dyn Printer> = if self.config.loop_through {
            Box::new(SimplePrinter::new(self.config))
        } else {
            Box::new(InteractivePrinter::new(
                printer_config,
                self.assets,
                &mut opened_input,
                #[cfg(feature = "git")]
                &line_changes,
            )?)
        };

        self.print_file(
            &mut *printer,
            writer,
            &mut opened_input,
            !is_first,
            #[cfg(feature = "git")]
            &line_changes,
        )
    }

    fn print_file(
        &self,
        printer: &mut dyn Printer,
        writer: &mut OutputHandle,
        input: &mut OpenedInput,
        add_header_padding: bool,
        #[cfg(feature = "git")] line_changes: &Option<LineChanges>,
    ) -> Result<()> {
        if !input.reader.first_line.is_empty() || self.config.style_components.header() {
            printer.print_header(writer, input, add_header_padding)?;
        }

        if !input.reader.first_line.is_empty() {
            let line_ranges = match &self.config.visible_lines {
                VisibleLines::Ranges(line_ranges) => line_ranges.clone(),
                #[cfg(feature = "git")]
                &VisibleLines::DiffContext(context) => {
                    let mut line_ranges: Vec<LineRange> = vec![];

                    if let Some(line_changes) = line_changes {
                        for &line in line_changes.keys() {
                            let line = line as usize;
                            line_ranges
                                .push(LineRange::new(line.saturating_sub(context), line + context));
                        }
                    }

                    LineRanges::from(line_ranges)
                }
            };

            let mermaid_kind = if self.should_render_mermaid() {
                self.detect_mermaid_input_kind(input)
            } else {
                MermaidInputKind::None
            };

            self.print_file_ranges(
                printer,
                writer,
                &mut input.reader,
                &line_ranges,
                mermaid_kind,
            )?;
        }
        printer.print_footer(writer, input)?;

        Ok(())
    }

    fn should_render_mermaid(&self) -> bool {
        match self.config.mermaid_mode {
            MermaidMode::Never => false,
            MermaidMode::Always => true,
            MermaidMode::Auto => !self.config.loop_through,
        }
    }

    fn mermaid_render_options(&self) -> RenderOptions {
        let color_mode = if !self.config.colored_output {
            Some(ColorMode::None)
        } else if self.config.theme == "ansi" || self.config.theme == "base16" {
            Some(ColorMode::Ansi16)
        } else if self.config.true_color {
            Some(ColorMode::Truecolor)
        } else {
            Some(ColorMode::Ansi256)
        };
        let theme = Self::mermaid_theme_from_syntect(
            self.assets.get_theme(&self.config.theme),
            &self.config.theme,
        );
        RenderOptions {
            color_mode,
            theme,
            ..RenderOptions::default()
        }
    }

    #[allow(clippy::suboptimal_flops)]
    fn mermaid_theme_from_syntect(
        syntect_theme: &syntect::highlighting::Theme,
        theme_name: &str,
    ) -> MermaidTheme {
        let rgb_color = |c: Option<syntect::highlighting::Color>| -> Option<(u8, u8, u8)> {
            c.filter(|c| c.a > 1).map(|c| (c.r, c.g, c.b))
        };
        let luma = |(r, g, b): (u8, u8, u8)| -> f64 {
            0.299 * f64::from(r) + 0.587 * f64::from(g) + 0.114 * f64::from(b)
        };
        let mix =
            |(r1, g1, b1): (u8, u8, u8), (r2, g2, b2): (u8, u8, u8), t: f64| -> (u8, u8, u8) {
                let ch = |c1: u8, c2: u8| -> u8 {
                    (f64::from(c1) * t + f64::from(c2) * (1.0 - t))
                        .round()
                        .clamp(0.0, 255.0) as u8
                };
                (ch(r1, r2), ch(g1, g2), ch(b1, b2))
            };
        let to_hex = |(r, g, b): (u8, u8, u8)| -> String { format!("#{r:02x}{g:02x}{b:02x}") };

        let bg_rgb = rgb_color(syntect_theme.settings.background);
        let fg_rgb = rgb_color(syntect_theme.settings.foreground);
        let gutter_rgb = rgb_color(syntect_theme.settings.gutter_foreground);

        let is_light = if let Some(bg) = bg_rgb {
            luma(bg) > 128.0
        } else if let Some(fg) = fg_rgb {
            luma(fg) < 128.0
        } else {
            theme_name.to_ascii_lowercase().contains("light")
        };

        let fg = fg_rgb.unwrap_or(if is_light {
            (0x27, 0x27, 0x2a) // #27272a
        } else {
            (0xe4, 0xe4, 0xe7) // #e4e4e7
        });
        let bg = bg_rgb.unwrap_or(if is_light {
            (0xff, 0xff, 0xff) // #ffffff
        } else {
            (0x18, 0x18, 0x1b) // #18181b
        });

        let muted_base = gutter_rgb.unwrap_or(bg);
        let border = to_hex(mix(fg, muted_base, 0.65));
        let line = to_hex(mix(fg, muted_base, 0.55));
        let arrow = to_hex(mix(fg, bg, 0.80));

        MermaidTheme {
            fg: to_hex(fg),
            border: border.clone(),
            line: line.clone(),
            arrow,
            accent: None,
            bg: Some(to_hex(bg)),
            corner: Some(line),
            junction: Some(border),
        }
    }

    fn detect_mermaid_input_kind(&self, input: &mut OpenedInput) -> MermaidInputKind {
        let is_binary = input
            .reader
            .content_type
            .is_some_and(|c| c.is_binary() && !self.config.show_nonprintable)
            && !matches!(self.config.binary, crate::BinaryBehavior::AsText);
        if is_binary {
            return MermaidInputKind::None;
        }

        if let Some(lang) = self.config.language {
            if is_mermaid_token(lang) {
                return MermaidInputKind::Standalone;
            }
            if is_markdown_token(lang) {
                return MermaidInputKind::Markdown;
            }
        }

        if self.config.language.is_none() {
            if let Some(path) = input.path() {
                match self.config.syntax_mapping.get_syntax_for(path) {
                    Some(MappingTarget::MapTo(target)) => {
                        if is_mermaid_token(target) {
                            return MermaidInputKind::Standalone;
                        }
                        if is_markdown_token(target) {
                            return MermaidInputKind::Markdown;
                        }
                    }
                    Some(MappingTarget::MapToUnknown) => {
                        return MermaidInputKind::None;
                    }
                    Some(MappingTarget::MapExtensionToUnknown) => {}
                    None => {
                        if path
                            .extension()
                            .and_then(|e| e.to_str())
                            .is_some_and(is_mermaid_token)
                        {
                            return MermaidInputKind::Standalone;
                        }
                    }
                }
            }
        }

        if let Ok(syntax_in_set) = self.assets.get_syntax(
            self.config.language,
            self.config.fallback_syntax,
            input,
            &self.config.syntax_mapping,
        ) {
            let syntax_name = syntax_in_set.syntax.name.as_str();
            if is_mermaid_token(syntax_name) {
                return MermaidInputKind::Standalone;
            }
            if is_markdown_token(syntax_name) {
                return MermaidInputKind::Markdown;
            }
        }

        if self.config.language.is_none() && input.path().is_none() {
            if let Some(fallback) = self.config.fallback_syntax {
                if is_mermaid_token(fallback) {
                    return MermaidInputKind::Standalone;
                }
                if is_markdown_token(fallback) {
                    return MermaidInputKind::Markdown;
                }
            }
        }

        MermaidInputKind::None
    }

    fn print_file_ranges(
        &self,
        printer: &mut dyn Printer,
        writer: &mut OutputHandle,
        reader: &mut InputReader,
        line_ranges: &LineRanges,
        mermaid_kind: MermaidInputKind,
    ) -> Result<()> {
        let mut current_line_buffer: Vec<u8> = Vec::new();
        let mut current_line_number: usize = 1;
        // Buffer needs to be 1 greater than the offset to have a look-ahead line for EOF
        let buffer_size: usize = line_ranges.largest_offset_from_end() + 1;
        // Buffers multiple line data and line number
        let mut buffered_lines: VecDeque<(Vec<u8>, usize)> = VecDeque::with_capacity(buffer_size);

        let mut reached_eof: bool = false;
        let mut first_range: bool = true;
        let mut mid_range: bool = false;

        let style_snip = self.config.style_components.snip();

        if mermaid_kind == MermaidInputKind::Standalone {
            let mut all_lines: Vec<(Vec<u8>, usize)> = Vec::new();
            while reader.read_line(&mut current_line_buffer)? {
                let is_blank = self.config.number_nonblank
                    && current_line_buffer
                        .iter()
                        .all(|&b| b == b'\r' || b == b'\n');
                all_lines.push((mem::take(&mut current_line_buffer), current_line_number));
                if !is_blank {
                    current_line_number += 1;
                }
            }

            let Some(&(_, last_line_nr)) = all_lines.last() else {
                return Ok(());
            };
            let max_buf = MaxBufferedLineNumber::Final(last_line_nr);

            let any_in_range = all_lines
                .iter()
                .any(|&(_, nr)| line_ranges.check(nr, max_buf) == RangeCheckResult::InRange);
            if !any_in_range {
                return Ok(());
            }

            let mut source = String::new();
            for (idx, (raw_line, _)) in all_lines.iter().enumerate() {
                let line_str = String::from_utf8_lossy(raw_line);
                if idx == 0 {
                    source.push_str(line_str.strip_prefix('\u{feff}').unwrap_or(&line_str));
                } else {
                    source.push_str(&line_str);
                }
            }

            if let Some(diagram) = crate::mermaid::render(&source, &self.mermaid_render_options())
                .ok()
                .filter(|d| !d.trim().is_empty())
            {
                for (raw_line, nr) in &all_lines {
                    printer.print_line(true, writer, *nr, raw_line, max_buf)?;
                }
                for diagram_line in diagram.lines() {
                    printer.print_diagram_line(writer, diagram_line)?;
                }
                if self.config.unbuffered {
                    writer.flush()?;
                }
                return Ok(());
            }

            for (raw_line, line_nr) in all_lines {
                match line_ranges.check(line_nr, max_buf) {
                    RangeCheckResult::BeforeOrBetweenRanges => {
                        printer.print_line(true, writer, line_nr, &raw_line, max_buf)?;
                        mid_range = false;
                    }
                    RangeCheckResult::InRange => {
                        if style_snip {
                            if first_range {
                                first_range = false;
                                mid_range = true;
                            } else if !mid_range {
                                mid_range = true;
                                printer.print_snip(writer)?;
                            }
                        }
                        printer.print_line(false, writer, line_nr, &raw_line, max_buf)?;
                        if self.config.unbuffered {
                            writer.flush()?;
                        }
                    }
                    RangeCheckResult::AfterLastRange => {
                        break;
                    }
                }
            }
            return Ok(());
        }

        let next_buffered_line =
            |reader: &mut InputReader,
             buffered_lines: &mut VecDeque<(Vec<u8>, usize)>,
             current_line_buffer: &mut Vec<u8>,
             current_line_number: &mut usize,
             reached_eof: &mut bool|
             -> Result<Option<(Vec<u8>, usize, MaxBufferedLineNumber)>> {
                while buffered_lines.len() < buffer_size && !*reached_eof {
                    if reader.read_line(current_line_buffer)? {
                        if self.config.number_nonblank
                            && current_line_buffer
                                .iter()
                                .all(|&b| b == b'\r' || b == b'\n')
                        {
                            buffered_lines
                                .push_back((mem::take(current_line_buffer), *current_line_number));
                        } else {
                            buffered_lines
                                .push_back((mem::take(current_line_buffer), *current_line_number));
                            *current_line_number += 1;
                        }
                    } else {
                        *reached_eof = true;
                    }
                }

                let Some((line, line_nr)) = buffered_lines.pop_front() else {
                    return Ok(None);
                };

                let max_buffered_line_number = buffered_lines
                    .back()
                    .map(|(_, max_line_number)| {
                        if *reached_eof {
                            MaxBufferedLineNumber::Final(*max_line_number)
                        } else {
                            MaxBufferedLineNumber::Tentative(*max_line_number)
                        }
                    })
                    .unwrap_or(MaxBufferedLineNumber::Final(line_nr));

                Ok(Some((line, line_nr, max_buffered_line_number)))
            };

        while let Some((line, line_nr, max_buffered_line_number)) = next_buffered_line(
            reader,
            &mut buffered_lines,
            &mut current_line_buffer,
            &mut current_line_number,
            &mut reached_eof,
        )? {
            if mermaid_kind == MermaidInputKind::Markdown
                && is_mermaid_open_fence(&line, line_nr == 1)
            {
                let fence_line_nr = line_nr;
                let mut block_lines = vec![(line, line_nr)];
                let mut closed = false;

                while let Some((next_line, next_nr, _)) = next_buffered_line(
                    reader,
                    &mut buffered_lines,
                    &mut current_line_buffer,
                    &mut current_line_number,
                    &mut reached_eof,
                )? {
                    let is_close = is_mermaid_close_fence(&next_line);
                    block_lines.push((next_line, next_nr));
                    if is_close {
                        closed = true;
                        break;
                    }
                }

                let latest_max_buf = buffered_lines
                    .back()
                    .map(|(_, max_line_number)| {
                        if reached_eof {
                            MaxBufferedLineNumber::Final(*max_line_number)
                        } else {
                            MaxBufferedLineNumber::Tentative(*max_line_number)
                        }
                    })
                    .unwrap_or_else(|| {
                        let last_nr = block_lines
                            .last()
                            .map(|&(_, nr)| nr)
                            .unwrap_or(fence_line_nr);
                        MaxBufferedLineNumber::Final(last_nr)
                    });

                let any_in_range = block_lines.iter().any(|&(_, nr)| {
                    line_ranges.check(nr, latest_max_buf) == RangeCheckResult::InRange
                });

                if !any_in_range {
                    for (raw_line, nr) in block_lines {
                        match line_ranges.check(nr, latest_max_buf) {
                            RangeCheckResult::BeforeOrBetweenRanges => {
                                printer.print_line(true, writer, nr, &raw_line, latest_max_buf)?;
                                mid_range = false;
                            }
                            RangeCheckResult::InRange => {}
                            RangeCheckResult::AfterLastRange => {
                                return Ok(());
                            }
                        }
                    }
                    continue;
                }

                let rendered = if closed && block_lines.len() >= 2 {
                    let mut mermaid_source = String::new();
                    for (raw_line, _) in &block_lines[1..block_lines.len() - 1] {
                        mermaid_source.push_str(&String::from_utf8_lossy(raw_line));
                    }
                    crate::mermaid::render(&mermaid_source, &self.mermaid_render_options())
                        .ok()
                        .filter(|d| !d.trim().is_empty())
                } else {
                    None
                };

                if let Some(diagram) = rendered {
                    if style_snip {
                        if first_range {
                            first_range = false;
                            mid_range = true;
                        } else if !mid_range {
                            mid_range = true;
                            printer.print_snip(writer)?;
                        }
                    }

                    for (raw_line, nr) in &block_lines {
                        printer.print_line(true, writer, *nr, raw_line, latest_max_buf)?;
                    }

                    for diagram_line in diagram.lines() {
                        printer.print_diagram_line(writer, diagram_line)?;
                    }
                    if self.config.unbuffered {
                        writer.flush()?;
                    }

                    if let Some(&(_, last_nr)) = block_lines.last() {
                        match line_ranges.check(last_nr, latest_max_buf) {
                            RangeCheckResult::BeforeOrBetweenRanges => {
                                mid_range = false;
                            }
                            RangeCheckResult::InRange => {}
                            RangeCheckResult::AfterLastRange => {
                                break;
                            }
                        }
                    }
                } else {
                    printer.set_error_line(fence_line_nr);
                    for (raw_line, nr) in block_lines {
                        match line_ranges.check(nr, latest_max_buf) {
                            RangeCheckResult::BeforeOrBetweenRanges => {
                                printer.print_line(true, writer, nr, &raw_line, latest_max_buf)?;
                                mid_range = false;
                            }
                            RangeCheckResult::InRange => {
                                if style_snip {
                                    if first_range {
                                        first_range = false;
                                        mid_range = true;
                                    } else if !mid_range {
                                        mid_range = true;
                                        printer.print_snip(writer)?;
                                    }
                                }
                                printer.print_line(false, writer, nr, &raw_line, latest_max_buf)?;
                                if self.config.unbuffered {
                                    writer.flush()?;
                                }
                            }
                            RangeCheckResult::AfterLastRange => {
                                return Ok(());
                            }
                        }
                    }
                }
                continue;
            }

            match line_ranges.check(line_nr, max_buffered_line_number) {
                RangeCheckResult::BeforeOrBetweenRanges => {
                    // Call the printer in case we need to call the syntax highlighter
                    // for this line. However, set `out_of_range` to `true`.
                    printer.print_line(true, writer, line_nr, &line, max_buffered_line_number)?;
                    mid_range = false;
                }

                RangeCheckResult::InRange => {
                    if style_snip {
                        if first_range {
                            first_range = false;
                            mid_range = true;
                        } else if !mid_range {
                            mid_range = true;
                            printer.print_snip(writer)?;
                        }
                    }

                    printer.print_line(false, writer, line_nr, &line, max_buffered_line_number)?;
                    if self.config.unbuffered {
                        writer.flush()?;
                    }
                }
                RangeCheckResult::AfterLastRange => {
                    break;
                }
            }
        }
        Ok(())
    }
}

extern crate ansi_term;
extern crate atty;
extern crate console;
extern crate git2;
extern crate syntect;

#[macro_use]
extern crate clap;

use std::collections::HashMap;
use std::io::{self, BufRead, Result};
use std::path::Path;
use std::process;

use ansi_term::Colour::{Fixed, Green, Red, White, Yellow};
use atty::Stream;
use clap::{App, AppSettings, Arg, ArgMatches};
use console::Term;
use git2::{DiffOptions, IntoCString, Repository};

use syntect::easy::HighlightFile;
use syntect::highlighting::{Theme, ThemeSet};
use syntect::parsing::SyntaxSet;
use syntect::util::as_24_bit_terminal_escaped;

#[derive(Copy, Clone, Debug)]
enum LineChange {
    Added,
    RemovedAbove,
    RemovedBelow,
    Modified,
}

type LineChanges = HashMap<u32, LineChange>;

const GRID_COLOR : u8 = 238;

fn print_horizontal_line(grid_char: char, term_width: usize) {
    let prefix = format!("{}{}", "─".repeat(7), grid_char);
    let line = "─".repeat(term_width - prefix.len());
    println!("{}{}", Fixed(GRID_COLOR).paint(prefix), Fixed(GRID_COLOR).paint(line));
}

fn print_file<P: AsRef<Path>>(
    theme: &Theme,
    filename: P,
    line_changes: Option<LineChanges>,
) -> io::Result<()> {
    let ss = SyntaxSet::load_defaults_nonewlines();

    let mut highlighter = HighlightFile::new(filename.as_ref().clone(), &ss, &theme)?;

    let term = Term::stdout();
    let (_, term_width) = term.size();
    let term_width = term_width as usize;

    print_horizontal_line('┬', term_width);

    println!(
        "       {} {}",
        Fixed(GRID_COLOR).paint("│"),
        White.bold().paint(filename.as_ref().to_string_lossy())
    );

    print_horizontal_line('┼', term_width);

    for (idx, maybe_line) in highlighter.reader.lines().enumerate() {
        let line_nr = idx + 1;
        let line = maybe_line.unwrap_or("<INVALID UTF-8>".into());
        let regions = highlighter.highlight_lines.highlight(&line);

        let line_change = if let Some(ref changes) = line_changes {
            match changes.get(&(line_nr as u32)) {
                Some(&LineChange::Added) => Green.paint("+"),
                Some(&LineChange::RemovedAbove) => Red.paint("‾"),
                Some(&LineChange::RemovedBelow) => Red.paint("_"),
                Some(&LineChange::Modified) => Yellow.paint("~"),
                _ => Fixed(1).paint(" "), // TODO
            }
        } else {
            Fixed(1).paint(" ") // TODO
        };

        println!(
            "{} {} {} {}",
            Fixed(244).paint(format!("{:4}", line_nr)),
            line_change,
            Fixed(GRID_COLOR).paint("│"),
            as_24_bit_terminal_escaped(&regions, false)
        );
    }

    print_horizontal_line('┴', term_width);

    Ok(())
}

fn get_line_changes(filename: String) -> Option<LineChanges> {
    let repo = Repository::open_from_env().ok()?;

    let mut diff_options = DiffOptions::new();
    diff_options.pathspec(filename.into_c_string().ok()?);
    diff_options.context_lines(0);

    let diff = repo.diff_index_to_workdir(None, Some(&mut diff_options))
        .ok()?;

    let mut line_changes: LineChanges = HashMap::new();

    let mark_section =
        |line_changes: &mut LineChanges, start: u32, end: i32, change: LineChange| {
            for line in start..(end + 1) as u32 {
                line_changes.insert(line, change);
            }
        };

    let _ = diff.foreach(
        &mut |_, _| true,
        None,
        Some(&mut |_, hunk| {
            let old_lines = hunk.old_lines();
            let new_start = hunk.new_start();
            let new_lines = hunk.new_lines();
            let new_end = (new_start + new_lines) as i32 - 1;

            if old_lines == 0 && new_lines > 0 {
                mark_section(&mut line_changes, new_start, new_end, LineChange::Added);
            } else if new_lines == 0 && old_lines > 0 {
                if new_start <= 0 {
                    mark_section(&mut line_changes, 1, 1, LineChange::RemovedAbove);
                } else {
                    mark_section(
                        &mut line_changes,
                        new_start,
                        new_start as i32,
                        LineChange::RemovedBelow,
                    );
                }
            } else {
                mark_section(&mut line_changes, new_start, new_end, LineChange::Modified);
            }

            true
        }),
        None,
    );

    Some(line_changes)
}

fn run(matches: &ArgMatches) -> Result<()> {
    let theme_set = ThemeSet::load_from_folder("/home/shark/Informatik/rust/bat/themes").unwrap();
    let theme = &theme_set.themes["Monokai"];

    if let Some(files) = matches.values_of("FILE") {
        for file in files {
            let line_changes = get_line_changes(file.to_string());
            print_file(theme, file, line_changes)?;
        }
    }

    Ok(())
}

fn main() {
    let clap_color_setting = if atty::is(Stream::Stdout) {
        AppSettings::ColoredHelp
    } else {
        AppSettings::ColorNever
    };

    let matches = App::new(crate_name!())
        .version(crate_version!())
        .setting(clap_color_setting)
        .setting(AppSettings::DeriveDisplayOrder)
        .setting(AppSettings::UnifiedHelpMessage)
        .setting(AppSettings::NextLineHelp)
        .setting(AppSettings::DisableVersion)
        .max_term_width(90)
        .about(crate_description!())
        .arg(
            Arg::with_name("FILE")
                .help("File(s) to print")
                .multiple(true)
                .empty_values(false),
        )
        .help_message("Print this help message.")
        .version_message("Show version information.")
        .get_matches();

    let result = run(&matches);

    if let Err(e) = result {
        eprintln!("{}: {}", Red.paint("[bat error]"), e);
        process::exit(1);
    }
}

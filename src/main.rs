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

use ansi_term::Colour::{Fixed, Green, Red, Yellow, White};
use atty::Stream;
use clap::{App, AppSettings, Arg, ArgMatches};
use console::Term;
use git2::{DiffOptions, IntoCString, Repository};

use syntect::easy::HighlightFile;
use syntect::highlighting::ThemeSet;
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

fn print_file<P: AsRef<Path>>(filename: P, line_changes: Option<LineChanges>) -> io::Result<()> {
    let ss = SyntaxSet::load_defaults_nonewlines();
    let ts = ThemeSet::load_defaults();
    let theme = &ts.themes["base16-eighties.dark"];

    let mut highlighter = HighlightFile::new(filename.as_ref().clone(), &ss, theme)?;

    let term = Term::stdout();
    let (_height, width) = term.size();

    let prefix = "───────┬";
    let line = "─".repeat(width as usize - prefix.len());
    println!("{}{}", Fixed(238).paint(prefix), Fixed(238).paint(line));

    println!(
        "       {} {}",
        Fixed(238).paint("│"),
        White.bold().paint(filename.as_ref().to_string_lossy())
    );

    let prefix = "───────┼";
    let line = "─".repeat(width as usize - prefix.len());
    println!("{}{}", Fixed(238).paint(prefix), Fixed(238).paint(line));

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
            Fixed(238).paint("│"),
            as_24_bit_terminal_escaped(&regions, false)
        );
    }

    let prefix = "───────┴";
    let line = "─".repeat(width as usize - prefix.len());
    println!("{}{}", Fixed(238).paint(prefix), Fixed(238).paint(line));

    Ok(())
}

fn get_line_changes(filename: String) -> Option<LineChanges> {
    let repo = Repository::open_from_env().ok()?;

    let mut diff_options = DiffOptions::new();
    diff_options.pathspec(filename.into_c_string().unwrap());
    diff_options.context_lines(0);

    let diff = repo.diff_index_to_workdir(None, Some(&mut diff_options))
        .unwrap();

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
    if let Some(files) = matches.values_of("file") {
        for file in files {
            let line_changes = get_line_changes(file.to_string());
            print_file(file, line_changes)?;
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

    let matches = App::new("bat")
        .version(crate_version!())
        .setting(clap_color_setting)
        .setting(AppSettings::DeriveDisplayOrder)
        .setting(AppSettings::UnifiedHelpMessage)
        .setting(AppSettings::NextLineHelp)
        .max_term_width(90)
        .about("A cat(1) clone with wings.")
        .arg(
            Arg::with_name("file")
                .help("Files to print")
                .multiple(true)
                .empty_values(false),
        )
        .help_message("Print this help message.")
        .version_message("Show version information.")
        .get_matches();

    let result = run(&matches);

    if let Err(e) = result {
        eprintln!("{}: {}", Red.paint("bat error"), e);
        process::exit(1);
    }
}

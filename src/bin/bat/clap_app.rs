use clap::{App as ClapApp, AppSettings, Arg, ArgGroup, SubCommand};
use std::path::Path;

pub fn build_app(interactive_output: bool) -> ClapApp<'static, 'static> {
    let clap_color_setting = if interactive_output {
        AppSettings::ColoredHelp
    } else {
        AppSettings::ColorNever
    };

    let app = ClapApp::new(crate_name!())
        .version(crate_version!())
        .global_setting(clap_color_setting)
        .global_setting(AppSettings::DeriveDisplayOrder)
        .global_setting(AppSettings::UnifiedHelpMessage)
        .global_setting(AppSettings::HidePossibleValuesInHelp)
        .setting(AppSettings::ArgsNegateSubcommands)
        .setting(AppSettings::AllowExternalSubcommands)
        .setting(AppSettings::DisableHelpSubcommand)
        .setting(AppSettings::VersionlessSubcommands)
        .max_term_width(100)
        .about(
            "A cat(1) clone with wings.\n\n\
             Use '--help' instead of '-h' to see a more detailed version of the help text.",
        )
        .long_about("A cat(1) clone with syntax highlighting and Git integration.")
        .arg(
            Arg::with_name("FILE")
                .help("File(s) to print / concatenate. Use '-' for standard input.")
                .long_help(
                    "File(s) to print / concatenate. Use a dash ('-') or no argument at all \
                     to read from standard input.",
                )
                .multiple(true)
                .empty_values(false),
        )
        .arg(
            Arg::with_name("show-all")
                .long("show-all")
                .alias("show-nonprintable")
                .short("A")
                .conflicts_with("language")
                .help("Show non-printable characters (space, tab, newline, ..).")
                .long_help(
                    "Show non-printable characters like space, tab or newline. \
                     This option can also be used to print binary files. \
                     Use '--tabs' to control the width of the tab-placeholders."
                ),
        )
        .arg(
            Arg::with_name("plain")
                .overrides_with("plain")
                .overrides_with("number")
                .short("p")
                .long("plain")
                .multiple(true)
                .help("Show plain style (alias for '--style=plain').")
                .long_help(
                    "Only show plain style, no decorations. This is an alias for \
                     '--style=plain'. When '-p' is used twice ('-pp'), it also disables \
                     automatic paging (alias for '--style=plain --pager=never').",
                ),
        )
        .arg(
            Arg::with_name("language")
                .short("l")
                .long("language")
                .overrides_with("language")
                .help("Set the language for syntax highlighting.")
                .long_help(
                    "Explicitly set the language for syntax highlighting. The language can be \
                     specified as a name (like 'C++' or 'LaTeX') or possible file extension \
                     (like 'cpp', 'hpp' or 'md'). Use '--list-languages' to show all supported \
                     language names and file extensions.",
                )
                .takes_value(true),
        )
        .arg(
            Arg::with_name("highlight-line")
                .long("highlight-line")
                .short("H")
                .takes_value(true)
                .number_of_values(1)
                .multiple(true)
                .value_name("N:M")
                .help("Highlight lines N through M.")
                .long_help(
                    "Highlight the specified line ranges with a different background color \
                     For example:\n  \
                     '--highlight-line 40' highlights line 40\n  \
                     '--highlight-line 30:40' highlights lines 30 to 40\n  \
                     '--highlight-line :40' highlights lines 1 to 40\n  \
                     '--highlight-line 40:' highlights lines 40 to the end of the file"
                ),
        )
        .arg(
            Arg::with_name("tabs")
                .long("tabs")
                .overrides_with("tabs")
                .takes_value(true)
                .value_name("T")
                .validator(
                    |t| {
                        t.parse::<u32>()
                            .map_err(|_t| "must be a number")
                            .map(|_t| ()) // Convert to Result<(), &str>
                            .map_err(|e| e.to_string())
                    }, // Convert to Result<(), String>
                )
                .help("Set the tab width to T spaces.")
                .long_help(
                    "Set the tab width to T spaces. Use a width of 0 to pass tabs through \
                     directly",
                ),
        )
        .arg(
            Arg::with_name("wrap")
                .long("wrap")
                .overrides_with("wrap")
                .takes_value(true)
                .value_name("mode")
                .possible_values(&["auto", "never", "character"])
                .default_value("auto")
                .hide_default_value(true)
                .help("Specify the text-wrapping mode (*auto*, never, character).")
                .long_help("Specify the text-wrapping mode (*auto*, never, character). \
                           The '--terminal-width' option can be used in addition to \
                           control the output width."),
        )
        .arg(
            Arg::with_name("terminal-width")
                .long("terminal-width")
                .takes_value(true)
                .value_name("width")
                .hidden_short_help(true)
                .allow_hyphen_values(true)
                .validator(
                    |t| {
                        let is_offset = t.starts_with('+') || t.starts_with('-');
                        t.parse::<i32>()
                            .map_err(|_e| "must be an offset or number")
                            .and_then(|v| if v == 0 && !is_offset {
                                Err("terminal width cannot be zero".into())
                            } else {
                                Ok(())
                            })
                            .map_err(|e| e.to_string())
                    })
                .help(
                    "Explicitly set the width of the terminal instead of determining it \
                     automatically. If prefixed with '+' or '-', the value will be treated \
                     as an offset to the actual terminal width. See also: '--wrap'.",
                ),
        )
        .arg(
            Arg::with_name("number")
                .long("number")
                .overrides_with("number")
                .short("n")
                .help("Show line numbers (alias for '--style=numbers').")
                .long_help(
                    "Only show line numbers, no other decorations. This is an alias for \
                     '--style=numbers'",
                ),
        )
        .arg(
            Arg::with_name("color")
                .long("color")
                .overrides_with("color")
                .takes_value(true)
                .value_name("when")
                .possible_values(&["auto", "never", "always"])
                .hide_default_value(true)
                .default_value("auto")
                .help("When to use colors (*auto*, never, always).")
                .long_help(
                    "Specify when to use colored output. The automatic mode \
                     only enables colors if an interactive terminal is detected. \
                     Possible values: *auto*, never, always.",
                ),
        )
        .arg(
            Arg::with_name("italic-text")
                .long("italic-text")
                .takes_value(true)
                .value_name("when")
                .possible_values(&["always", "never"])
                .default_value("never")
                .hide_default_value(true)
                .help("Use italics in output (always, *never*)")
                .long_help("Specify when to use ANSI sequences for italic text in the output. Possible values: always, *never*."),
        )
        .arg(
            Arg::with_name("decorations")
                .long("decorations")
                .overrides_with("decorations")
                .takes_value(true)
                .value_name("when")
                .possible_values(&["auto", "never", "always"])
                .default_value("auto")
                .hide_default_value(true)
                .help("When to show the decorations (*auto*, never, always).")
                .long_help(
                    "Specify when to use the decorations that have been specified \
                     via '--style'. The automatic mode only enables decorations if \
                     an interactive terminal is detected. Possible values: *auto*, never, always.",
                ),
        )
        .arg(
            Arg::with_name("paging")
                .long("paging")
                .overrides_with("paging")
                .takes_value(true)
                .value_name("when")
                .possible_values(&["auto", "never", "always"])
                .default_value("auto")
                .hide_default_value(true)
                .help("Specify when to use the pager (*auto*, never, always).")
                .long_help(
                    "Specify when to use the pager. To control which pager \
                     is used, set the PAGER or BAT_PAGER environment \
                     variables (the latter takes precedence) or use the '--pager' option. \
                     To disable the pager permanently, set BAT_PAGER to an empty string \
                     or set '--paging=never' in the configuration file. \
                     Possible values: *auto*, never, always.",
                ),
        )
        .arg(
            Arg::with_name("pager")
                .long("pager")
                .overrides_with("pager")
                .takes_value(true)
                .value_name("command")
                .hidden_short_help(true)
                .help("Determine which pager to use.")
                .long_help(
                    "Determine which pager is used. This option will overwrite \
                     the PAGER and BAT_PAGER environment variables. The default \
                     pager is 'less'. To disable the pager completely, use the \
                     '--paging' option. \
                     Example: '--pager \"less -RF\"'.",
                ),
        )
        .arg(
            Arg::with_name("map-syntax")
                .short("m")
                .long("map-syntax")
                .multiple(true)
                .takes_value(true)
                .number_of_values(1)
                .value_name("from:to")
                .help("Map a file extension or name to an existing syntax.")
                .long_help(
                    "Map a file extension or file name to an existing syntax (specified by a file \
                     extension or file name). For example, to highlight *.build files with the \
                     Python syntax, use '-m build:py'. To highlight files named '.myignore' with \
                     the Git Ignore syntax, use '-m .myignore:gitignore'.",
                )
                .takes_value(true),
        )
        .arg(
            Arg::with_name("theme")
                .long("theme")
                .overrides_with("theme")
                .takes_value(true)
                .help("Set the color theme for syntax highlighting.")
                .long_help(
                    "Set the theme for syntax highlighting. Use '--list-themes' to \
                     see all available themes. To set a default theme, add the \
                     '--theme=\"...\"' option to the configuration file or export the \
                     BAT_THEME environment variable (e.g.: export \
                     BAT_THEME=\"...\").",
                ),
        )
        .arg(
            Arg::with_name("list-themes")
                .long("list-themes")
                .help("Display all supported highlighting themes.")
                .long_help("Display a list of supported themes for syntax highlighting."),
        )
        .arg(
            Arg::with_name("style")
                .long("style")
                .value_name("style-components")
                // Need to turn this off for overrides_with to work as we want. See the bottom most
                // example at https://docs.rs/clap/2.32.0/clap/struct.Arg.html#method.overrides_with
                .use_delimiter(false)
                .takes_value(true)
                .overrides_with("style")
                .overrides_with("plain")
                .overrides_with("number")
                // Cannot use clap's built in validation because we have to turn off clap's delimiters
                .validator(|val| {
                    let mut invalid_vals = val.split(',').filter(|style| {
                        !&[
                            "auto", "full", "plain", "changes", "header", "grid", "numbers", "snip"
                        ]
                            .contains(style)
                    });

                    if let Some(invalid) = invalid_vals.next() {
                        Err(format!("Unknown style, '{}'", invalid))
                    } else {
                        Ok(())
                    }
                })
                .help(
                    "Comma-separated list of style elements to display \
                     (*auto*, full, plain, changes, header, grid, numbers, snip).",
                )
                .long_help(
                    "Configure which elements (line numbers, file headers, grid \
                     borders, Git modifications, ..) to display in addition to the \
                     file contents. The argument is a comma-separated list of \
                     components to display (e.g. 'numbers,changes,grid') or a \
                     pre-defined style ('full'). To set a default style, add the \
                     '--style=\"..\"' option to the configuration file or export the \
                     BAT_STYLE environment variable (e.g.: export BAT_STYLE=\"..\"). \
                     Possible values: *auto*, full, plain, changes, header, grid, numbers, snip.",
                ),
        )
        .arg(
            Arg::with_name("line-range")
                .long("line-range")
                .short("r")
                .multiple(true)
                .takes_value(true)
                .number_of_values(1)
                .value_name("N:M")
                .help("Only print the lines from N to M.")
                .long_help(
                    "Only print the specified range of lines for each file. \
                     For example:\n  \
                     '--line-range 30:40' prints lines 30 to 40\n  \
                     '--line-range :40' prints lines 1 to 40\n  \
                     '--line-range 40:' prints lines 40 to the end of the file\n  \
                     '--line-range 40' only prints line 40",
                ),
        )
        .arg(
            Arg::with_name("list-languages")
                .long("list-languages")
                .short("L")
                .conflicts_with("list-themes")
                .help("Display all supported languages.")
                .long_help("Display a list of supported languages for syntax highlighting."),
        )
        .arg(
            Arg::with_name("unbuffered")
                .short("u")
                .long("unbuffered")
                .hidden_short_help(true)
                .long_help(
                    "This option exists for POSIX-compliance reasons ('u' is for \
                     'unbuffered'). The output is always unbuffered - this option \
                     is simply ignored.",
                ),
        )
        .arg(
            Arg::with_name("no-config")
                .long("no-config")
                .hidden(true)
                .help("Do not use the configuration file"),
        )
        .arg(
            Arg::with_name("config-file")
                .long("config-file")
                .conflicts_with("list-languages")
                .conflicts_with("list-themes")
                .hidden(true)
                .help("Show path to the configuration file."),
        )
        .arg(
            Arg::with_name("config-dir")
                .long("config-dir")
                .hidden(true)
                .help("Show bat's configuration directory."),
        )
        .arg(
            Arg::with_name("cache-dir")
                .long("cache-dir")
                .hidden(true)
                .help("Show bat's cache directory."),
        )
        .help_message("Print this help message.")
        .version_message("Show version information.");

    // Check if the current directory contains a file name cache. Otherwise,
    // enable the 'bat cache' subcommand.
    if Path::new("cache").exists() {
        app
    } else {
        app.subcommand(
            SubCommand::with_name("cache")
                .about("Modify the syntax-definition and theme cache")
                .arg(
                    Arg::with_name("build")
                        .long("build")
                        .short("b")
                        .help("Initialize (or update) the syntax/theme cache.")
                        .long_help(
                            "Initialize (or update) the syntax/theme cache by loading from \
                             the source directory (default: the configuration directory).",
                        ),
                )
                .arg(
                    Arg::with_name("clear")
                        .long("clear")
                        .short("c")
                        .help("Remove the cached syntax definitions and themes."),
                )
                .group(
                    ArgGroup::with_name("cache-actions")
                        .args(&["build", "clear"])
                        .required(true),
                )
                .arg(
                    Arg::with_name("source")
                        .long("source")
                        .requires("build")
                        .takes_value(true)
                        .value_name("dir")
                        .help("Use a different directory to load syntaxes and themes from."),
                )
                .arg(
                    Arg::with_name("target")
                        .long("target")
                        .requires("build")
                        .takes_value(true)
                        .value_name("dir")
                        .help(
                            "Use a different directory to store the cached syntax and theme set.",
                        ),
                )
                .arg(
                    Arg::with_name("blank")
                        .long("blank")
                        .requires("build")
                        .help(
                            "Create completely new syntax and theme sets \
                             (instead of appending to the default sets).",
                        ),
                ),
        )
    }
}

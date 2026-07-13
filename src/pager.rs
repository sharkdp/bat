use shell_words::ParseError;
use std::env;

/// If we use a pager, this enum tells us from where we were told to use it.
#[derive(Debug, PartialEq)]
pub(crate) enum PagerSource {
    /// From --config
    Config,

    /// From the env var BAT_PAGER
    EnvVarBatPager,

    /// From the env var PAGER
    EnvVarPager,

    /// No pager was specified, default is used
    Default,
}

/// We know about some pagers, for example 'less'. This is a list of all pagers we know about
#[derive(Debug, PartialEq)]
pub(crate) enum PagerKind {
    /// bat
    Bat,

    /// less
    Less,

    /// more
    More,

    /// most
    Most,

    /// builtin
    Builtin,

    /// A pager we don't know about
    Unknown,
}

impl PagerKind {
    fn from_bin(bin: &str) -> PagerKind {
        use std::path::Path;

        if bin == "builtin" {
            return PagerKind::Builtin;
        }

        // Set to `less` by default on most Linux distros.
        let pager_bin = Path::new(bin).file_stem();

        // The name of the current running binary. Normally `bat` but sometimes
        // `batcat` for compatibility reasons.
        let current_bin = env::args_os().next();

        // Check if the current running binary is set to be our pager.
        let is_current_bin_pager = current_bin
            .map(|s| Path::new(&s).file_stem() == pager_bin)
            .unwrap_or(false);

        match pager_bin.map(|s| s.to_string_lossy()).as_deref() {
            Some("less") => PagerKind::Less,
            Some("more") => PagerKind::More,
            Some("most") => PagerKind::Most,
            _ if is_current_bin_pager => PagerKind::Bat,
            _ => PagerKind::Unknown,
        }
    }
}

/// A pager such as 'less', and from where we got it.
#[derive(Debug)]
pub(crate) struct Pager {
    /// The pager binary
    pub bin: String,

    /// The pager binary arguments (that we might tweak)
    pub args: Vec<String>,

    /// What pager this is
    pub kind: PagerKind,

    /// From where this pager comes
    pub source: PagerSource,
}

impl Pager {
    fn new(bin: &str, args: &[String], kind: PagerKind, source: PagerSource) -> Pager {
        Pager {
            bin: String::from(bin),
            args: args.to_vec(),
            kind,
            source,
        }
    }
}

/// Whether the given text (a `less` short-flag token with its leading `-`
/// already stripped) contains a real, un-consumed `-S`.
///
/// Some `less` short options take an argument that is glued to the letter
/// with no separator (`-P<prompt>`, `-o<file>`, `-t<tag>`, `-p<pattern>`,
/// `-D<color>`, ...). Once such a letter is seen, everything after it in the
/// same token is the value and must not be re-parsed as further flags — any
/// `S` inside it is coincidence, not a chop request. We walk the bundle
/// character by character and either return true when we hit `S`, return
/// false when we hit a value-taking letter (its argument follows), or ignore
/// no-arg letters. Any non-alphabetic character means the token isn't a
/// plain short-flag bundle at all.
fn short_bundle_signals_chop(bundle: &str) -> bool {
    for c in bundle.chars() {
        match c {
            'S' => return true,
            // less(1) short options that consume a glued value.
            'b' | 'D' | 'h' | 'j' | 'k' | 'o' | 'O' | 'p' | 'P' | 't' | 'T' | 'x' | 'y' | 'z'
            | '#' => return false,
            c if c.is_ascii_alphabetic() => continue,
            _ => return false,
        }
    }
    false
}

/// Whether a single pager argument selects `less`'s chop-long-lines behavior
/// (either the `-S` short flag, possibly inside a bundle like `-RSF`, or the
/// long form `--chop-long-lines`).
fn arg_signals_chop_long_lines(arg: &str) -> bool {
    if arg == "--chop-long-lines" {
        return true;
    }
    // Long options that aren't `--chop-long-lines` never signal chop, so skip
    // anything with a `--` prefix before checking short-flag bundles.
    if let Some(rest) = arg.strip_prefix("--") {
        return rest == "chop-long-lines";
    }
    if let Some(rest) = arg.strip_prefix('-') {
        return short_bundle_signals_chop(rest);
    }
    false
}

/// Whether the `LESS` environment variable's contents ask `less` to chop long
/// lines. The variable is a whitespace-separated list of options; each token
/// may or may not carry a leading `-` (e.g. `LESS=-RS` and `LESS=RS` are both
/// legal short-flag bundles).
fn less_env_signals_chop_long_lines(less: &str) -> bool {
    less.split_whitespace().any(|token| {
        if token == "--chop-long-lines" {
            return true;
        }
        if let Some(rest) = token.strip_prefix("--") {
            return rest == "chop-long-lines";
        }
        // A leading `-` is optional in $LESS; treat the token as a short-flag
        // bundle either way.
        let bundle = token.strip_prefix('-').unwrap_or(token);
        short_bundle_signals_chop(bundle)
    })
}

/// Returns true when the effective pager is `less` and it has been told to
/// chop long lines (via the pager's own arguments or the `LESS` environment
/// variable). The application uses this to skip its own line-wrapping so that
/// `less -S` actually has something to chop.
pub(crate) fn pager_will_chop_long_lines(config_pager: Option<&str>) -> bool {
    let pager = match get_pager(config_pager) {
        Ok(Some(pager)) => pager,
        _ => return false,
    };
    if pager.kind != PagerKind::Less {
        return false;
    }
    if pager.args.iter().any(|a| arg_signals_chop_long_lines(a)) {
        return true;
    }
    // `less` always merges options from the `LESS` env var with its argv, so
    // consult it regardless of whether the pager command specified args.
    env::var("LESS")
        .ok()
        .as_deref()
        .is_some_and(less_env_signals_chop_long_lines)
}

/// Returns what pager to use, after looking at both config and environment variables.
pub(crate) fn get_pager(config_pager: Option<&str>) -> Result<Option<Pager>, ParseError> {
    let bat_pager = env::var("BAT_PAGER");
    let pager = env::var("PAGER");

    let (cmd, source) = match (config_pager, &bat_pager, &pager) {
        (Some(config_pager), _, _) => (config_pager, PagerSource::Config),
        (_, Ok(bat_pager), _) => (bat_pager.as_str(), PagerSource::EnvVarBatPager),
        (_, _, Ok(pager)) => (pager.as_str(), PagerSource::EnvVarPager),
        _ => ("less", PagerSource::Default),
    };

    let parts = shell_words::split(cmd)?;
    match parts.split_first() {
        Some((bin, args)) => {
            let kind = PagerKind::from_bin(bin);

            let use_less_instead = if source == PagerSource::EnvVarPager {
                // 'more' and 'most' do not supports colors; automatically use
                // 'less' instead if the problematic pager came from the
                // generic PAGER env var.
                // If PAGER=bat, silently use 'less' instead to prevent
                // recursion.
                // Never silently use 'less' if BAT_PAGER or --pager has been
                // specified.
                matches!(kind, PagerKind::More | PagerKind::Most | PagerKind::Bat)
            } else {
                false
            };

            Ok(Some(if use_less_instead {
                let no_args = vec![];
                Pager::new("less", &no_args, PagerKind::Less, PagerSource::EnvVarPager)
            } else {
                Pager::new(bin, args, kind, source)
            }))
        }
        None => Ok(None),
    }
}

#[cfg(test)]
mod chop_detection_tests {
    use super::{arg_signals_chop_long_lines, less_env_signals_chop_long_lines};

    #[test]
    fn arg_short_flag_alone_is_chop() {
        assert!(arg_signals_chop_long_lines("-S"));
    }

    #[test]
    fn arg_short_flag_bundle_is_chop() {
        assert!(arg_signals_chop_long_lines("-RSF"));
    }

    #[test]
    fn arg_long_flag_is_chop() {
        assert!(arg_signals_chop_long_lines("--chop-long-lines"));
    }

    #[test]
    fn arg_lowercase_s_is_not_chop() {
        // `-s` is `--squeeze-blank-lines`; only `-S` chops.
        assert!(!arg_signals_chop_long_lines("-s"));
    }

    #[test]
    fn arg_unrelated_long_option_is_not_chop() {
        assert!(!arg_signals_chop_long_lines("--RAW-CONTROL-CHARS"));
    }

    #[test]
    fn less_env_bundle_without_leading_dash_is_chop() {
        // `LESS=RS` is a legal option bundle for `less`.
        assert!(less_env_signals_chop_long_lines("RS"));
    }

    #[test]
    fn less_env_reproduces_issue_2869_bundle() {
        // Exact reproduction of the `LESS` value reported in sharkdp/bat#2869.
        assert!(less_env_signals_chop_long_lines("-XQRSia"));
    }

    #[test]
    fn less_env_split_tokens_can_signal_chop() {
        assert!(less_env_signals_chop_long_lines("-R --chop-long-lines"));
    }

    #[test]
    fn less_env_without_chop_flag_returns_false() {
        assert!(!less_env_signals_chop_long_lines("-RFX"));
    }

    // Options with a glued string argument (`less -P<prompt>`, `-o<file>`,
    // `-t<tag>`, `-p<pattern>`) must not be misread as short-flag bundles
    // just because their value happens to contain an uppercase `S`.
    #[test]
    fn arg_custom_prompt_containing_s_is_not_chop() {
        assert!(!arg_signals_chop_long_lines("-PSpecial-Prompt"));
    }

    #[test]
    fn arg_log_output_path_containing_s_is_not_chop() {
        assert!(!arg_signals_chop_long_lines("-o/tmp/Session.log"));
    }

    #[test]
    fn arg_tag_value_starting_with_s_is_not_chop() {
        assert!(!arg_signals_chop_long_lines("-tSet"));
    }

    #[test]
    fn less_env_prompt_format_containing_s_is_not_chop() {
        // Custom LESS prompt format tokens (see `PROMPTS` in less(1)).
        assert!(!less_env_signals_chop_long_lines("-P%pB:S"));
    }
}

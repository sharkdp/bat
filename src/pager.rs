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

    /// A pager we don't know about
    Unknown,
}

impl PagerKind {
    fn from_bin(bin: &str) -> PagerKind {
        use std::path::Path;

        match Path::new(bin)
            .file_stem()
            .map(|s| s.to_string_lossy())
            .as_deref()
        {
            Some("bat") => PagerKind::Bat,
            Some("less") => PagerKind::Less,
            Some("more") => PagerKind::More,
            Some("most") => PagerKind::Most,
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

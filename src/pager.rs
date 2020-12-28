/// If we use a pager, this enum tells us from where we were told to use it.
#[derive(Debug, PartialEq)]
pub enum PagerSource {
    /// From --config
    Config,

    /// From the env var BAT_PAGER
    BatPagerEnvVar,

    /// From the env var PAGER
    PagerEnvVar,

    /// No pager was specified, default is used
    Default,
}

/// A pager such as 'less', and from where we got it.
pub struct Pager {
    pub pager: String,
    pub source: PagerSource,
}

impl Pager {
    fn new(pager: &str, source: PagerSource) -> Pager {
        Pager {
            pager: String::from(pager),
            source,
        }
    }
}

/// Returns what pager to use, after looking at both config and environment variables.
pub fn get_pager(pager_from_config: Option<&str>) -> Pager {
    match (
        pager_from_config,
        std::env::var("BAT_PAGER"),
        std::env::var("PAGER"),
    ) {
        (Some(config), _, _) => Pager::new(config, PagerSource::Config),
        (_, Ok(bat_pager), _) => Pager::new(&bat_pager, PagerSource::BatPagerEnvVar),
        (_, _, Ok(pager)) => Pager::new(&pager, PagerSource::PagerEnvVar),
        _ => Pager::new("less", PagerSource::Default),
    }
}

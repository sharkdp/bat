#[derive(Debug, PartialEq)]
pub enum PagerSource {
    /// From the env var BAT_PAGER
    BatPagerEnvVar,

    /// From the env var PAGER
    PagerEnvVar,

    /// From --config
    Config,

    /// No pager was specified, default is used
    Default,
}

pub struct Pager {
    pub pager: String,
    pub source: PagerSource,
}

impl Pager {
    fn new(
        pager: &str,
        source: PagerSource
    ) -> Pager {
        Pager {
            pager: String::from(pager),
            source,
        }
    }
}

pub fn get_pager(
    pager_from_config: Option<&str>,
) -> Pager {
    if pager_from_config.is_some() {
        return Pager::new(pager_from_config.unwrap(), PagerSource::Config);
    } else {
        return match (std::env::var("BAT_PAGER"), std::env::var("PAGER")) {
            (Ok(bat_pager), _) => Pager::new(&bat_pager, PagerSource::BatPagerEnvVar),
            (_, Ok(pager)) => Pager::new(&pager, PagerSource::PagerEnvVar),
            _ => Pager::new("less", PagerSource::Default),
        };
    }
}

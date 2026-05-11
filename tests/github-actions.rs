#[test]
fn all_jobs_not_missing_any_jobs() {
    let yaml: serde_yaml::Value =
        serde_yaml::from_reader(std::fs::File::open(".github/workflows/CICD.yml").unwrap())
            .unwrap();
    let jobs = yaml.get("jobs").unwrap();

    // Get all jobs that all-jobs depends on:
    //
    //   jobs:
    //     all-jobs:
    //       needs:
    //         - this
    //         - list
    //         - ...
    let actual = jobs
        .get("all-jobs")
        .unwrap()
        .get("needs")
        .unwrap()
        .as_sequence()
        .unwrap();

    // Get all jobs used in CI, except the ones we want to ignore:
    //
    //   jobs:
    //     this: ...
    //     list: ...
    //     ...
    let exceptions = [
        "all-jobs", // 'all-jobs' should not reference itself
        "winget",   // only used when publishing a release
    ];
    let expected = jobs
        .as_mapping()
        .unwrap()
        .keys()
        .filter(|k| !exceptions.contains(&k.as_str().unwrap_or_default()))
        .map(ToOwned::to_owned)
        .collect::<Vec<_>>();

    // Make sure they match
    assert_eq!(
        *actual, expected,
        "`all-jobs` should depend on all other jobs"
    );
}

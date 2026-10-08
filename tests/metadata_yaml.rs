use bat::assets_metadata::AssetsMetadata;
use bat::error::Error;
use std::fs;
use tempfile::tempdir;

const LEGACY_METADATA: &str = "bat_version: 0.26.1\ncreation_time:\n  secs_since_epoch: 1700000000\n  nanos_since_epoch: 123456789\n";

#[test]
fn metadata_written_with_the_previous_serializer_remains_compatible() {
    let directory = tempdir().unwrap();
    fs::write(directory.path().join("metadata.yaml"), LEGACY_METADATA).unwrap();
    let metadata = AssetsMetadata::load_from_folder(directory.path())
        .unwrap()
        .unwrap();
    assert!(metadata.is_compatible_with("0.26.9"));
    assert!(!metadata.is_compatible_with("0.27.0"));
    let serialized = serde_yaml::to_string(&metadata).unwrap();
    let mut original: serde_yaml::Value = serde_yaml::from_str(LEGACY_METADATA).unwrap();
    // SystemTime has platform-dependent precision (100 ns on Windows).
    // Reading an old cache must preserve the instant at that native precision.
    let timestamp = std::time::UNIX_EPOCH + std::time::Duration::new(1700000000, 123456789);
    original["creation_time"]["nanos_since_epoch"] = serde_yaml::Value::from(
        timestamp
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .subsec_nanos(),
    );
    let restored: serde_yaml::Value = serde_yaml::from_str(&serialized).unwrap();
    assert_eq!(restored, original);
}

#[test]
fn null_values_and_unknown_fields_keep_the_metadata_contract() {
    let directory = tempdir().unwrap();
    fs::write(
        directory.path().join("metadata.yaml"),
        "bat_version: null\ncreation_time: null\nfuture_field: [one, two]\n",
    )
    .unwrap();
    let metadata = AssetsMetadata::load_from_folder(directory.path())
        .unwrap()
        .unwrap();
    assert_eq!(metadata, AssetsMetadata::default());
    assert!(!metadata.is_compatible_with("0.26.1"));
}

#[test]
fn malformed_yaml_remains_a_parse_error_even_when_cache_files_exist() {
    let directory = tempdir().unwrap();
    fs::write(
        directory.path().join("metadata.yaml"),
        "bat_version: [unterminated\n",
    )
    .unwrap();
    fs::write(directory.path().join("syntaxes.bin"), b"cache placeholder").unwrap();
    match AssetsMetadata::load_from_folder(directory.path()) {
        Err(Error::SerdeYamlError(error)) => assert!(error.location().is_some()),
        other => panic!("Malformed metadata was not rejected: {other:?}"),
    }
}

#[test]
fn yaml_aliases_and_json_metadata_remain_readable() {
    let directory = tempdir().unwrap();
    for text in [
        "version: &version 0.26.1\nbat_version: *version\ncreation_time: null\n",
        r#"{"bat_version":"0.26.1","creation_time":null}"#,
    ] {
        fs::write(directory.path().join("metadata.yaml"), text).unwrap();
        let metadata = AssetsMetadata::load_from_folder(directory.path())
            .unwrap()
            .unwrap();
        assert!(metadata.is_compatible_with("0.26.1"));
    }
}

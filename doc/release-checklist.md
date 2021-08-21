# Release checklist

## Dependencies

See this page for a good overview: https://deps.rs/repo/github/sharkdp/bat

- [ ] Optional: update dependencies with `cargo update`. This is also done
      by dependabot, so it is not strictly necessary.
- [ ] Check for outdated dependencies (`cargo outdated`) and decide for each
      of them whether we want to (manually) upgrade. This will require changes
      to `Cargo.toml`.

## Version bump

- [ ] Update version in `Cargo.toml`. Run `cargo build` to update `Cargo.lock`.
      Make sure to `git add` the `Cargo.lock` changes as well.
- [ ] Find the current min. supported Rust version by running
      `grep '^\s*MIN_SUPPORTED_RUST_VERSION' .github/workflows/CICD.yml`.
- [ ] Update the version and the min. supported Rust version in `README.md` and
      `doc/README-*.md`.
- [ ] Update `CHANGELOG.md`. Introduce a section for the new release and prepare
      a new (empty) "unreleased" section at the top.

## Update syntaxes and themes (build assets)

- [ ] Install the latest master version (`cargo install -f --path .`) and make
      sure that it is available on the `PATH` (`bat --version` should show the
      new version).
- [ ] Run `assets/create.sh` and check in the binary asset files.

## Documentation

- [ ] Review the `-h` and `--help` texts
- [ ] Review the `man` page

## Pre-release checks

- [ ] Push all changes and wait for CI to succeed (before continuing with the next
      section).
- [ ] Optional: manually test the new features and command-line options. To do this,
      install the latest `bat` version again (to include the new syntaxes/themes).
- [ ] Run `cargo publish --dry-run --allow-dirty` to make sure that it will succeed
      later (after creating the GitHub release).

## Release

- [ ] Create a tag and push it to the remote `git tag vX.Y.Z; git push --tags`.
      This will trigger the deployment via GitHub Actions.
- [ ] Create a new release on GitHub: https://github.com/sharkdp/bat/releases/new
      Select the new tag and also use it as the release title. Copy the
      corresponding section from `CHANGELOG.md` and possibly add remarks for
      package maintainers. Publish the release.
- [ ] Check if the binary deployment works (archives and Debian packages should
      appear when the CI run for the Git tag has finished).
- [ ] Publish to crates.io by cloning *a fresh repository* and calling `cargo publish`.

## Post release

- [ ] Optional: inform package maintainers about the update:
    - https://www.archlinux.org/packages/community/x86_64/bat/

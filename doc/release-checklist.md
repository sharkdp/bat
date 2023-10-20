# Release checklist

## Version bump

- [ ] Update version in `Cargo.toml`. Run `cargo build` to update `Cargo.lock`.
      Make sure to `git add` the `Cargo.lock` changes as well.
- [ ] Find the current min. supported Rust version by running
      `cargo metadata --no-deps --format-version 1 | jq -r '.packages[0].rust_version'`.
- [ ] Update the version and the min. supported Rust version in `README.md` and
      `doc/README-*.md`. Check with
      `git grep -i -e 'rust.*1\.' -e '1\..*rust' | grep README | grep -v tests/`.

## CHANGELOG.md updates

- [ ] Go to https://github.com/sharkdp/bat/releases/new, click "Choose a tag",
  type the name of the tag that will be created later, click "Generate release
  notes". DO NOT ACTUALLY CREATE ANY RELEASE IN THIS STEP.
- [ ] Compare current `CHANGELOG.md` with auto-generated release notes and add
  missing entries. Expect in particular dependabot PRs to not be in
  `CHANGELOG.md` since they are [auto-merged] if CI passes.
- [ ] Introduce a section for the new release and perform final touch-ups.

## Update syntaxes and themes (build assets)

- [ ] Install the latest master version (`cargo clean && cargo install --locked -f --path .`) and make
      sure that it is available on the `PATH` (`bat --version` should show the
      new version).
- [ ] Run `assets/create.sh` and check in the binary asset files.

## Documentation

- [ ] Review [`-h`](./short-help.txt), [`--help`](./long-help.txt), and the `man` page. The `man` page is shown in
      the output of the CI job called *Documentation*, so look there.
      The CI workflow corresponding to the tip of the master branch is a good place to look.

## Pre-release checks

- [ ] Push all changes and wait for CI to succeed (before continuing with the
      next section).
- [ ] Optional: manually test the new features and command-line options. To do
      this, install the latest `bat` version again (to include the new syntaxes
      and themes).
- [ ] Run `cargo publish --dry-run` to make sure that it will
      succeed later (after creating the GitHub release).

## Release

- [ ] Create a tag and push it: `git tag vX.Y.Z; git push origin tag vX.Y.Z`.
      This will trigger the deployment via GitHub Actions.
      REMINDER: If your `origin` is a fork, don't forget to push to e.g. `upstream` instead!
- [ ] Go to https://github.com/sharkdp/bat/releases/new to create the new
      release. Select the new tag and also use it as the release title. For the
      release notes, copy the corresponding section from `CHANGELOG.md` and
      possibly add additional remarks for package maintainers.
      Publish the release.
- [ ] Check if the binary deployment works (archives and Debian packages should
      appear when the CI run for the Git tag has finished).
- [ ] Publish to crates.io by running `cargo publish` in a *clean* repository.
      The safest way to do this is to clone a fresh copy.

## Post-release

- [ ] Prepare a new "unreleased" section at the top of `CHANGELOG.md`.
      Put this at the top:

```
# unreleased

## Features

## Bugfixes

## Other

## Syntaxes

## Themes

## `bat` as a library


```

[auto-merged]: https://github.com/sharkdp/bat/blob/master/.github/workflows/Auto-merge-dependabot-PRs.yml

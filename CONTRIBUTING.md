# Contributing

Thank you for considering to contribute to `bat`!



## Add an entry to the changelog

Keeping the [`CHANGELOG.md`](CHANGELOG.md) file up-to-date makes the release
process much easier and therefore helps to get your changes into a new `bat`
release faster. However, not every change to the repository requires a
changelog entry. Below are a few examples of that.

Please update the changelog if your contribution contains changes regarding
any of the following:
  - the behavior of `bat`
  - syntax mappings
  - syntax definitions
  - themes
  - the build system, linting, or CI workflows

A changelog entry is not necessary when:
  - updating documentation
  - fixing typos

>[!NOTE]
> For PRs, a CI workflow verifies that a suitable changelog entry is
> added. If such an entry is missing, the workflow will fail. If your
> changes do not need an entry to the changelog (see above), that
> workflow failure can be disregarded.


### Changelog entry format

The top of the `CHANGELOG` contains a *"unreleased"* section with a few
subsections (Features, Bugfixes, …). Please add your entry to the subsection
that best describes your change.

Entries must follow this format:
```
- Short description of what has been changed, see #123 (@user)
```
Please replace `#123` with the number of your pull request (not issue) and
`@user` by your GitHub username.


## Development

Please check out the [Development](https://github.com/sharkdp/bat#development)
section in the README.


## Adding a new feature

Please consider opening a
[feature request ticket](https://github.com/sharkdp/bat/issues/new?assignees=&labels=feature-request&template=feature_request.md)
first in order to give us a chance to discuss the details and specifics of the potential new feature before you go and build it.


## Adding new syntaxes/languages or themes

Before you make a pull request that adds a new syntax or theme, please read
the [Customization](https://github.com/sharkdp/bat#customization) section
in the README first.

If you really think that a particular syntax or theme should be added for all
users, please read the corresponding
[documentation](https://github.com/sharkdp/bat/blob/master/doc/assets.md)
first.


## Regression tests

You are **strongly encouraged** to add regression tests. Regression tests are great,
not least because they:

* ensure that your contribution will never completely stop working.

* makes code reviews easier, because it becomes very clear what the code is
  supposed to do.

For functional changes, you most likely want to add a test to
[`tests/integration_tests.rs`](https://github.com/sharkdp/bat/blob/master/tests/integration_tests.rs).
Look at existing tests to know how to write a new test. In short, you will
invoke the `bat` binary with a certain set of arguments, and then assert on
stdout/stderr.

To learn how to write regression tests for theme and syntax changes, read the
[Syntax
tests](https://github.com/sharkdp/bat/blob/master/doc/assets.md#syntax-tests)
section in `assets.md`.

# Contributing

Thank you for considering to contribute to `bat`!



## Add an entry to the changelog

If your contribution changes the behavior of `bat` (as opposed to a typo-fix
in the documentation), please update the [`CHANGELOG.md`](CHANGELOG.md) file
and describe your changes. This makes the release process much easier and
therefore helps to get your changes into a new `bat` release faster.

The top of the `CHANGELOG` contains a *"unreleased"* section with a few
subsections (Features, Bugfixes, …). Please add your entry to the subsection
that best describes your change.

Entries follow this format:
```
- Short description of what has been changed, see #123 (@user)
```
Here, `#123` is the number of the original issue and/or your pull request.
Please replace `@user` by your GitHub username.


## Development

Please check out the [Development](https://github.com/sharkdp/bat#development)
section in the README.


## Adding a new feature

Please consider opening a
[feature request ticket](https://github.com/sharkdp/bat/issues/new?assignees=&labels=feature-request&template=feature_request.md)
first in order to give us a chance to discuss the feature first.


## Adding new syntaxes/languages or themes

Before you make a pull request that adds a new syntax or theme, please read
the [Customization](https://github.com/sharkdp/bat#customization) section
in the README first.

If you really think that a particular syntax or theme should be added for all
users, please read the corresponding
[documentation](https://github.com/sharkdp/bat/blob/master/doc/assets.md)
first.

# `/src/syntax_mapping/builtins`

The files in this directory define path/name-based syntax mappings, which amend
and take precedence over the extension/content-based syntax mappings provided by
[syntect](https://github.com/trishume/syntect).

## File organisation

Each TOML file should describe the syntax mappings of a single application, or
otherwise a set of logically-related rules.

What defines "a single application" here is deliberately vague, since the
file-splitting is purely for maintainability reasons. (Technically, we could
just as well use a single TOML file.) So just use common sense.

TOML files should reside in the corresponding subdirectory of the platform(s)
that they intend to target. At compile time, the build script will go through
each subdirectory that is applicable to the compilation target, collect the
syntax mappings defined by all TOML files, and embed them into the binary.

## File syntax

Each TOML file should contain a single section named `mappings`, with each of
its keys being a language identifier (first column of `bat -L`; also referred to
as "target").

The value of each key should be an array of strings, with each item being a glob
matcher. We will call each of these items a "rule".

For example, if `foo-application` uses both TOML and YAML configuration files,
we could write something like this:

```toml
# 30-foo-application.toml
[mappings]
"TOML" = [
    # rules for TOML syntax go here
    "/usr/share/foo-application/toml-config/*.conf",
    "/etc/foo-application/toml-config/*.conf",
]
"YAML" = [
    # rules for YAML syntax go here
    # ...
]
```

### Dynamic environment variable replacement

In additional to the standard glob matcher syntax, rules also support dynamic
replacement of environment variables at runtime. This allows us to concisely
handle things like [XDG](https://specifications.freedesktop.org/basedir-spec/latest/).

All environment variables intended to be replaced at runtime must be enclosed in
`${}`, for example `"/foo/*/${YOUR_ENV}-suffix/*.log"`. Note that this is the
**only** admissible syntax; other variable substitution syntaxes are not
supported and will either cause a compile time error, or be treated as plain
text.

For example, if `foo-application` also supports per-user configuration files, we
could write something like this:

```toml
# 30-foo-application.toml
[mappings]
"TOML" = [
    # rules for TOML syntax go here
    "/usr/share/foo-application/toml-config/*.conf",
    "/etc/foo-application/toml-config/*.conf",
    "${XDG_CONFIG_HOME}/foo-application/toml-config/*.conf",
    "${HOME}/.config/foo-application/toml-config/*.conf",
]
"YAML" = [
    # rules for YAML syntax go here
    # ...
]
```

If any environment variable replacement in a rule fails (for example when a
variable is unset), or if the glob string after replacements is invalid, the
entire rule will be ignored.

### Explicitly mapping to unknown

Sometimes it may be necessary to "unset" a particular syntect mapping - perhaps
a syntax's matching rules are "too greedy", and is claiming files that it should
not. In this case, there are two special identifiers:
`MappingTarget::MapToUnknown` and `MappingTarget::MapExtensionToUnknown`
(corresponding to the two variants of the `syntax_mapping::MappingTarget` enum).

An example of this would be `*.conf` files in general. So we may write something
like this:

```toml
# 99-unset-ambiguous-extensions.toml
[mappings]
"MappingTarget::MapExtensionToUnknown" = [
    "*.conf",
]
```

## Ordering

At compile time, all TOML files applicable to the target are processed in
lexicographical filename order. So `00-foo.toml` takes precedence over
`10-bar.toml`, which takes precedence over `20-baz.toml`, and so on. Note that
**only** the filenames of the TOML files are taken into account; the
subdirectories they are placed in have no influence on ordering.

This behaviour can be occasionally useful for creating high/low priority rules,
such as in the aforementioned example of explicitly mapping `*.conf` files to
unknown. Generally this should not be much of a concern though, since rules
should be written as specifically as possible for each application.

Rules within each TOML file are processed (and therefore matched) in the order
in which they are defined. At runtime, the syntax selection algorithm will
short-circuit and return the target of the first matching rule.

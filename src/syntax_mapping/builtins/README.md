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

At compile time, the build script will collect all the syntax mappings defined
by the TOML files within this directory, and embed them into the binary.

## File syntax

Each TOML file should contain a single section named `mappings`, with each of
its keys being a language identifier (first column of `bat -L`).

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
supported and are thus treated as plain text.

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
variable is unset), the entire rule will be ignored.

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

At compile time, all TOML files are processed in filesystem order. So
`00-foo.toml` takes precedence over `10-bar.toml`, which takes precedence over
`20-baz.toml`, and so on. This may be occasionally useful for creating high/low
priority rules, such as in the aforementioned example of explicitly mapping
`*.conf` files to unknown.

Generally this should not be much of a concern, since rules should be written as
specifically as possible for each application.

Rules within each TOML file are inserted (and therefore processed) in the order
in which they are defined.

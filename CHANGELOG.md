# unreleased

## Features


## Bugfixes


## Other


## Syntaxes


## New themes


## `bat` as a library




# v0.18.3

## Bugfixes

- Bump `git2` dependency to fix build with Rust 1.54, see #1761


# v0.18.2

## Features

- Ignore known backup/template filename suffixes when selecting the syntax, see #1687 (@scop)

## Bugfixes

- Fix for a security vulnerability on Windows. Prior to this release, `bat` would execute programs called `less`/`less.exe` from the current working directory (instead of the one from `PATH`) with priority. An attacker might be able to use this by placing a malicious program in a shared directory where the user would execute `bat`. `bat` users on Windows are advised to upgrade to this version. See #1724 and #1472 (@Ry0taK).

## Other

- Add bash completion, see #1678 (@scop)
- Fix Clippy lints, see #1661 (@mohamed-abdelnour)
- Add syntax highlighting test files, see #1213 and #1668 (@mohamed-abdelnour)

## Syntaxes

- Upgraded Julia syntax to fix a highlighting bug, see #1692
- Added support for `dash` syntax, see #1654 (@mohamed-abdelnour)
- Added support for `XAML` syntax, see #1590 and #1655 (@mohamed-abdelnour)
- Apply `DotENV` syntax also for `.env.default` and `.env.defaults` files, see #1669


# v0.18.1

## Bugfixes

- Mouse support and screen clearing broken for `less` versions with minor version number (581.2), see #1629 and #1639 (@aswild)

## Other

- `Input::ordinary_file` and `Input::with_name` now accept `Path` rather than `OsStr` see #1571 (@matklad)
- The `LESS` environment variable is now included in `bat --diagnostic`, see #1589 (@Enselic)
- Increased min. required Rust version to 1.45

## Syntaxes

- Improved the Syslog syntax highlighting, see #1606 (@keith-hall)
- Replaced "Advanced CSV" with a custom CSV syntax definition written especially for `bat`; see #1574 (@keith-hall)
- Added SystemVerilog file syntax, see #1580 (@SeanMcLoughlin)
- Added Solidity and Vyper syntax, see #1602 (@Ersikan)

## New themes

- Dark+ VS Code theme, see #1588 and #1598 (@PatriotRossii)



# v0.18.0

## Features

- Use a pager when `bat --list-languages` is called, see #1394 (@stku1985)

## Bugfixes

- If the last line doesn't end with a newline character, don't add it if `--style=plain`, see #1438 (@Enselic)
- Only print themes hint in interactive mode (`bat --list-themes`), see #1439 (@rsteube)
- Make ./tests/syntax-tests/regression_test.sh work on recent versions of macOS, see #1443 (@Enselic)
- VimL syntax highlighting fix, see #1450 (@esensar)
- Print an 'Invalid syntax theme settings' error message if a custom theme is broken, see #614 (@Enselic)
- If plain mode is set and wrap is not explicitly opted in, long lines will no be truncated, see #1426
- If `PAGER` (but not `BAT_PAGER` or `--pager`) is `more` or `most`, silently use `less` instead to ensure support for colors, see #1063 (@Enselic)
- If `PAGER` is `bat`, silently use `less` to prevent recursion. For `BAT_PAGER` or `--pager`, exit with error, see #1413 (@Enselic)
- Manpage highlighting fix, see #1511 (@keith-hall)
- `BAT_CONFIG_PATH` ignored by `bat` if non-existent, see #1550 (@sharkdp)

## Other

- Performance improvements, see #1421 (@LovecraftianHorror)
- Added a new `--diagnostic` option to collect information for bug reports, see #1459 (@sharkdp)
- Modified default theme colors to differentiate between a JSON key and a string value, see #1400 (@keith-hall)
- Upped min required Rust version to 1.42

## Syntaxes

- Added Zig syntax, see #1470 (@paulsmith)
- Added Lean syntax, see #1446 (@Julian)
- Added `.resource` extension for Robot Framework files, see #1386
- Added `gnuplot` syntax, see #1431 (@sharkdp)
- Highlight *.pac (Proxy auto-config) files as JavaScript, see #1515 (@sharkdp)

## New themes

- `ansi` replaces `ansi-dark` and `ansi-light`, see #1104 and #1412 (@mk12). **Breaking change:** users that were previously using one of the `ansi-*` themes should switch to `ansi`.
- The Gruvbox theme has been updated, see #1291 (@j0hnmeow). **Breaking change:** users that were previously using `gruvbox` or `gruvbox-white` should update and use `gruvbox-dark`/`gruvbox-light` instead.

## `bat` as a library

- The following `PrettyPrinter` methods have been removed (they were previously deprecated):
   - `input_stdin_with_name`
   - `input_from_bytes_with_name`
   - `input_from_reader_with_name`
   - `vcs_modification_markers` (if the `git` feature is not enabled)


# v0.17.1

## Bugfixes

- Running `bat` without arguments fails ("output file is also an input"), see #1396


# v0.17.0

## Features

- Added a new `--style` value, `rule`, which adds a simple horizontal ruled line between files, see #1276 (@tommilligan)
- Pass `-S` ("chop long lines") to `less` if `--wrap=never` is set in `bat`, see #1255 (@gahag)

## Bugfixes

- Detect infinite loop when input and output are the same, see #1193 and #1197 (@niklasmohrin)
- Throw an error when `bat` is being used as `pager`, see #1343 (@adrian-rivera)
- Bash syntax highlighting not selected for `*.ebuild` and `*.eclass` files, see #1292 (@sharkdp)
- Fix `zsh` completion when using `-p`, see #1320 (@xzfc)

## Other

- Add note to refer to see detailed help with `--help` (and vice versa with `-h`), see #1215 (@henil)
- Add a `Contributors` section to `README`, see #1348 (@adrian-rivera)

## Syntaxes

- Manpage syntax highlighting has been improved, see #1315 (@keith-hall)
- Add Svelte file syntax, see #1285 (@kjmph)

## New themes

- Coldark, see #1329 (@armandphilippot)


# v0.16.0

## Features

- Added support for the `NO_COLOR` environment variable, see #1021 and #1031 (@eth-p)
- Added `-P` short flag to disable paging, see #1075 and #1082 (@LordFlashmeow)
- Added `--force-colorization`/`-f` flag to provide an alias for forced color and decoration output, see #1141 (@alexanderkarlis)

## Bugfixes

- Fixed non-printable characters display for redirected output, see #1061 (@gsomix)
- Handle file extension conflicts in `--list-languages`, see #1076 and #1135 (@Kienyew)

## Other

- Switched to "·" (U+00B7) Middle Dot from "•" (U+2022) Bullet for non-printing spaces, see #1056 and #1100 (@LordFlashmeow)
- Added zsh shell completion script, see #1136 (@Kienyew)
- Improved `--help` text (@sharkdp)
- Added custom languages/themes sections to manpage (@eth-p)

## Syntaxes

- Update AsciiDoc syntax, see #1034 (@rxt1077)
- GLSL (@caioalonso)
- Add Nginx and Apache config file syntax, see #1137 (@kjmph, @niklasmohrin)
- Use `fstab` syntax for `crypttab` files, see #1073 (@sharkdp)
- Support syntax highlighting for files in `$XDG_CONFIG_HOME/git/`, see #1191 (@ahmedelgabri)

## New themes

- Gruvbox, see #1069 (@kyleondy)
- base16-256 for [base16-shell](https://github.com/chriskempson/base16-shell) users, see #1111 (@mk12)

## `bat` as a library

- Add APIs to provide `Input` descriptions with `InputDescription` (@eth-p)
- Add function to directly provide `Input`s to `PrettyPrinter` (@eth-p)
- **Breaking:** `Input::theme_preview_file` is no longer available. (@eth-p)

## Packaging

- Removed build dependency on `liquid` (@sharkdp).

# v0.15.4

## Bugfixes

- Added missing Solarized themes, see #1027
- Fixed highlighting bug in Haskell source files, see #1026

# v0.15.3

## Bugfixes

- Cannot run `bat` with relative paths, see #1022
- bat mishighlights Users that start with digits in SSH config, see #984

## New syntaxes

- SML, see #1005 (@kopecs)

## Other

- Some syntaxes and themes have been updated to the latest version

# v0.15.2

## Bugfixes

- Fix syntax detection for files called 'rails', see #1008
- Fix potential errors with syntax detection for symlinked files, see #1001
- `--map-syntax` doesn't work with names provided through `--file-name` (@eth-p)

## Other

- Add padding above headers when not using a grid, see #968 and #981 (@pt2121)
- bat now prints an error if an invalid syntax is specified via `-l` or `--map-syntax`, see #1004 (@eth-p)

## `bat` as a library

- `PrettyPrinter::vcs_modification_markers` has been marked deprecated when building without the `git` feature, see #997 and #1020 (@eth-p, @sharkdp)

## Packaging

- Compilation problems with `onig_sys` on various platforms have been resolved by upgrading to `syntect 4.2`, which includes a new `onig` version that allows to build `onig_sys` without the `bindgen` dependency. This removes the need for `libclang(-dev)` to be installed to compile `bat`. Package maintainers might want to remove `clang` as a build dependency. See #650 for more details.

# v0.15.1

## Bugfixes

- Fix highlighting of Markdown files, see #963 and #977
- Fix `base16` theme (was broken since in v0.14), see #972, #934 and #979 (@mk12).
  Users suffering from #865 ("no color for bat in ssh from a Windows client") can use the `ansi-dark` and `ansi-light` themes from now on.

## New syntaxes

- Fortran, see #957
- Email (@mariozaizar)
- QML, see #962 (@pylipp)

# v0.15.0

## Features

- Add a new `--diff`/`-d` option that can be used to only show lines surrounding
  Git changes, i.e. added, removed or modified lines. The amount of additional
  context can be controlled with `--diff-context=N`. See #23 and #940

## Bugfixes

- Error message printed in the middle of the output for another file, see #946
- Performance improvements when using custom caches (via `bat cache --build`): the `bat` startup time should now be twice as fast (@lzutao).

## Themes

- Updated version of the Solarized dark/light themes, see #941

## `bat` as a library

- There are a few changes in the "low level" API (the `Config` struct has changed and
  the error handler needs a new `&mut dyn Write` argument). The high-level API is not
  affected.

# v0.14.0

## Features

- Added a new `--file-name <name>…` option to overwrite the displayed filename(s)
  in the header. This is useful when piping input into `bat`. See #654 and #892 (@neuronull).
- Added a new `--generate-config-file` option to create an initial configuration file
  at the right place. See #870 (@jmick414)

## Bugfixes

- Performance problems with C# source code have been fixed, see #677 (@keith-hall)
- Performance problems with Makefiles have been fixed, see #750 (@keith-hall)
- Fix bug when highlighting Ruby files with unindented heredocs, see #914 (@keith-hall)
- A highlighting problem with Rust source code has been fixed, see #924 (@keith-hall)
- Windows: short files that do not require paging are displayed and then lost, see #887
- `--highlight-line` did not work correctly in combination with `--tabs=0` and `--wrap=never`,
  see #937

## Other

- When saving/reading user-provided syntaxes or themes, `bat` will now maintain a
  `metadata.yaml` file which includes information about the `bat` version which was
  used to create the cached files. When loading cached files, we now print an error
  if they have been created with an incompatible version. See #882
- Updated `liquid` dependency to 0.20, see #880 (@ignatenkobrain)

## `bat` as a library

- A completely new "high level" API has been added that is much more convenient
  to use. See the `examples` folder for the updated code. The older "low level"
  API is still available (basically everything that is not in the root `bat`
  module), but has been refactored quite a bit. It is recommended to only use
  the new "high level" API, if possible. This will be much easier to keep stable.
  Note that this should still be considered a "beta" release of `bat`-as-a-library.
  For more details and some screenshots of the example programs, see #936.
- Stripped out a lot of binary-only dependencies, see #895 and #899 (@dtolnay)

  This introduces a `features = ["application"]` which is enabled by default and pulls in
  everything required by `bat` the application. When depending on bat as a library, downstream
  `Cargo.toml` should disable this feature to cut out inapplicable heavy dependencies:
  ``` toml
  [dependencies]
  bat = { version = "0.14", default-features = false }
  ```
  Other optional functionality has also been put behind features: `paging` and `git` support.
- Allow using the library with older syntect, see #896 and #898 (@dtolnay)

## New syntaxes

- Rego, see #872 (@patrick-east)
- Stylo, see #917


# v0.13.0

## `bat` as a library

Beginning with this release, `bat` can be used as a library (#423).

This was a huge effort and I want to thank all people who made this possible: @DrSensor, @mitsuhiko, @mre, @eth-p!

- Initial attempt in #469 (@mitsuhiko)
- Second attempt, complete restructuring of the `bat` crate, see #679 (@DrSensor)
- Updates to example, public API, error handling, further refactoring: #693 #873 #875 (@sharkdp)

I want to stress that this is the very first release of the library. Things are very likely to change. A lot of things are still missing (including the documentation).

That being said, you can start using it! See the example programs in [`examples/`](https://github.com/sharkdp/bat/tree/master/examples).

You can see the API documentation here: https://docs.rs/bat/

## Features

- (**Breaking change**) Glob-based syntax mapping, see #877 and #592. With this change,
  users need to update their bat config files (`bat --config-file`), if they have any `--map-syntax` settings
  present.

  The option now works like this:
  ```bash
  --map-syntax <glob-pattern>:<syntax-name>
  ```

  For more information, see the `--help` text, the man page or the README.

  This new feature allows us to properly highlight files like:
  * `/etc/profile`
  * `~/.ssh/config`

- `--highlight-line` now accepts line ranges, see #809 (@lkalir)
- Proper wrapping support for output with wide Unicode characters, see #811 #787 and #815 (@Kogia-sima)
- A lot of updates to existing syntaxes via #644 (@benwaffle, @keith-hall)
- `BAT_CACHE_PATH` can be used to place cached `bat` assets in a non-standard path, see #829 (@neuronull)
- Support combination of multiple styles at the same time, see #857 (@aslpavel)

## Bugfixes

- Do not pass '--no-init' on newer less versions, see #749 and #786 (@sharkdp)
- 'bat cache' still takes precedence over existing files, see #666 (@sharkdp)
- `.sc` files should be treated as scala files, see #443 (@benwaffle)
- Allow underscores and dashes in page names, see #670 (@LunarLambda)
- Keep empty lines empty, see #601 (@mbarbar)
- Wrapping does not work when piping, see #758 (@fusillicode, @allevo, @gildo)
- Allow for non-unicode filenames, see #225 (@sharkdp)
- Empty file without header produces incomplete grid, see #798 (@eth-p)
- Files named `build` don't respect shebang lines, see #685 (@sharkdp)

## Other

- Parametrizable names for man page and shell completion files, see #659 #673 #656 (@eth-p)
- Enabled LTO, making `bat` about 10% faster, see #719 (@bolinfest, @sharkdp)
- Suggestions non how to configure `bat` for MacOS dark mode, see README (@jerguslejko)
- Extended ["Integration with other tools"](https://github.com/sharkdp/bat#integration-with-other-tools) section (@eth-p)
- Updated [instrutions on how to use `bat` as a `man`-pager](https://github.com/sharkdp/bat#man), see #652, see #667 (@sharkdp)
- Add section concerning file encodings, see #688 and #568 (@sharkdp)
- Updated sort order of command-line options in `--help` text and manpage, see #653 and #700 (@hrlmartins)
- Updates to the man page syntax, see #718 (@sharkdp)
- Japanese documentation updates, see #863 (@k-ta-yamada, @sorairolake and @wt-l00)
- Accept "default" as a theme, see #757 (@fvictorio)
- Updated Windows installation instructions, see #852 (@sorenbug)
- Updated man page, see #573 (@sharkdp)

## New syntaxes

- Jinja2, see #648 (@Martin819)
- SaltStack SLS, see #658 (@Martin819)
- `/etc/fstab`, see #696 (@flopp and @eth-p)
- `/etc/group` and `/etc/passwd`, see #698 (@argentite)
- `/proc/cpuinfo` and `/proc/meminfo`, see #593 (@sharkdp)
- Nim, see #542 (@sharkdp)
- Vue, see #826 (@chaaaaarlotte)
- CoffeScript, see #833 (@sharkdp)

## New themes

- Dracula, see #687 (@clarfon)
- Nord, see #760 (@crabique)
- Solarized light and dark, see #768 (@hakamadare)

## Packaging

- `bat` is now in the official Ubuntu and Debian repositories, see #323 and #705 (@MarcoFalke)
- `bat` can now be installed via MacPorts, see #675 (@bn3t)
- Install fish completions into 'vendor_completions.d', see #651 (@sharkdp)

## Thanks

- To @eth-p for joining me as a maintainer! I'm very grateful for all the work you put into
  managing and responding to issues, improving our deployment, adding PR/issue templates (#837) as
  well as fixing bugs and implementing new features.

# v0.12.1

## Bugfixes

- Fixes a bug for older Windows versions (*"The procedure entry point `CreateFile2` could not be located"*), see #643 (@rivy)

# v0.12.0

## Features

- Binary file content can now be viewed with `bat -A`, see #623, #640 (@pjsier and @sharkdp)
- `bat` can now be used as a man pager. Take a look at the README and #523 for more details.
- Add new style component to separate multiple `--line-range`s, see #570 (@eth-p)
- Added `-L` as an alias for `--list-languages`

## Bugfixes

- Output looks unbalanced when using '--style=grid,numbers' without 'header', see #571 (@eth-p)
- issues with filenames starting with "cache", see #584
- Can't build cache with new theme without creating cache dir, see #576 (@frm)
- `--terminal-width -10` is parsed incorrectly, see #611

## Other

- Added fish completions to DEB package, see #554

## New syntaxes

- Emacs Org mode, see #36 (@bricewge)
- `requirements.txt`
- DotENV `.env`
- SSH config syntax (`-l ssh_config`), see #582 (@issmirnov)
- `/etc/hosts`, see #583 (@issmirnov)
- GraphQL, see #625 (@dandavison)
- Verilog, see #616
- SCSS and Sass, see #637
- `strace` syntax, see #599

## Packaging

- `bat` is now in the official Gentoo repositories, see #588 (@toku-sa-n)
- `bat` is now in the official Alpine Linux repositories, see #586 (@5paceToast)
- `bat` is in the official Fedora repositories, see #610 (@ignatenkobrain)

# v0.11.0

## Features

- Three new special color themes are available: `ansi-light`, `ansi-dark` and `base16`. These
  are useful for people that often switch from dark to light themes in their terminal emulator
  or for people that want the colors to match their terminal theme colors. For more details,
  see #543 and #490 (@mk12, implementation idea by @trishume)
- Hand-written auto completion script for Fish shell, see #524 and #555 (@ev-dev and @eth-p)
- The `-p`/`--plain` option can now be used twice (typically `-pp`). The first `-p` switches the
  `--style` to "plain". The second `-p` disables the pager. See #560 and #552 (@eth-p)

## Bugfixes

- Do not replace arguments to `less` when using `--pager`, see #509
- Binary files will now be indicated by a warning in interactive mode, see #530 #466 #550 (@maxfilov)
- Empty files are (once again) printed with a single header line, see #504 and #566 (@reidwagner
  and @sharkdp)
- `--terminal-width=0` is now disallowed, see #559 (@eth-p)
- Accidental printing of files named `cache`, see #557

## Other

- New integration tests, see #500 and #502 (@reidwagner and @sharkdp)
- New ["Integration with other tools"](https://github.com/sharkdp/bat#integration-with-other-tools) section in the README.
- Migrated to Rust 2018 (@expobrain)

## New syntaxes

- F# syntax has been updated, see #531 (@stroborobo)
- Fish shell, see #548 (@sanga)

## Packaging

- `bat` is now available on Chocolatey, see #541 (@rasmuskriest)

# v0.10.0

## Features

- Added new `--highlight-line <N>` option, see #453, #346 and #175 (@tskinn and @sharkdp)

## Changes

- **Change the default configuration directory on macOS** to `~/.config/bat`, see #442 (@lavifb). If you are on macOS, you need to copy your configuration directory from the previous place (`~/Library/Preferences/bat`) to the new place (`~/.config/bat`).
- Completely disabled the generation of shell completion files, see #372
- Properly set arguments to `less` if `PAGER` environment variable contains something like `less -F` (which is missing the `-R` option), see #430 (@majecty)
- Report the name of missing files, see #444 (@ufuji1984)
- Don't start pager if file doesn't exist, see #387
- Rename `bat cache --init` to `bat cache --build`, see #498
- Move the `--config-dir` and `--cache-dir` options from `bat cache` to `bat` and hide them from the help text.

## Bugfixes

- Blank line at the end of output when using `--style=plain`, see #379
- EOF must be sent twice on stdin if no other input is sent, see #477 (@reidwagner)

## New syntaxes

- Twig (@ahmedelgabri)
- `.desktop` files (@jleclanche)
- AsciiDoc (@markusthoemmes)
- Assembly (x86_64 and ARM)
- Log files (@caos21)
- Protobuf and ProtobufText (@caos21)
- Terraform (@caos21)
- Jsonnet (@hfm)
- Varlink (@haraldh)

## Other

- Added Japanese version of README (@sh-tech and @object1037)
- Code improvements (@barskern)

# v0.9.0

## Features

- A new `-A`/`--show-all` option has been added to show and highlight non-printable characters (in analogy to GNU `cat`s option):

  ![](https://camo.githubusercontent.com/c3e769482ef3184f6be6adaa34bdc8d19c378254/68747470733a2f2f692e696d6775722e636f6d2f324b54486859542e706e67)

  see #395 and #381 for more details.

- Added `--pager` option (to configure the pager from the configuration file), see #362 (@majecty)

- Added `BAT_CONFIG_PATH` environment variable to set a non-default path for `bat`s configuration file, see #375 (@deg4uss3r)

- Allow for multiple occurrences of `--style` to allow for the configuration
  of styles from the config file, see #367 (@sindreij)

- Allow for multiple `--line-range` arguments, see #23

- The `--terminal-width` option can now also accept offsets, see #376

## Changes

- Use of italics is now *disabled by default* (see #389 for details). They can be
  re-enabled by adding `--italic-text=always` to your configuration file.

- The default tab-width has been set to 4.

- Added new "Sublime Snazzy" theme.

- Shell completions are currently *not* shipped anymore, see #372 for details.

## Bugfixes

- Avoid endless recursion when `PAGER="bat"`, see #383 (@rodorgas)

## Other

- `bat` is now available on openSUSE, see #405 (@dmarcoux)

- Added section about the new configuration file in the README (@deg4uss3r)

- Chinese translation of README (@chinanf-boy)

- Re-written tests for `--tabs` (@choznerol)

- Speed up integration tests, see #394

# v0.8.0

## Features

- Support for a configuration file with the following simple format:

  ```bash
  --tabs=4
  --theme="Sublime Snazzy"

  # A line-comment
  --map-syntax .ignore:.gitignore
  --map-syntax PKGBUILD:bash
  --map-syntax Podfile:ruby

  # Flags and options can also be on a single line:
  --wrap=never --paging=never
  ```

  The configuration file path can be accessed via `bat --config-file`. On Linux,
  it is stored in `~/.config/bat/config`.

- Support for the `BAT_OPTS` environment variable with the same format as specified
  above (in a single line). This takes precedence over the configuration file.

  See also #310.

- Support for custom syntax mappings via the `-m`/`--max-syntax` option.

  This allows users to (re)map certain file extensions or file names to an existing syntax:

  ``` bash
  bat --map-syntax .config:json ...
  ```

  The option can be use multiple times. Note that you can easily make these mappings permanent by using bats new configuration file.

  See #169

- Support pager command-line arguments in `PAGER` and `BAT_PAGER`, see #352 (@Foxboron)

- Add support for wildcards in Windows CMD, see #309 (@zxey)

- First-line syntax detection for all input types, see #205

- Encoding support for UTF-16LE and UTF-16BE, see #285

- New syntaxes: Robot framework (@sanga)

## Changes

- Binary files are now detected and not displayed when the output goes to an interactive terminal, see #205

## Bugfixes

- JavaDoc comments break syntax highlighting in .java files, see #81

- Bat Panics on Haskell Source Code, see #314

## Other

- Better `-h` and `--help` texts.

- Updated documentation on how to configure `bat`s pager

- Updated documentation for light backgrounds, see #328 (@russtaylor)

- Generate shell completions during build, see #115 (@davideGiovannini)

- A lot of new tests have been written

- `bat` is now available via [Termux](https://termux.com/), see #341 (@fornwall)

- `bat` is now available via [nix](https://nixos.org/nix), see #344 (@mgttlinger)

- `bat` is now available via [Docker](https://hub.docker.com/r/danlynn/bat/), see #331 (@danlynn)

# v0.7.1

## Features

- Use the `ansi_colours` package by @mina86 for better true-color approximation on 8 bit color terminals, see #319 and #202.

## Bugfixes

- Bat Panics on Haskell Source Code, see #314
- Disable wrapping when `--style=plain`/`-p` is used, see #289

## Other

- Added Ansible install instructions (@aeimer)
- Added section about Cygwin to the README (@eth-p)

# v0.7.0

## Features

- Tabs are now (optionally) expanded to spaces. This can be controlled with the new
  `--tabs` command-line option or the `BAT_TABS` environment variable. The
  new feature also closes two bugs #166 and #184. For more information, see #302 (@eth-p).

- Added support for the `BAT_STYLE` environment variable, see #208 (@ms2300)

- Added `OneHalf` theme for terminals with a light-gray background, see #256

- Added new syntaxes for CSV, JSX in JavaScript and TypeScript, Cabal, Dart,
  F#, PureScript, Swift, Crystal, PowerShell (Many Thanks to @tobenna and @mimadrid)

## Changes

- Query `git diff` only when needed, see #303 (@ShikChen)

- Disable wrapping when `--plain` is used, see #289 (@eth-p)

## Bugfixes

- Can read files named `cache`, see #275 (@BK1603)

- A lot of bugfixes for Windows, see #252, #264

- Detect `less` reliably and in a portable way, see #271 and #290 (@Aankhen)

- last decoration line is not formatted properly with `--wrap never`, see #299 (@Rogach)

- Do not show file header for directories, see #292

## Other

- Enabled a new `aarch64` build target, see #258 (@rachitchokshi)

- Provide Debian packages for `armhf`, see #280 (@rachitchokshi)

- Added README section about "`bat` on Windows" (@Aankhen)

- Windows installation via scoop (@meltinglava)

# v0.6.1

## Bugfixes

- Fixed panic when running `bat --list-languages | head`, see #232 (@mchlrhw)
- Respect `--color` settings for `--list-themes` and `--list-languages`, see #233
- Git modifications now work on Windows

## Other

- There will be auto-generated Windows releases, starting with this version (@anykao)

# v0.6.0

## Features

- The `--list-themes` option now shows a preview for each highlighting theme (@ms2300)
- Added `-p`/`--plain` as an alias for `--style=plain`, see #212 (@ms2300)
- Major refactorings, enabling some progress on #150. In non-interactive mode, `bat` will now copy input bytes 1:1.
- New languages: Elm, Kotlin, Puppet, TypeScript, see #215 #216 #217 #218
- New syntax highlighting theme: zenburn (@colindean)

## Changes

- New themes in `$BAT_CONFIG_DIR/themes` are now loaded *in addition* to
  the default themes (they may also override), see #172
- The `Default.tmTheme` symlink is not necessary anymore.

## Bugfixes

* Using `bat cache --init` leads to duplicated syntaxes, see #206

## Other

* Extended and cleaned-up `--help` text.
* Added initial version of a man page, see #52
* New README sections: *Development* and *Troubleshooting*, see #220

# v0.5.0

## Features

- Added `--line-range n:m` option to print a range of lines, see #159 (@tskinn)
- The syntax highlighting theme can now be controlled by the `BAT_THEME` environment variable, see [README](https://github.com/sharkdp/bat#highlighting-theme) and #177 (@mandx)
- The `PAGER` and `BAT_PAGER` environment variables can be used to control the pager that `bat` uses, see #158 and the [new README section](https://github.com/sharkdp/bat#using-a-different-pager)
- Added syntax highlighting for Nix, see #180
- Added syntax highlighting for AWK (Gert Hulselmans)

## Changes

- The customization of syntax sets and theme sets is now separated. Syntax definitions are now loaded *in addition* to the ones that are stored in the `bat` binary by default. Please refer to these new sections in the README: [Adding new syntaxes](https://github.com/sharkdp/bat#adding-new-syntaxes--language-definitions), [Adding new themes](https://github.com/sharkdp/bat#adding-new-themes), also see #172
- The color for the filename is now the default foreground color. The colors for the grid and the line numbers is now determined from the syntax highlighting theme, which now also works for light backgrounds, see #178.

## Bugfixes

- Escape Sequences get partially stripped, see #182 (@eth-p)
- Use separate Git repository for snapshot testing, see #165 and #161
- Markdown breaking on JavaScript, see #183

## Other

- Binaries for armv7 are now provided, see #196
- `bat` is now in the official [Arch package repositories](https://www.archlinux.org/packages/community/x86_64/bat/).
- Optimizations in the RGB => 8-bit conversion (@mina86)

# v0.4.1

(this is just a small bugfix release, see 0.4.0 for all features and changes)

## Bugfixes

- Fix problem with `cargo test` when `bat` is not checked out in a Git repository, see #161

# v0.4.0

## Features

* Support for line-wrapping, see #54 and #102 (@eth-p)
* New and updated `--style` parameter, see #74 and README (@pitkley)
* Added `--theme` and `--list-themes` options, see #89 (@rleungx)
* Added syntax highlighting for: Julia (@iamed2), Dockerfiles, VimL, CMake, INI, Less
* Added a few popular Sublime Text highlighting themes, see #133
* Support for bold, italic and underline font styles, see #96
* Support for 32bit systems is now available, see #84
* Added `-u` and `-n` options, see #134
* ANSI color support on Windows 10

## Changes

* The customization folder for own syntaxes has been renamed from `syntax` to `syntaxes`, see README.
* Changed Markdown syntax to the default Sublime Text syntax, see #157
* Sorted language listing (@rleungx)
* Command line arguments like `--theme` or `--color` can now override themselves.
* Improved `--help` text.

## Bugfixes

- Fixed crash for (really) small terminal sizes, see #117 (@eth-p)
- Syntax detection for `.bashrc`, `CMakeLists.txt` etc., see #100
- Properly handle lines with invalid UTF-8, see #7 (@BrainMaestro)
- Better error handling, see #17 (@rleungx and @sharkdp)
- Proper handling of UTF-8 characters in `less`, see #98 (@ghuls)
- Build fix on nightly, see #148 (@tathanhdinh)

## Other

- [Comparison with alternative projects](https://github.com/sharkdp/bat/blob/master/doc/alternatives.md).
- New "bat" logo in the README, see #119 (@jraulhernandezi)
- Output test cases (@BrainMaestro)
- Lots of great refactoring work (@BrainMaestro)

# v0.3.0

## Features

* Automatic paging by integrating with `less`, see #29 (@BrainMaestro)
* Added support for reading from standard input, see #2
* Added support for writing to non-interactive terminals (pipes, files, ..); new
  `--color=auto/always/never` option, see #26 (@BrainMaestro)
* Added `--list-languages` option to print all available syntaxes, see #69 (@connorkuehl)
* New option to specify the syntax via `-l`/`--language`, see #19 (@BrainMaestro)
* New option to control the output style (`--style`), see #5 (@nakulcg)
* Added syntax highlighting support for TOML files, see #37

## Changes

* The `init-cache` sub-command has been removed. The cache can now be controlled via
  `bat cache`. See `bat cache -h` for all available commands.

## Bug fixes

* Get git repository from file path instead of current directory, see #22 (@nakulcg)
* Process substitution can now be used with bat (`bat <(echo a) <(echo b)`), see #80

## Thanks

I'd like to say a big THANK YOU to all contributors and everyone that has given us
some form of feedback.

Special thanks go to @BrainMaestro for his huge support with new features, bug reports
and code reviews!

# v0.2.3

- Added a new statically linked version of bat (`..-musl-..`)

# v0.2.2

- Remove openssl dependency completely, see #30.

# v0.2.1

- Added Elixir syntax, see #25.
- Use libcurl-openssl instead of libcurl-gnutls, see #30.

# v0.2.0

- Support for custom syntaxes, add 'Markdown extended' theme
- Bugfix: Git modifications not shown from child folder

# v0.1.0

Initial release

<p align="center">
  <img src="doc/logo-header.svg" alt="bat - a cat clone with wings"><br>
  <a href="https://travis-ci.org/sharkdp/bat"><img src="https://travis-ci.org/sharkdp/bat.svg?branch=master" alt="Build Status"></a>
  <a href="https://ci.appveyor.com/project/sharkdp/bat"><img src="https://ci.appveyor.com/api/projects/status/cptsmtbiwbnr2vhf?svg=true"></a>
  <img src="https://img.shields.io/crates/l/bat.svg" alt="license">
  <a href="https://crates.io/crates/bat"><img src="https://img.shields.io/crates/v/bat.svg?colorB=319e8c" alt="Version info"></a><br>
  A <i>cat(1)</i> clone with syntax highlighting and Git integration.
</p>

<p align="center">
  <a href="#syntax-highlighting">Key Features</a> •
  <a href="#how-to-use">How To Use</a> •
  <a href="#installation">Installation</a> •
  <a href="#customization">Customization</a> •
  <a href="#project-goals-and-alternatives">Project goals, alternatives</a> •
  Translation [<a href="https://github.com/chinanf-boy/bat-zh">中文</a>]
</p>

### Syntax highlighting

`bat` supports syntax highlighting for a large number of programming and markup
languages:

![Syntax highlighting example](https://imgur.com/rGsdnDe.png)

### Git integration

`bat` communicates with `git` to show modifications with respect to the index
(see left side bar):

![Git integration example](https://i.imgur.com/2lSW4RE.png)

### Show non-printable characters

You can use the `-A`/`--show-all` option to show and highlight non-printable
characters:

![Non-printable character example](https://i.imgur.com/WndGp9H.png)

### Automatic paging

`bat` can pipe its own output to `less` if the output is too large for one screen.

### File concatenation

Oh.. you can also use it to concatenate files :wink:. Whenever
`bat` detects a non-interactive terminal (i.e. when you pipe into another process
or into a file), `bat` will act as a drop-in replacement for `cat` and
fall back to printing the plain file contents.

## How to use

Display a single file on the terminal

```bash
> bat README.md
```

Display multiple files at once

```bash
> bat src/*.rs
```

Read from stdin, determine the syntax automatically

```bash
> curl -s https://sh.rustup.rs | bat
```

Read from stdin, specify the language explicitly

```bash
> yaml2json .travis.yml | json_pp | bat -l json
```

Show and highlight non-printable characters:
```bash
> bat -A /etc/hosts
```

Use it as a `cat` replacement:

```bash
bat > note.md  # quickly create a new file

bat header.md content.md footer.md > document.md

bat -n main.rs  # show line numbers (only)

bat f - g  # output 'f', then stdin, then 'g'.
```

## Installation

### On Ubuntu
*... and other Debian-based Linux distributions.*

Download the latest `.deb` package from the [release page](https://github.com/sharkdp/bat/releases)
and install it via:
``` bash
sudo dpkg -i bat_0.9.0_amd64.deb  # adapt version number and architecture
```

### On Arch Linux

You can install [the `bat` package](https://www.archlinux.org/packages/community/x86_64/bat/)
from the official sources:

```bash
pacman -S bat
```

### On Void Linux

You can install `bat` via xbps-install:
```
xbps-install -S bat
```

### On FreeBSD

You can install a precompiled [`bat` package](https://www.freshports.org/textproc/bat) with pkg:

```bash
pkg install bat
```

or build it on your own from the FreeBSD ports:

```bash
cd /usr/ports/textproc/bat
make install
```

### Via nix

You can install `bat` using the [nix package manager](https://nixos.org/nix):

```bash
nix-env -i bat
```

### On openSUSE

You can install `bat` with zypper:

```
zypper install bat
```

### On macOS

You can install `bat` with [Homebrew](http://braumeister.org/formula/bat):

```bash
brew install bat
```

### On Windows

You can download prebuilt binaries from the [Release page](https://github.com/sharkdp/bat/releases),
or install it with [scoop](https://scoop.sh/):

```bash
scoop install bat
```

[See below](#using-bat-on-windows) for notes.

### Via Docker

There is a [Docker image](https://hub.docker.com/r/danlynn/bat/) that you can use to run `bat` in a container:
```bash
docker pull danlynn/bat
alias bat='docker run -it --rm -e BAT_THEME -e BAT_STYLE -e BAT_TABS -v "$(pwd):/myapp" danlynn/bat'
```

### Via Ansible

You can install `bat` with [Ansible](https://www.ansible.com/):

```bash
# Install role on local machine
ansible-galaxy install aeimer.install_bat
```

```yaml
---
# Playbook to install bat
- host: all
  roles:
    - aeimer.install_bat
```

- [Ansible Galaxy](https://galaxy.ansible.com/aeimer/install_bat)
- [GitHub](https://github.com/aeimer/ansible-install-bat)

This should work with the following distributions:
- Debian/Ubuntu
- ARM (eg. Raspberry PI)
- Arch Linux
- Void Linux
- FreeBSD
- MacOS

### From binaries

Check out the [Release page](https://github.com/sharkdp/bat/releases) for
prebuilt versions of `bat` for many different architectures. Statically-linked
binaries are also available: look for archives with `musl` in the file name.

### From source

If you want to build `bat` from source, you need Rust 1.29 or
higher. You can then use `cargo` to build everything:

```bash
cargo install bat
```

You may have to install `cmake` and the `libz` development package
(`libz-dev` or `libz-devel`) in order for the build to succeed.

## Customization

### Highlighting theme

Use `bat --list-themes` to get a list of all available themes for syntax
highlighting. To select the `TwoDark` theme, call `bat` with the
`--theme=TwoDark` option or set the `BAT_THEME` environment variable to
`TwoDark`. Use `export BAT_THEME="TwoDark"` in your shell's startup file to
make the change permanent. Alternatively, use `bat`s
[configuration file](https://github.com/sharkdp/bat#configuration-file).

If you want to preview the different themes on a custom file, you can use
the following command (you need [`fzf`](https://github.com/junegunn/fzf) for this):
``` bash
bat --list-themes | fzf --preview="bat --theme={} --color=always /path/to/file"
```

`bat` looks good on a dark background by default. However, if your terminal uses a
light background, some themes like `GitHub` or `OneHalfLight` will work better for you.
You can also use a custom theme by following the
['Adding new themes' section below](https://github.com/sharkdp/bat#adding-new-themes).

### Output style

You can use the `--style` option to control the appearance of `bat`s output.
You can use `--style=numbers,changes`, for example, to show only Git changes
and line numbers but no grid and no file header. Set the `BAT_STYLE` environment
variable to make these changes permanent or use `bat`s
[configuration file](https://github.com/sharkdp/bat#configuration-file).

### Adding new syntaxes / language definitions

`bat` uses the excellent [`syntect`](https://github.com/trishume/syntect/)
library for syntax highlighting. `syntect` can read any
[Sublime Text `.sublime-syntax` file](https://www.sublimetext.com/docs/3/syntax.html)
and theme. To add new syntax definitions, do the following.

Create a folder with syntax definition files:

```bash
BAT_CONFIG_DIR="$(bat cache --config-dir)"

mkdir -p "$BAT_CONFIG_DIR/syntaxes"
cd "$BAT_CONFIG_DIR/syntaxes"

# Put new '.sublime-syntax' language definition files
# in this folder (or its subdirectories), for example:
git clone https://github.com/tellnobody1/sublime-purescript-syntax
```

Now use the following command to parse these files into a binary cache:

```bash
bat cache --init
```

Finally, use `bat --list-languages` to check if the new languages are available.

If you ever want to go back to the default settings, call:

```bash
bat cache --clear
```

### Adding new themes

This works very similar to how we add new syntax definitions.

First, create a folder with the new syntax highlighting themes:
```bash
BAT_CONFIG_DIR="$(bat cache --config-dir)"

mkdir -p "$BAT_CONFIG_DIR/themes"
cd "$BAT_CONFIG_DIR/themes"

# Download a theme in '.tmTheme' format, for example:
git clone https://github.com/greggb/sublime-snazzy

# Update the binary cache
bat cache --init
```

Finally, use `bat --list-themes` to check if the new themes are available.

### Using a different pager

`bat` uses the pager that is specified in the `PAGER` environment variable. If this variable is not
set, `less` is used by default. If you want to use a different pager, you can either modify the
`PAGER` variable or set the `BAT_PAGER` environment variable to override what is specified in
`PAGER`.

If you want to pass command-line arguments to the pager, you can also set them via the
`PAGER`/`BAT_PAGER` variables:

```bash
export BAT_PAGER="less -RF"
```

Instead of using environment variables, you can also use `bat`s [configuration file](https://github.com/sharkdp/bat#configuration-file) to configure the pager (`--pager` option).

**Note**: By default, if the pager is set to `less` (any no command-line options are specified),
`bat` will pass the following command line
options to the pager: `-R`/`--RAW-CONTROL-CHARS`, `-F`/`--quit-if-one-screen` and `-X`/`--no-init`.
The first (`-R`) is needed to interpret ANSI colors correctly. The second option (`-F`) instructs
less to exit immediately if the output size is smaller than the vertical size of the terminal.
This is convenient for small files because you do not have to press `q` to quit the pager. The
third option (`-X`) is needed to fix a bug with the `--quit-if-one-screen` feature in old versions
of `less`. Unfortunately, it also breaks mouse-wheel support in `less`. If you want to enable
mouse-wheel scrolling, you can either pass just `-R` (as in the example above, this will disable
the quit-if-one-screen feature), or you can use a recent version of `less` and pass `-RF` which
will hopefully enable both quit-if-one-screen and mouse-wheel scrolling.

If scrolling still doesn't work for you, you can try to pass the `-S` option in addition.

## Configuration file

`bat` can also be customized with a configuration file. The location of the file is dependent
on your operating system. To get the default path for your system, call
```
bat --config-file
```

Alternatively, you can use the `BAT_CONFIG_PATH` environment variable to point `bat` to a
non-default location of the configuration file:
```bash
export BAT_CONFIG_PATH="/path/to/bat.conf"
```

### Format

The configuration file is a simple list of command line arguments. Use `bat --help` to see a full list of possible options and values. In addition, you can add comments by prepending a line with the `#` character.

Example configuration file:
```bash
# Set the theme to "TwoDark"
--theme="TwoDark"

# Show line numbers, Git modifications and file header (but no grid)
--style="numbers,changes,header"

# Use italic text on the terminal (not supported on all terminals)
--italic-text=always

# Add mouse scrolling support in less (does not work with older
# versions of "less")
--pager="less -FR"

# Use C++ syntax (instead of C) for .h header files
--map-syntax h:cpp

# Use "gitignore" highlighting for ".ignore" files
--map-syntax .ignore:.gitignore
```

## Using `bat` on Windows

`bat` mostly works out-of-the-box on Windows, but a few features may need extra configuration.

### Paging

Windows only includes a very limited pager in the form of `more`. You can download a Windows binary
for `less` [from its homepage](http://www.greenwoodsoftware.com/less/download.html) or [through
Chocolatey](https://chocolatey.org/packages/Less). To use it, place the binary in a directory in
your `PATH` or [define an environment variable](#using-a-different-pager).

### Colours

Windows 10 natively supports colours in both `conhost.exe` (Command Prompt) and PowerShell since
[v1511](https://en.wikipedia.org/wiki/Windows_10_version_history#Version_1511_(November_Update)), as
well as in newer versions of bash. On earlier versions of Windows, you can use
[Cmder](http://cmder.net/), which includes [ConEmu](https://conemu.github.io/).

**Note:** The Git and MSYS versions of `less` do not correctly interpret colours on Windows. If you
don’t have any other pagers installed, you can disable paging entirely by passing `--paging=never`
or by setting `BAT_PAGER` to an empty string.

### Cygwin

`bat` on Windows does not natively support Cygwin's unix-style paths (`/cygdrive/*`). When passed an absolute cygwin path as an argument, `bat` will encounter the following error: `The system cannot find the path specified. (os error 3)`

This can be solved by creating a wrapper or adding the following function to your `.bash_profile` file:

```shell
bat() {
    local index
    local args=("$@")
    for index in $(seq 0 ${#args[@]}) ; do
        case "${args[index]}" in
        -*) continue;;
        *)  [ -e "${args[index]}" ] && args[index]="$(cygpath --windows "${args[index]}")";;
        esac
    done
    command bat "${args[@]}"
}
```

## Troubleshooting

### Terminals & colors

`bat` handles terminals *with* and *without* truecolor support. However, the colors in the syntax
highlighting themes are not optimized for 8-bit colors and it is therefore strongly recommended
that you use a terminal with 24-bit truecolor support (`terminator`, `konsole`, `iTerm2`, ...).
See [this article](https://gist.github.com/XVilka/8346728) for more details and a full list of
terminals with truecolor support.

Make sure that your truecolor terminal sets the `COLORTERM` variable to either `truecolor` or
`24bit`. Otherwise, `bat` will not be able to determine whether or not 24-bit escape sequences
are supported (and fall back to 8-bit colors).

### Line numbers and grid are hardly visible

Please try a different theme (see `bat --list-themes` for a list). The `OneHalfDark` and
`OneHalfLight` themes provide grid and line colors that are brighter.

## Development

```bash
# Recursive clone to retrieve all submodules
git clone --recursive https://github.com/sharkdp/bat

# Build (debug version)
cd bat
cargo build

# Run unit tests and integration tests
cargo test

# Install (release version)
cargo install

# Build a bat binary with modified syntaxes and themes
bash assets/create.sh
cargo install -f
```

## Project goals and alternatives

`bat` tries to achieve the following goals:

- Provide beautiful, advanced syntax highlighting
- Integrate with Git to show file modifications
- Be a drop-in replacement for (POSIX) `cat`
- Offer a user-friendly command-line interface

There are a lot of alternatives, if you are looking for similar programs. See
[this document](doc/alternatives.md) for a comparison.

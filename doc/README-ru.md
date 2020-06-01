<p align="center">
  <img src="doc/logo-header.svg" alt="bat - a cat clone with wings"><br>
  <a href="https://travis-ci.org/sharkdp/bat"><img src="https://travis-ci.org/sharkdp/bat.svg?branch=master" alt="Build Status"></a>
  <a href="https://ci.appveyor.com/project/sharkdp/bat"><img src="https://ci.appveyor.com/api/projects/status/cptsmtbiwbnr2vhf/branch/master?svg=true"></a>
  <img src="https://img.shields.io/crates/l/bat.svg" alt="license">
  <a href="https://crates.io/crates/bat"><img src="https://img.shields.io/crates/v/bat.svg?colorB=319e8c" alt="Version info"></a><br>
  Клон утилиты <i>cat(1)</i> с поддержкой выделения синтаксиса и Git
</p>

<p align="center">
  <a href="#syntax-highlighting">Главные преимущества</a> •
  <a href="#how-to-use">Использование</a> •
  <a href="#installation">Установка</a> •
  <a href="#customization">Кастомизация</a> •
  <a href="#project-goals-and-alternatives">Цели проекта и его альтернативы </a> •
  Translation [<a href="https://github.com/chinanf-boy/bat-zh">中文</a>][<a href="doc/README-ja.md">日本語</a>][<a href="doc/README-ko.md">한국어</a>][<a href="doc/README-ru.md">日本語</a>][<a href="doc/README-ko.md">Русский</a>]
</p>

### Выделение синтаксиса

`bat` поддерживает выделение синтаксиса для огромного количества языков программирования и разметки:

![Пример выделения синтаксиса](https://i.imgur.com/3FGy5tW.png)

### Интеграция с Git
`bat` использует `git` чтобы показать изменения в коде
(смотрите на левый сайдбар):

![Пример интеграции с Git](https://i.imgur.com/azUAzdx.png)

### Show non-printable characters

Вы можете использовать `-A` / `--show-all` флаг чтобы показать символы, которые невозможно напечатать:

![Строка с неотображемыми символами](https://i.imgur.com/X0orYY9.png)

### Автоматическое разделение текста

`bat` умеет траслировать вывод в `less` если вывод слишком большой, чтобы уместится на экране целиком

### Объединение файлов

О..вы так же можете объединять файлы :wink:. Всегда когда
`bat` обнаруживает неинтерактивный терминал (например когда вы направляете вывод в файл или процесс), `bat` будет работать как вставная замена `cat` и откатится к выводу это файла как обычного текста

## Как использовать

Вывести единственный файл в терминале

```bash
> bat README.md
```

Отобразить сразу несколько файлов в терминале

```bash
> bat src/*.rs
```

Читаем из stdin и определяем синтаксис автоматически

```bash
> curl -s https://sh.rustup.rs | bat
```

Читает из stdin и указываем что это за язык

```bash
> yaml2json .travis.yml | json_pp | bat -l json
```

Вывести и выделить неотображаемые символы
```bash
> bat -A /etc/hosts
```

Использование как замена `cat`

```bash
bat > note.md  # мгновенно создаем новый файл

bat header.md content.md footer.md > document.md

bat -n main.rs  # показываем только количество строк

bat f - g  # выводит 'f' в stdin, а потом 'g'.
```

### Интеграция с другими утилитами

#### `find` или `fd`

Вы можете использовать флаг `-exec` в `find` чтобы посмотреть превью всех файлов в `bat`
```bash
find … -exec bat {} +
```

Если вы используете [`fd`](https://github.com/sharkdp/fd), вы вполне можете использоваль флаг  `-X`/`--exec-batch` чтобы сделать тоже самое:
```bash
fd … -X bat
```

#### `ripgrep`

С помощью [`batgrep`](https://github.com/eth-p/bat-extras/blob/master/doc/batgrep.md), `bat` может быть использован для вывода результата запроса [`ripgrep`](https://github.com/BurntSushi/ripgrep)

```bash
batgrep needle src/
```

#### `tail -f`

`bat` может быть использован с  `tail -f` чтобы просматривать файл вместе с выделением синтаксиса
```bash
tail -f /var/log/pacman.log | bat --paging=never -l log
```
Заметье, что мы отключаем пэйджинг чтобы это заработало. Мы так-же указываем синтаксис (`-l log`), так как он не может быть определен в данном случае.

#### `git`

Вы можете использовать `bat` с `git show` чтобы просмотреть старую версию файла с выделением синтаксиса:
```bash
git show v0.6.0:src/main.rs | bat -l rs
```

Обратите внимание, что выделение синтаксиса не работает в `git diff` на данный момент. Если вам это нужно, посмотрите [`delta`](https://github.com/dandavison/delta).

#### `xclip`

Количество строк и просмотр изменений в `bat` может сделать копирование содержания файла немного сложной. Чтобы предотвратить это, используйте флаг `-p`/`--plain` или просто перевести вывод в `xclip`:
```bash
bat main.cpp | xclip
```
`bat` обнаружит, что вывод будет переведен, и выведет обычный текст без выделения синтаксиса.

#### `man`

`bat` может быть использован в виде выделения цвета для  `man`, для этого установите переменную окружения
`MANPAGER`:

```bash
export MANPAGER="sh -c 'col -bx | bat -l man -p'"
man 2 select
```

Возможно вам понадобится также установить `MANROFFOPT="-c"` если у вас есть проблемы с форматированием.

Если вы хотите сделать этой одной командной, вы можете использовать `batman`](https://github.com/eth-p/bat-extras/blob/master/doc/batman.md).

Обратите внимание, что [синтаксис manpage](assets/syntaxes/02_Extra/Manpage.sublime-syntax) разрабатывается в этом репозитории, и все еще находится в разработке.

#### `prettier` / `shfmt` / `rustfmt`

[`Prettybat`](https://github.com/eth-p/bat-extras/blob/master/doc/prettybat.md) это скрипт, который форматирует код и выводит его с помощью `bat`.


## Установка

[![Packaging status](https://repology.org/badge/vertical-allrepos/bat.svg)](https://repology.org/project/bat/versions)

### Ubuntu (с помощью `apt`)
*... и другие дистрибутивы основанные на Debian.*

`bat` есть в репозиториях [Ubuntu](https://packages.ubuntu.com/eoan/bat) и
[Debian](https://packages.debian.org/sid/bat), и доступен начиная с Ubuntu Eoan 19.10. На Debian `bat` пока-что доступен только с нестабильной веткой "Sid"

Если ваша версия Ubuntu/Debian достаточно новая, вы можете установить `bat` так:

```bash
apt install bat
```

Если вы установили `bat` таким образом, то бинарный файл может быть установлен как `batcat` заместо `bat` (из-за [имя конфлиует с другим пакетом](https://github.com/sharkdp/bat/issues/982)). Вы можете сделать симлинк или алиас `bat -> batcat` чтобы предотвратить проблемы которые могут произойти с другими дистрибутивами.

``` bash
mkdir -p ~/.local/bin
ln -s /usr/bin/batcat ~/.local/bin/bat
```

### Ubuntu (С помощью самоно нового `.deb` пакета)
*... и другие дистрибутивы Linux основанные на Debian*

Если пакет еще не был предоставлен в вашем Ubuntu/Debian или вы хотите самую последнюю версия `bat`, вы можете скачать самый последний `deb` пакет отсюда:
[release page](https://github.com/sharkdp/bat/releases) и установить так:

```bash
sudo dpkg -i bat_0.15.4_amd64.deb  # измените архитектуру и версию
```

### Alpine Linux

Вы можете установить [`bat`](https://pkgs.alpinelinux.org/packages?name=bat) из оффициальных источников:

```bash
apk add bat
```

### Arch Linux

Вы можете установить [`bat`](https://www.archlinux.org/packages/community/x86_64/bat/) из оффициального источника:

```bash
pacman -S bat
```

### Fedora

Вы можете установить [`bat`](https://koji.fedoraproject.org/koji/packageinfo?packageID=27506) из оффициального репозитория [Fedora Modular](https://docs.fedoraproject.org/en-US/modularity/using-modules/).

```bash
dnf install bat
```

### Gentoo Linux

Вы можете установить [`bat`](https://packages.gentoo.org/packages/sys-apps/bat) из оффициальных источников:

```bash
emerge sys-apps/bat
```

### Void Linux

Вы можете установить `bat` с помощью xbps-install:
```bash
xbps-install -S bat
```

### FreeBSD

Вы можете установить [`bat`](https://www.freshports.org/textproc/bat) с помощью pkg:

```bash
pkg install bat
```

или самому скомпилировать его:

```bash
cd /usr/ports/textproc/bat
make install
```

### С помощью nix

Вы можете установить `bat` используя [nix package manager](https://nixos.org/nix):

```bash
nix-env -i bat
```

### openSUSE

Вы можете установить`bat` с помощью zypper:

```bash
zypper install bat
```

### On macOS

Вы можете установить`bat` с помощью [Homebrew](http://braumeister.org/formula/bat):

```bash
brew install bat
```

Или-же установить его с помощью [MacPorts](https://ports.macports.org/port/bat/summary):

```bash
port install bat
```

### On Windows

Есть несколько способов установить `bat`. Как только вы установили его, посмотрите на секцию ["Using `bat` on Windows"](#using-bat-on-windows).

#### С помощью Chocolatey

Вы можете установить `bat` с помощью [Chocolatey](https://chocolatey.org/packages/Bat):
```bash
choco install bat
```

#### With Scoop

Вы можете установить `bat` с помощью [scoop](https://scoop.sh/):
```bash
scoop install bat
```

Для этого у вас должен быть установлен [Visual C++ Redistributable](https://support.microsoft.com/en-us/help/2977003/the-latest-supported-visual-c-downloads).

#### Из заранее скомпилированных файлов:

Их вы можете скачать на [странице релизов](https://github.com/sharkdp/bat/releases),

Для этого у вас должен быть установлен [Visual C++ Redistributable](https://support.microsoft.com/en-us/help/2977003/the-latest-supported-visual-c-downloads).

### С помощью Docker

Вы можете использовать [Docker image](https://hub.docker.com/r/danlynn/bat/) чтобы запустить `bat` в контейнере:
```bash
docker pull danlynn/bat
alias bat='docker run -it --rm -e BAT_THEME -e BAT_STYLE -e BAT_TABS -v "$(pwd):/myapp" danlynn/bat'
```

### С помощью Ansible

Вы можете установить `bat` с [Ansible](https://www.ansible.com/):

```bash
# Устанавливаем роль на устройстве
ansible-galaxy install aeimer.install_bat
```

```yaml
---
# Playbook для установки bat
- host: all
  roles:
    - aeimer.install_bat
```

- [Ansible Galaxy](https://galaxy.ansible.com/aeimer/install_bat)
- [GitHub](https://github.com/aeimer/ansible-install-bat)

Этот способ должен сработать со следующими дистрибутивами:
- Debian/Ubuntu
- ARM (например Raspberry PI)
- Arch Linux
- Void Linux
- FreeBSD
- MacOS

### С помощью бинарников

Перейдите на [страницу релизов](https://github.com/sharkdp/bat/releases) для
скомпилированных файлов `bat` для различных платформ. Бинарные файлы со статической связкой так-же доступны - выбирайте архив с `musl` в имени.

### Из исходников

Если вы желаете установить `bat` из исходников, вам понадобится Rust 1.40 или выше. Далее, вы должны использовать `cargo` чтобы все скомпилировать:

```bash
cargo install --locked bat
```

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
```bash
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
mkdir -p "$(bat --config-dir)/syntaxes"
cd "$(bat --config-dir)/syntaxes"

# Put new '.sublime-syntax' language definition files
# in this folder (or its subdirectories), for example:
git clone https://github.com/tellnobody1/sublime-purescript-syntax
```

Now use the following command to parse these files into a binary cache:

```bash
bat cache --build
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
mkdir -p "$(bat --config-dir)/themes"
cd "$(bat --config-dir)/themes"

# Download a theme in '.tmTheme' format, for example:
git clone https://github.com/greggb/sublime-snazzy

# Update the binary cache
bat cache --build
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

**Note**: By default, if the pager is set to `less` (and no command-line options are specified),
`bat` will pass the following command line options to the pager: `-R`/`--RAW-CONTROL-CHARS`,
`-F`/`--quit-if-one-screen` and `-X`/`--no-init`. The last option (`-X`) is only used for `less`
versions older than 530.

The `-R` option is needed to interpret ANSI colors correctly. The second option (`-F`) instructs
less to exit immediately if the output size is smaller than the vertical size of the terminal.
This is convenient for small files because you do not have to press `q` to quit the pager. The
third option (`-X`) is needed to fix a bug with the `--quit-if-one-screen` feature in old versions
of `less`. Unfortunately, it also breaks mouse-wheel support in `less`.

If you want to enable mouse-wheel scrolling on older versions of `less`, you can pass just `-R` (as
in the example above, this will disable the quit-if-one-screen feature). For less 530 or newer,
it should work out of the box.

### Dark mode

If you make use of the dark mode feature in macOS, you might want to configure `bat` to use a different
theme based on the OS theme. The following snippet uses the `default` theme when in the _dark mode_
and the `GitHub` theme when in the _light mode_.

```bash
alias cat="bat --theme=\$(defaults read -globalDomain AppleInterfaceStyle &> /dev/null && echo default || echo GitHub)"
```

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

A default configuration file can be created with the `--generate-config-file` option.
```bash
bat --generate-config-file
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

# Use C++ syntax for .ino files
--map-syntax "*.ino:C++"

# Use ".gitignore"-style highlighting for ".ignore" files
--map-syntax ".ignore:Git Ignore"
```

## Using `bat` on Windows

`bat` mostly works out-of-the-box on Windows, but a few features may need extra configuration.

### Paging

Windows only includes a very limited pager in the form of `more`. You can download a Windows binary
for `less` [from its homepage](http://www.greenwoodsoftware.com/less/download.html) or [through
Chocolatey](https://chocolatey.org/packages/Less). To use it, place the binary in a directory in
your `PATH` or [define an environment variable](#using-a-different-pager). The [Chocolatey package](#on-windows) installs `less` automatically.

### Colors

Windows 10 natively supports colors in both `conhost.exe` (Command Prompt) and PowerShell since
[v1511](https://en.wikipedia.org/wiki/Windows_10_version_history#Version_1511_(November_Update)), as
well as in newer versions of bash. On earlier versions of Windows, you can use
[Cmder](http://cmder.net/), which includes [ConEmu](https://conemu.github.io/).

**Note:** The Git and MSYS versions of `less` do not correctly interpret colors on Windows. If you
don’t have any other pagers installed, you can disable paging entirely by passing `--paging=never`
or by setting `BAT_PAGER` to an empty string.

### Cygwin

`bat` on Windows does not natively support Cygwin's unix-style paths (`/cygdrive/*`). When passed an absolute cygwin path as an argument, `bat` will encounter the following error: `The system cannot find the path specified. (os error 3)`

This can be solved by creating a wrapper or adding the following function to your `.bash_profile` file:

```bash
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

### File encodings

`bat` natively supports UTF-8 as well as UTF-16. For every other file encoding, you may need to
convert to UTF-8 first because the encodings can typically not be auto-detected. You can `iconv`
to do so.
Example: if you have a PHP file in Latin-1 (ISO-8859-1) encoding, you can call:
``` bash
iconv -f ISO-8859-1 -t UTF-8 my-file.php | bat
```
Note: you might have to use the `-l`/`--language` option if the syntax can not be auto-detected
by `bat`.

## Development

```bash
# Recursive clone to retrieve all submodules
git clone --recursive https://github.com/sharkdp/bat

# Build (debug version)
cd bat
cargo build --bins

# Run unit tests and integration tests
cargo test

# Install (release version)
cargo install --locked

# Build a bat binary with modified syntaxes and themes
bash assets/create.sh
cargo install --locked --force
```

## Maintainers

- [sharkdp](https://github.com/sharkdp)
- [eth-p](https://github.com/eth-p)

## Project goals and alternatives

`bat` tries to achieve the following goals:

- Provide beautiful, advanced syntax highlighting
- Integrate with Git to show file modifications
- Be a drop-in replacement for (POSIX) `cat`
- Offer a user-friendly command-line interface

There are a lot of alternatives, if you are looking for similar programs. See
[this document](doc/alternatives.md) for a comparison.

## License
Copyright (c) 2018-2020 [bat-developers](https://github.com/sharkdp/bat).

`bat` is distributed under the terms of both the MIT License and the Apache License 2.0.

See the [LICENSE-APACHE](LICENSE-APACHE) and [LICENSE-MIT](LICENSE-MIT) files for license details.

<p align="center">
  <img src="doc/logo-header.svg" alt="bat - a cat clone with wings"><br>
  <a href="https://travis-ci.org/sharkdp/bat"><img src="https://travis-ci.org/sharkdp/bat.svg?branch=master" alt="Build Status"></a>
  <a href="https://ci.appveyor.com/project/sharkdp/bat"><img src="https://ci.appveyor.com/api/projects/status/cptsmtbiwbnr2vhf/branch/master?svg=true"></a>
  <img src="https://img.shields.io/crates/l/bat.svg" alt="license">
  <a href="https://crates.io/crates/bat"><img src="https://img.shields.io/crates/v/bat.svg?colorB=319e8c" alt="Version info"></a><br>
  Клон утилиты <i>cat(1)</i> с поддержкой выделения синтаксиса и Git
</p>

<p align="center">
  <a href="#выделение-синтаксиса">Ключевые возможности</a> •
  <a href="#как-использовать">Использование</a> •
  <a href="#установка">Установка</a> •
  <a href="#кастомизация">Кастомизация</a> •
  <a href="#цели-и-альтернативы">Цели и альтернативы </a> •
  Перевод [<a href="https://github.com/chinanf-boy/bat-zh">中文</a>][<a href="doc/README-ja.md">日本語</a>][<a href="doc/README-ko.md">한국어</a>][<a href="doc/README-ru.md">Русский</a>]
</p>

### Выделение синтаксиса

`bat` поддерживает выделение синтаксиса для огромного количества языков программирования и разметки:

![Пример выделения синтаксиса](https://i.imgur.com/3FGy5tW.png)

### Интеграция с Git
`bat` использует `git`, чтобы показать изменения в коде
(смотрите на левый сайдбар):

![Пример интеграции с Git](https://i.imgur.com/azUAzdx.png)

### Показать непечатаемые символы

Вы можете использовать `-A` / `--show-all` флаг, чтобы показать символы, которые невозможно напечатать:

![Строка с неотображемыми символами](https://i.imgur.com/X0orYY9.png)

### Автоматическое разделение текста

`bat` умеет перенаправлять вывод в `less`, если вывод не помещается на экране полностью.

### Объединение файлов

О... Вы также можете объединять файлы :wink:. Когда
`bat` обнаружит неинтерактивный терминал (например, когда вы перенаправляете вывод в файл или процесс), он будет работать как утилита `cat` и выведет содержимое файлов как обычный текст (без подсветки синтаксиса).

## Как использовать

Вывести единственный файл в терминале

```bash
> bat README.md
```

Отобразить сразу несколько файлов в терминале

```bash
> bat src/*.rs
```

Читаем из stdin и определяем синтаксис автоматически (внимание: это делается по заглавной строке файла, например, `#!/bin/sh`)

```bash
> curl -s https://sh.rustup.rs | bat
```

Прочитать из stdin с явным указанием языка

```bash
> yaml2json .travis.yml | json_pp | bat -l json
```

Вывести и выделить неотображаемые символы
```bash
> bat -A /etc/hosts
```

Использование в качестве замены `cat`

```bash
bat > note.md  # мгновенно создаем новый файл

bat header.md content.md footer.md > document.md

bat -n main.rs  # показываем только количество строк

bat f - g  # выводит 'f' в stdin, а потом 'g'.
```

### Интеграция с другими утилитами

#### `find` или `fd`

Вы можете использовать флаг `-exec` в `find`, чтобы посмотреть превью всех файлов в `bat`
```bash
find … -exec bat {} +
```

Если вы используете [`fd`](https://github.com/sharkdp/fd), применяйте для этого флаг `-X`/`--exec-batch`:
```bash
fd … -X bat
```

#### `ripgrep`

С помощью [`batgrep`](https://github.com/eth-p/bat-extras/blob/master/doc/batgrep.md), `bat` может быть использован для вывода результата запроса [`ripgrep`](https://github.com/BurntSushi/ripgrep)

```bash
batgrep needle src/
```

#### `tail -f`

`bat` может быть использован вместе с  `tail -f`, чтобы выводить содержимое файла с подсветкой синтаксиса в реальном времени.
```bash
tail -f /var/log/pacman.log | bat --paging=never -l log
```
Заметьте, что мы должны отключить пэйджинг, чтобы это заработало. Мы также явно указали синтаксис (`-l log`), так как он не может быть автоматически определен в данном случае.

#### `git`

Вы можете использовать `bat` с `git show`, чтобы просмотреть старую версию файла с выделением синтаксиса:
```bash
git show v0.6.0:src/main.rs | bat -l rs
```

Обратите внимание, что выделение синтаксиса не работает в `git diff` на данный момент. Если вам это нужно, посмотрите [`delta`](https://github.com/dandavison/delta).

#### `xclip`

Нумерация стро и отображение изменений затрудняет копирование содержимого файлов в буфер обмена.
Чтобы спроваиться с этим, используйте флаг `-p`/`--plain` или просто перенаправьте стандартный вывод в `xclip`:
```bash
bat main.cpp | xclip
```
`bat` обнаружит перенаправление вывода и выведет обычный текст без выделения синтаксиса.

#### `man`

`bat` может быть использован в виде выделения цвета для  `man`, для этого установите переменную окружения
`MANPAGER`:

```bash
export MANPAGER="sh -c 'col -bx | bat -l man -p'"
man 2 select
```

Возможно вам понадобится также установить `MANROFFOPT="-c"`, если у вас есть проблемы с форматированием.

Если вы хотите сделать этой одной командой, вы можете использовать [`batman`](https://github.com/eth-p/bat-extras/blob/master/doc/batman.md).

Обратите внимание, что [синтаксис manpage](assets/syntaxes/02_Extra/Manpage.sublime-syntax) разрабатывается в этом репозитории и все еще находится в разработке.

#### `prettier` / `shfmt` / `rustfmt`

[`Prettybat`](https://github.com/eth-p/bat-extras/blob/master/doc/prettybat.md) — скрипт, который форматирует код и выводит его с помощью `bat`.


## Установка

[![Packaging status](https://repology.org/badge/vertical-allrepos/bat.svg)](https://repology.org/project/bat/versions)

### Ubuntu (с помощью `apt`)
*... и другие дистрибутивы основанные на Debian.*

`bat` есть в репозиториях [Ubuntu](https://packages.ubuntu.com/eoan/bat) и
[Debian](https://packages.debian.org/sid/bat) и доступен начиная с Ubuntu Eoan 19.10. На Debian `bat` пока что доступен только с нестабильной веткой "Sid".

Если ваша версия Ubuntu/Debian достаточно новая, вы можете установить `bat` так:

```bash
apt install bat
```

Если вы установили `bat` таким образом, то бинарный файл может быть установлен как `batcat` вместо `bat` (из-за [конфликта имени с другим пакетом](https://github.com/sharkdp/bat/issues/982)). Вы можете сделать симлинк или алиас `bat -> batcat`, чтобы предотвратить подобные проблемы и в других дистрибутивах.

``` bash
mkdir -p ~/.local/bin
ln -s /usr/bin/batcat ~/.local/bin/bat
```

### Ubuntu (С помощью самого нового `.deb` пакета)
*... и другие дистрибутивы Linux основанные на Debian*

Если пакет еще недоступен в вашем Ubuntu/Debian дистрибутиве или вы хотите установить самую последнюю версию `bat`, то вы можете скачать самый последний `deb`-пакет отсюда:
[release page](https://github.com/sharkdp/bat/releases) и установить так:

```bash
sudo dpkg -i bat_0.16.0_amd64.deb  # измените архитектуру и версию
```

### Alpine Linux

Вы можете установить [`bat`](https://pkgs.alpinelinux.org/packages?name=bat) из официальных источников:

```bash
apk add bat
```

### Arch Linux

Вы можете установить [`bat`](https://www.archlinux.org/packages/community/x86_64/bat/) из официального источника:

```bash
pacman -S bat
```

### Fedora

Вы можете установить [`bat`](https://koji.fedoraproject.org/koji/packageinfo?packageID=27506) из официального репозитория [Fedora Modular](https://docs.fedoraproject.org/en-US/modularity/using-modules/).

```bash
dnf install bat
```

### Gentoo Linux

Вы можете установить [`bat`](https://packages.gentoo.org/packages/sys-apps/bat) из официальных источников:

```bash
emerge sys-apps/bat
```

### Void Linux

Вы можете установить `bat` с помощью `xbps-install`:
```bash
xbps-install -S bat
```

### FreeBSD

Вы можете установить [`bat`](https://www.freshports.org/textproc/bat) с помощью `pkg`:

```bash
pkg install bat
```

или самому скомпилировать его:

```bash
cd /usr/ports/textproc/bat
make install
```

### С помощью nix

Вы можете установить `bat`, используя [nix package manager](https://nixos.org/nix):

```bash
nix-env -i bat
```

### openSUSE

Вы можете установить `bat` с помощью `zypper`:

```bash
zypper install bat
```

### macOS

Вы можете установить `bat` с помощью [Homebrew](http://braumeister.org/formula/bat):

```bash
brew install bat
```

Или же установить его с помощью [MacPorts](https://ports.macports.org/port/bat/summary):

```bash
port install bat
```

### Windows

Есть несколько способов установить `bat`. Как только вы установили его, посмотрите на секцию ["Использование `bat` в Windows"](#using-bat-on-windows).

#### С помощью Chocolatey

Вы можете установить `bat` с помощью [Chocolatey](https://chocolatey.org/packages/Bat):
```bash
choco install bat
```

#### С помощью Scoop

Вы можете установить `bat` с помощью [scoop](https://scoop.sh/):
```bash
scoop install bat
```

Для этого у вас должен быть установлен [Visual C++ Redistributable](https://support.microsoft.com/en-us/help/2977003/the-latest-supported-visual-c-downloads).

#### Из заранее скомпилированных файлов:

Их вы можете скачать на [странице релизов](https://github.com/sharkdp/bat/releases).

Для этого у вас должен быть установлен [Visual C++ Redistributable](https://support.microsoft.com/en-us/help/2977003/the-latest-supported-visual-c-downloads).

### С помощью Docker

Вы можете использовать [Docker image](https://hub.docker.com/r/danlynn/bat/), чтобы запустить `bat` в контейнере:
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
- macOS

### Из скомпилированных файлов

Перейдите на [страницу релизов](https://github.com/sharkdp/bat/releases) для
скомпилированных файлов `bat` для различных платформ. Бинарные файлы со статической связкой так же доступны: выбирайте архив с `musl` в имени.

### Из исходников

Если вы желаете установить `bat` из исходников, вам понадобится Rust 1.40 или выше. После этого используйте `cargo`, чтобы все скомпилировать:

```bash
cargo install --locked bat
```

## Кастомизация

### Темы для выделения текста

Используйте `bat --list-themes`, чтобы вывести список всех доступных тем. Для выбора темы `TwoDark` используйте `bat` с флагом
`--theme=TwoDark` или выставьте переменную окружения `BAT_THEME` в `TwoDark`. Используйте `export BAT_THEME="TwoDark"` в конфигурационном файле вашей оболочки, чтобы изменить ее навсегда. Или же используйте [конфигурационный файл](https://github.com/sharkdp/bat#configuration-file) `bat`.

Если вы хотите просто просмотреть темы, используйте следующую команду (для этого вам понадобится [`fzf`](https://github.com/junegunn/fzf)):
```bash
bat --list-themes | fzf --preview="bat --theme={} --color=always /путь/к/файлу"
```

`bat` отлично смотрится на темном фоне. Однако если ваш терминал использует светлую тему, то такие темы как `GitHub` или `OneHalfLight` будут смотреться куда лучше!
Вы также можете использовать новую тему, для этого перейдите [в раздел добавления тем](https://github.com/sharkdp/bat#добавление-новых-тем).

### Изменение внешнего вывода

Вы можете использовать флаг `--style`, чтобы изменять внешний вид вывода в `bat`.
Например, вы можете использовать `--style=numbers,changes`, чтобы показать только количество строк и изменений в Git. Установите переменную окружения `BAT_STYLE` чтобы изменить это навсегда, или используйте [конфиг файл](https://github.com/sharkdp/bat#configuration-file) `bat`.

### Добавление новых синтаксисов

`bat` использует [`syntect`](https://github.com/trishume/syntect/) для выделения синтаксиса. `syntect` может читать
[файл `.sublime-syntax`](https://www.sublimetext.com/docs/3/syntax.html)
и темы. Чтобы добавить новый синтаксис, сделайте следующее:

Создайте каталог с синтаксисом:

```bash
mkdir -p "$(bat --config-dir)/syntaxes"
cd "$(bat --config-dir)/syntaxes"

# Разместите файлы '.sublime-syntax'
# в каталоге (или субкаталогах), например:
git clone https://github.com/tellnobody1/sublime-purescript-syntax
```

Теперь используйте следующую команду, чтобы превратить эти файлы в бинарный кеш:

```bash
bat cache --build
```

Теперь вы можете использовать `bat --list-languages`, чтобы проверить, доступны ли новые языки.

Если когда-нибудь вы заходите вернуться к настройкам по умолчанию, введите

```bash
bat cache --clear
```

### Добавление новых тем

Это работает похожим образом, так же как и добавление новых тем выделения синтаксиса

Во-первых, создайте каталог с новыми темами для синтаксиса:
```bash
mkdir -p "$(bat --config-dir)/themes"
cd "$(bat --config-dir)/themes"

# Загрузите тему в формате '.tmTheme':
git clone https://github.com/greggb/sublime-snazzy

# Обновите кеш
bat cache --build
```

Теперь используйте `bat --list-themes`, чтобы проверить доступность новых тем.

### Использование другого пейджера.

`bat` использует пейджер, указанный в переменной окружения `PAGER`. Если она не задана, то используется `less`.
Если вы желаете использовать другой пейджер, вы можете либо изменить переменную `PAGER`, либо `BAT_PAGER` чтобы перезаписать то, что указано в `PAGER`.

Чтобы передать дополнительные аргументы вашему пейджеру, перечислите их в этой переменной:

```bash
export BAT_PAGER="less -RF"
```

Так же вы можете использовать [файл конфигурации](https://github.com/sharkdp/bat#configuration-file) `bat` (флаг `--pager`).

**Внимание**: По умолчанию пейджером является`less` (без каких-либо аргументов),
`bat` задаст следующие флаги для пейджера:
 `-R`/`--RAW-CONTROL-CHARS`,
`-F`/`--quit-if-one-screen` и `-X`/`--no-init`. Последний флаг(`-X`) используется только для `less`, чья версия раньше 530.

Флаг `-R` нужен чтобы корректно воспроизвести ANSI цвета. Второй флаг (`-F`) говорит 
`less` чтобы тот сразу же завершился, если размер вывода меньше чем вертикальный размер терминала.
Это удобно для небольших файлов, так как вам не надо каждый раз нажимать `q`, чтобы выйти из пейджера. Третий флаг (`-X`) нужен для того, чтобы исправить баг с `--quit-if-one-screen` в старых версиях `less`. К сожалению, это блокирует возможность использования колеса мышки.

Если вы хотите все же его включить, вы можете добавить флаг `-R`. 
Для `less` новее чем 530 оно должно работать из коробки.

### Темная тема

Если вы используете темный режим в macOS, возможно вы захотите чтобы `bat` использовал другую тему, основанную на теме вашей ОС. Следующий сниппет использует тему `default`, когда у вас включен темный режим, и тему `GitHub`, когда включен светлый.

```bash
alias cat="bat --theme=\$(defaults read -globalDomain AppleInterfaceStyle &> /dev/null && echo default || echo GitHub)"
```

## Файл конфигурации

`bat` также может быть кастомизирован с помощью файла конфигурации. Его местоположение зависит от вашей ОС: чтобы посмотреть его путь, введите
```
bat --config-file
```

Также вы можете установить переменную окружения `BAT_CONFIG_PATH`, чтобы изменить путь к файлу конфигурации.
```bash
export BAT_CONFIG_PATH="/path/to/bat.conf"
```

Файл конфигурации «по умолчанию» может быть создан с помощью флага `--generate-config-file`.
```bash
bat --generate-config-file
```

### Формат

Файл конфигурации - это всего лишь набор аргументов. Введите `bat --help`, чтобы просмотреть список всех возможных флагов и аргументов. Также вы можете закомментировать строку с помощью `#`.

Пример файла конфигурации:
```bash
# Установить тему "TwoDark"
--theme="TwoDark"

# Показывать количество строк, изменений в Git и заголовок файла
--style="numbers,changes,header"

# Использовать курсив (поддерживается не всеми терминалами)
--italic-text=always

# Использовать синтаксис C++ для всех .ino файлов
--map-syntax "*.ino:C++"

# Использовать синтаксик Git Ignore для всех файлов .ignore
--map-syntax ".ignore:Git Ignore"
```

## Использование `bat` в Windows

`bat` полностью работоспособен "из коробки", но для некоторых возможностей могут понадобиться дополнительные настройки.

### Пейджинг

Windows поддерживает только очень простой пейджер `more`. Вы можете скачать установщик для `less` [с его сайта](http://www.greenwoodsoftware.com/less/download.html) или [через Chocolatey](https://chocolatey.org/packages/Less). Чтобы его использовать, скопируйте исполняемый файл в ваш `PATH` или [используйте переменную окружения](#Использование-другого-пейджера). [Пакет из Chocolatey](#windows) установит все автоматически.

### Цвета

Windows 10 поддерживает цвета и в `conhost.exe` (Command Prompt), и в PowerShell начиная с версии Windows
[v1511](https://ru.wikipedia.org/wiki/Windows_10#Обновления и поддержка), так же как и в bash. На ранних версиях Windows вы можете использовать 
[Cmder](http://cmder.net/), в котором есть [ConEmu](https://conemu.github.io/).

**Внимание:** Версия `less` в Git и MSYS2 воспроизводит цвета некорректно. Если у вас нет других пейджеров, мы можете отключить использование пейджеров с помощью флага `--paging=never`
или установить `BAT_PAGER` равным пустой строке.

### Cygwin

Из коробки `bat` не поддерживает пути в стиле Unix (`/cygdrive/*`). Когда указан абсолютный путь cygwin, `bat` выдаст следующую ошибку: `The system cannot find the path specified. (os error 3)`

Она может быть решена добавлением следующей функции в `.bash_profile`:

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

## Проблемы и их решение

### Терминалы и цвета

`bat` поддерживает терминалы *с* и *без* поддержки truecolor. Однако подсветка синтаксиса не оптимизирована для терминалов с 8-битными цветами, и рекомендуется использовать терминалы с поддержкой 24-битных цветов (`terminator`, `konsole`, `iTerm2`, ...).
Смотрите [эту статью](https://gist.github.com/XVilka/8346728) для полного списка терминалов.

Удостовертесь, что переменная `COLORTERM` равна `truecolor` или
`24bit`. Иначе `bat` не сможет определить поддержку 24-битных цветов (и будет использовать 8-битные).

### Текст и номера строк плохо видны

Используйте другую тему (`bat --list-themes` выведет список всех установленных тем). Темы `OneHalfDark` и
`OneHalfLight` имеют более яркие номера строк и тексты.

### Кодировки файлов

`bat` поддерживает UTF-8 и UTF-16. Файлы в других кодировках, возможно, придётся перекодировать, так как кодировка может быть распознана неверно. Используйте `iconv`.
Пример: у вас есть PHP файл в кодировке Latin-1 (ISO-8859-1):
``` bash
iconv -f ISO-8859-1 -t UTF-8 my-file.php | bat
```
Внимание: вам может понадобится флаг `-l`/`--language`, если `bat` не сможет автоматически определить синтаксис.

## Разработка

```bash
# Рекурсивно клонирует все модули
git clone --recursive https://github.com/sharkdp/bat

# Компиляции в режиме разработки
cd bat
cargo build --bins

# Запуск тестов
cargo test

# Установка (релизная версия)
cargo install --locked

# Компилирование исполняего файла bat с другим синтаксисом и темами
bash assets/create.sh
cargo install --locked --force
```

## Разработчики

- [sharkdp](https://github.com/sharkdp)
- [eth-p](https://github.com/eth-p)

## Цели и альтернативы

Цели проекта `bat`:

- Красивая и продвинутая подсветка синтаксиса.
- Интеграция с Git.
- Полноценная замена `cat`.
- Дружелюбный интерфейс и аргументы.

Есть очень много альтернатив `bat`. Смотрите [этот документ](doc/alternatives.md) для сравнения.

## Лицензия
Copyright (c) 2018-2020 [Разработчики bat](https://github.com/sharkdp/bat).

`bat` распостраняется под лицензями MIT License и Apache License 2.0 (на выбор пользователя).

Смотрите [LICENSE-APACHE](LICENSE-APACHE) и [LICENSE-MIT](LICENSE-MIT) для более подробного ознакомления.

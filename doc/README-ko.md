<p align="center">
  <img src="../doc/logo-header.svg" alt="bat - a cat clone with wings"><br>
  <a href="https://travis-ci.org/sharkdp/bat"><img src="https://travis-ci.org/sharkdp/bat.svg?branch=master" alt="Build Status"></a>
  <a href="https://ci.appveyor.com/project/sharkdp/bat"><img src="https://ci.appveyor.com/api/projects/status/cptsmtbiwbnr2vhf/branch/master?svg=true"></a>
  <img src="https://img.shields.io/crates/l/bat.svg" alt="license">
  <a href="https://crates.io/crates/bat"><img src="https://img.shields.io/crates/v/bat.svg?colorB=319e8c" alt="Version info"></a><br>

  문법 강조와 깃 통합 기능의 <i>cat(1)</i> 클론
</p>

<p align="center">
  <a href="#문법-강조">주요 기능들</a> •
  <a href="#사용법">사용법</a> •
  <a href="#설치">설치</a> •
  <a href="#커스터마이즈">커스터마이즈</a> •
  <a href="#프로젝트-목표와-대안들">프로젝트 목표와 대안들</a> •
  번역 [<a href="https://github.com/chinanf-boy/bat-zh">中文</a>][<a href="../doc/README-ja.md">日本語</a>]
</p>

### 문법 강조

`bat`은 다양한 프로그래밍 언어와 마크업 언어에 대해 문법 강조(Syntax highlighting)기능을 지원하고 있습니다: 

![Syntax highlighting example](https://imgur.com/rGsdnDe.png)

### Git 통합

`bat`은 `git`을 통해 인덱스와 함께 변경분을 표시합니다 (왼쪽 바를 확인하세요):

![Git integration example](https://i.imgur.com/2lSW4RE.png)

### 표시할 수 없는 문자 처리

`-A`/`--show-all` 옵션을 사용하여 표시할수 없는 문자를 시각화 해줍니다: 

![Non-printable character example](https://i.imgur.com/WndGp9H.png)

### 자동 페이징 

하나의 화면에 비해 출력이 너무 큰 경우, `less` 를 이용해 출력들을 연결할 수 있습니다.

### 파일 연결

이 뿐만 아니라 파일을 연결할 때도 사용 할 수 있습니다. :wink: `bat`가 인터렉티브 하지 않은(non-interactive)가 감지하면 (예를 들어,다른 프로세스 혹은 파일과 파이프라인을 연결 한 경우) `bat`은 `cat`을 대신하여 동작하며 일반 파일 내용을 표기해줍니다. 

## 사용법

터미널에서 하나의 파일 표시하기

```bash
> bat README.md
```

여러 파일 한번에 보여주기 

```bash
> bat src/*.rs
```

stdin에서 읽고, 자동으로 맞는 문법 강조 적용하기 

```bash
> curl -s https://sh.rustup.rs | bat
```

stdin에서 읽고, 명시적으로 언어 지정하여 적용하기 

```bash
> yaml2json .travis.yml | json_pp | bat -l json
```

표시할 수 없는 문자 처리하기 
```bash
> bat -A /etc/hosts
```

`cat` 대신 사용하기:

```bash
bat > note.md  # quickly create a new file

bat header.md content.md footer.md > document.md

bat -n main.rs  # show line numbers (only)

bat f - g  # output 'f', then stdin, then 'g'.
```

### 다른 툴과의 통합

#### `find` 와 `fd`

`find`의 `-exec` 옵션을 사용하여 `bat`의 모든 검색 결과를 미리 볼 수 있습니다: 
```bash
find … -exec bat {} +
```

[`fd`](https://github.com/sharkdp/fd)를 사용하고 있는 경우, `-X`/`--exec-batch` 옵션을 이용하여 동일하게 사용할 수 있습니다:
```bash
fd … -X bat
```

#### `ripgrep`

[`batgrep`](https://github.com/eth-p/bat-extras/blob/master/doc/batgrep.md)과 함께, [`ripgrep`](https://github.com/BurntSushi/ripgrep)의 검색 결과를 `bat`을 이용하여 볼 수 있습니다. 

```bash
batgrep needle src/
```

#### `tail -f`

`bat` 과 `tail -f`를 함께 사용하여, 특정 파일을 문법 강조하며 지속적으로 모니터링 할 수 있습니다. 
```bash
tail -f /var/log/pacman.log | bat --paging=never -l log
```
이 작업을 하려면 페이징 기능을 꺼야합니다. 또, 자동 감지가 되지 않기 때문에, 적용되어야할 문법을 명시적(`-l log`)으로 지정해야 합니다.

#### `git`

`bat`과 `git show`를 함께 사용하여 주어진 파일의 이전 기록을 문법 강조와 함께 볼 수 있습니다:
```bash
git show v0.6.0:src/main.rs | bat -l rs
```

diffs 내에서 문법 강조 표시는 현재 지원되지 않습니다. 이 기능은 [`delta`](https://github.com/dandavison/delta)에서 찾아 볼 수 있습니다. 

#### `xclip`

`bat` 출력에서 라인 넘버와 Git 수정 내역이 같이 있어 파일 내용을 복사하기가 어려울 수도 있습니다. 이 경우에는 `-p`/`--plain` 옵션을 사용 하거나 출력 시 파이프라인으로 `xclip`을 사용하면 됩니다: 
```bash
bat main.cpp | xclip
```
`bat` 에서는 리다이렉트된 것으로 감지하여, 파일 내용만 출력합니다. 

#### `man`

`bat`은 `MANPAGER` 환경 변수 설정을 통해 `man`에 대하여 컬러 페이져를 사용할 수 있습니다: 

```bash
export MANPAGER="sh -c 'col -bx | bat -l man -p'"
man 2 select
```

포매팅 문제가 발생한다면, `MANROFFOPT="-c"` 설정 해야 할 수도 있습니다 .

새 커맨드에서 이 번들을 사용하려면, [`batman`](https://github.com/eth-p/bat-extras/blob/master/doc/batman.md)을 이용 할 수 있습니다. 


참고 : [Manpage syntax](../assets/syntaxes/Manpage.sublime-syntax)는 이 저장소에서 개발되고 있으며, 아직 작업 중 입니다. 

#### `prettier` / `shfmt` / `rustfmt`

[`prettybat`](https://github.com/eth-p/bat-extras/blob/master/doc/prettybat.md) 스크립트는 코드를 포맷팅하고 `bat`으로 출력해주는 랩퍼(wrapper) 입니다. 


## 설치

[![Packaging status](https://repology.org/badge/vertical-allrepos/bat.svg)](https://repology.org/project/bat/versions)

### On Ubuntu
*... 그리고 기타 Debian 기반의 Linux 배포판들.*

Ubuntu Eoan 19.10 혹은 Debian unstable sid 이후 버전 부터는 [Ubuntu용 `bat` 패키지](https://packages.ubuntu.com/eoan/bat)나 [Debian용 `bat` 패키지](https://packages.debian.org/sid/bat) 를 설치 할 수 있습니다. 

```bash
apt install bat
```

만약 최근 릴리즈된 bat을 사용을 원하거나 buntu/Debian 예전 버전을 사용하는 경우, [릴리즈 페이지](https://github.com/sharkdp/bat/releases)에서 다음과 같이 `.deb` 패키지를 받아 설치 할 수도 있습니다:

```bash
sudo dpkg -i bat_0.16.0_amd64.deb  # adapt version number and architecture
```

### On Alpine Linux

공식 소스를 통해 [`bat` 패키지](https://pkgs.alpinelinux.org/packages?name=bat) 를 설치 할 수 있습니다:

```bash
apk add bat
```

### On Arch Linux

공식 소스를 통해 [`bat` 패키지](https://www.archlinux.org/packages/community/x86_64/bat/)를 설치할 수 있습니다:

```bash
pacman -S bat
```

### On Fedora

[공식 Fedora 모듈 저장소](https://docs.fedoraproject.org/en-US/modularity/using-modules/)에서 [`bat` 패키지](https://koji.fedoraproject.org/koji/packageinfo?packageID=27506)을 설치할 수 있습니다:

```bash
dnf install bat
```

### On Gentoo Linux

공식 소스를 통해 [`bat` 패키지](https://packages.gentoo.org/packages/sys-apps/bat)를 설치할 수 있습니다. 

```bash
emerge sys-apps/bat
```

### On Void Linux

xbps-install를 이용해 `bat`을 설치할 수 있습니다: 
```bash
xbps-install -S bat
```

### On FreeBSD

pkg를 이용하여 미리 컴파일된 [`bat` 패키지](https://www.freshports.org/textproc/bat)를 설치할 수 있습니다:

```bash
pkg install bat
```

또는 FreeBSD 포트에서 직접 빌드할 수도 있습니다: 

```bash
cd /usr/ports/textproc/bat
make install
```

### Via nix

[nix package manager](https://nixos.org/nix)를 이용해 `bat`을 설치할 수 있습니다:

```bash
nix-env -i bat
```

### On openSUSE

zypper을 이용해 `bat`을 설치할 수 있습니다:

```bash
zypper install bat
```

### On macOS

[Homebrew](http://braumeister.org/formula/bat)를 이용해 `bat`을 설치할 수 있습니다: 

```bash
brew install bat
```

또는 [MacPorts](https://ports.macports.org/port/bat/summary)를 사용할 수도 있습니다: 

```bash
port install bat
```

### On Windows

Windows에서 `bat`을 설치할 수 있는 몇 가지 옵션들이 있습니다. 먼저 `bat`을 설치 한 후, ["Windows에서 사용하기"](#Windows에서-사용하기) 참고하시기 바랍니다.  

#### With Chocolatey

[Chocolatey](https://chocolatey.org/packages/Bat)를 이용해 `bat`을 설치할 수 있습니다: 
```bash
choco install bat
```

#### With Scoop

[scoop](https://scoop.sh/)을 이용해 `bat`을 설치할 수 있습니다:
```bash
scoop install bat
```
[Visual C++ Redistributable 패키지](https://support.microsoft.com/en-us/help/2977003/the-latest-supported-visual-c-downloads)를 같이 설치해 주어야 합니다. 

#### From prebuilt binaries:

[릴리즈 페이지](https://github.com/sharkdp/bat/releases)에서 빌드된 바이너리를 다운받을 수 있습니다. 

[Visual C++ Redistributable 패키지](https://support.microsoft.com/en-us/help/2977003/the-latest-supported-visual-c-downloads)를 같이 설치해 주어야 합니다. 

### Via Docker

컨테이너에서 `bat`을 사용할 수 있는 [Docker image](https://hub.docker.com/r/danlynn/bat/)가 있습니다.: 
```bash
docker pull danlynn/bat
alias bat='docker run -it --rm -e BAT_THEME -e BAT_STYLE -e BAT_TABS -v "$(pwd):/myapp" danlynn/bat'
```

### Via Ansible

[Ansible](https://www.ansible.com/)을 사용해 `bat`을 설치할 수 있습니다:

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

다음 배포판들에서 동작합니다: 
- Debian/Ubuntu
- ARM (eg. Raspberry PI)
- Arch Linux
- Void Linux
- FreeBSD
- MacOS

### From binaries

[릴리즈 페이지](https://github.com/sharkdp/bat/releases)에서 다양한 환경을 위해 빌드된 버전들을 확인 할 수 있습니다. 정적 링크 바이너리들은 아카이브에서 파일 이름에 `musl` 이 포함 파일로 확인 할 수 있습니다. 

### From source

`bat`의 소스를 직접 빌드하기 위해서는, Rust 1.36 이상이 필요하며 `cargo`를 이용해 빌드할 수 있습니다. 

```bash
cargo install --locked bat
```

일부 플랫폼에서는 `llvm` 그리고/또는 `libclang-dev` 설치가 필요할 수도 있습니다. 

## 커스터마이즈

### 문법 강조 테마 

`bat --list-themes`을 사용하면, 현재 사용 가능한 문법 강조 테마들을 확인할 수 있습니다. `TwoDark` 테마 선택하는 경우,  `--theme=TwoDark` 옵션과 함께 `bat`을 사용하거나 환경변수에서 `BAT_THEME`를 `TwoDark`로 세팅해주면 됩니다. 쉘 시작 파일에 `export BAT_THEME="TwoDark"` 를 정의해 계속 사용도 가능합니다. 이 밖에 `bat`의 [설정 파일](#설정-파일)을 이용할 수도 있습니다. 

다른 테마를 미리 보고 싶은경우 다음 명령어와 같이 사용할 수 있습니다.(이 경우 [`fzf`](https://github.com/junegunn/fzf)가 필요합니다.)

```bash
bat --list-themes | fzf --preview="bat --theme={} --color=always /path/to/file"
```
`bat`의 기본 테마는 어두운 배경색 터미널에 적합합니다. 만일 밝은 배경색을 사용할 경우에는  `GitHub` 이나 `OneHalfLight` 과 같은 테마가 더 잘 어울립니다. 아래 [새로운 테마 추가하기](#새로운-테마-추가하기)에 따라 커스텀 테마를 사용할수도 있습니다.

### 출력 스타일

`--style` 옵션을 이용하여 `bat`의 출력 스타일을 변경 할 수 있습니다. 예를 들어,  `--style=numbers,changes`는 Git 변경분과 라인 넘버에 대해서만 출력하며 눈금과 파일 헤더가 표시되지 않습니다. `BAT_STYLE` 환경 변수로 정의하여 계속해서 사용하거나`bat`의 [설정 파일](#설정-파일)을 사용할 수도 있습니다.

### 새로운 문법 강조 / 언어 추가하기

`bat`은 문법 강조를 위해 [`syntect`](https://github.com/trishume/syntect/) 라이브러리를 사용하고 있습니다.  `syntect`는 [Sublime Text의 `.sublime-syntax` 파일](https://www.sublimetext.com/docs/3/syntax.html)과 테마를 읽을 수 있습니다. 새로운 문법 강조를 추가하는 방법은 다음과 같습니다. 

우선 문법 정의 파일을 넣을 폴더를 만듭니다:

```bash
mkdir -p "$(bat --config-dir)/syntaxes"
cd "$(bat --config-dir)/syntaxes"

# Put new '.sublime-syntax' language definition files
# in this folder (or its subdirectories), for example:
git clone https://github.com/tellnobody1/sublime-purescript-syntax
```

다음 명령어를 통해 파일을 바이너리 캐시로 파싱합니다. 

```bash
bat cache --build
```

마지막으로 `bat --list-languages`을 통해 새로운 언어가 사용 가능한지 확인합니다. 

기본 설정으로 돌아가려면, 다음 명령어를 이용합니다.:

```bash
bat cache --clear
```

### 새로운 테마 추가하기

새로운 문법 정의 추가와 매우 유사합니다. 

먼저, 새로운 문법 강조 테마 폴더를 만듭니다. 
```bash
mkdir -p "$(bat --config-dir)/themes"
cd "$(bat --config-dir)/themes"

# Download a theme in '.tmTheme' format, for example:
git clone https://github.com/greggb/sublime-snazzy

# Update the binary cache
bat cache --build
```

마지막으로 `bat --list-themes`을 통해 새로운 테마가 사용 가능한지 확인합니다.  

### 다른 페이져 사용하기

`bat`은 환경변수 `PAGER`로 사용할 페이져를 명시합니다. 만약 이 변수가 정의되어있지 않다면 `less`가 기본입니다. 만약 다른 페이져를 사용하고 싶다면 `PAGER` 변수를 수정하거나 `BAT_PAGER` 환경 변수로 `PAGER` 대신 정의 할 수도 있습니다. 

만약 커맨드라인 인수로 넘기려면, `PAGER`/`BAT_PAGER` 변수를 정의를 통해 사용 할 수 있습니다. :

```bash
export BAT_PAGER="less -RF"
```

환경 변수 대신, `bat`의 [설정 파일](#설정-파일)에서 페이져를 설정 할 수도 있습니다.(`--pager` 옵션)

**참고**: 기본적으로 페이져가 `less`로 설정 되어있다면, `bat`은 다음 옵션들을 받을 수 있습니다: `-R`/`--RAW-CONTROL-CHARS`,
`-F`/`--quit-if-one-screen` 그리고 `-X`/`--no-init`. 마지막 옵션(`-X`)은 530 이전 버전에서만 사용됩니다. 

`-R` 옵션은 ANSI 컬러를 올바르게 해석하기 위해 필요합니다. 두번째 옵션 (`-F`)은 출력 크기가 터미널의 세로 크기보다 작을 경우 즉시 종료되도록 합니다. 
페이져를 종료하기 위해 `q`를 누를 필요 없기 때문에 작은 파일을 다룰 때 용이합니다. 세번째 옵션(`-X`)는 `less` 이전 버전의 `--quit-if-one-screen` 기능과 함께 버그를 잡을 때 필요합니다. 안타깝게도, `less`의 마우스 휠 지원은 종료합니다. 

`less` 예전 버전에서 마우스 휠 기능을 활성화 시키려면, `-R` 옵션을 사용하면 됩니다. (위의 예제처럼, 이 옵션은 quit-if-one-screen  기능을 비활성화 시킵니다.)
530 이하 버전에서는 그대로 사용할 수 있습니다. 

### 다크 모드

macOS에서 다크 모드를 사용하고 있다면, OS 테마에 따라 다른 테마를 사용하도록 `bat`의 구성할 수 있습니다. 아래 코드는 라이트 모드에서는 `default` 테마를 다크모드에서는 `Github` 테마를 사용하는 방법입니다. 

```bash
alias cat="bat --theme=\$(defaults read -globalDomain AppleInterfaceStyle &> /dev/null && echo default || echo GitHub)"
```

## 설정 파일

`bat` 설정 파일로 커스터마이즈 할 수 있습니다. 파일의 위치는 운영 체제에 따라 다릅니다. 아래 커맨드를 통해 시스템의 기본 경로를 알 수 있습니다. 
```
bat --config-file
```

또는, `BAT_CONFIG_PATH` 환경 변수를 사용하여 `bat`의 설정 파일 위치를 지정할 수 있습니다. 
```bash
export BAT_CONFIG_PATH="/path/to/bat.conf"
```

### 포맷

설정 파일은 명령어 인수들의 리스트 입니다. `bat --help`를 이용하여 가능한 옵션들과 값들을 확인해 볼 수 있습니다. 또, `#` 으로 주석을 추가할수도 있습니다. 

설정 파일의 예:
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

##  Windows에서 사용하기

`bat` 대부분의 경우 Windows에서 기본적으로 잘 작동하지만, 일부 기능에는 추가 적인 구성이 필요할 수 있습니다.

### 페이징 

Windows는 `more` 형식의 매우 제한된 페이저만이 포함되어있습니다. `less`용 Windows 바이너리는 [이 홈페이지](http://www.greenwoodsoftware.com/less/download.html)나  [Chocolatey](https://chocolatey.org/packages/Less)에서 다운로드 받을 수 있습니다. 이를 사용하려면 바이너리를 `PATH` 디렉토리에 배치하거나 [환경 변수로 정의](#using-a-different-pager) 하세요. [Chocolatey 패키지](#on-windows)로 `less`를 자동으로 설치할 수 있습니다. 

### 색상

Windows 10은 기본 내장기능으로 [v1511](https://en.wikipedia.org/wiki/Windows_10_version_history#Version_1511_(November_Update)) 이후의  `conhost.exe`(Command Prompt) 와 PowerShell, 그리고 최신 버전의 bash에서 색상을 지원합니다. 이전 버전의 Windows에서는, [ConEmu](https://conemu.github.io/)가 포함 된 [Cmder](http://cmder.net/)를 사용할 수 있습니다 .

**참고:**  `less`의 Git과 MSYS 버전은 Windows에서 색상을 올바르게 해석하지 않습니다. 다른 페이져가 설치되어 있지 않은 경우, `--paging=never` 하거나 `BAT_PAGER`를 빈 문자열로 설정하여 페이징을 완전히 비활성화 할 수 있습니다 .

### Cygwin

Windows에서의 `bat`은 기본적으로 Cygwin의 unix 스타일의 경로(`/cygdrive/*`)를 지원하지 않습니다. cygwin 절대경로를 인자로 받았을 때, `bat`은 다음과 같이 오류를 반환합니다. `:The system cannot find the path specified. (os error 3)`

이 경우,  wrapper를 만들거나 다음 함수를 `.bash_profile`추가하여 문제를 해결하실 수 있습니다 :

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

## 트러블슈팅

### 터미널과 색상

`bat`은 터미널 트루컬러 지원 여부와 상관없이 동작합니다. 하지만, 문법 강조 테마의 색상이 8-bit 컬러에는 최적화 되어 있지 않고 있으며, 24-bit 트루컬러 지원하는 터미널 사용하는 것을 적극 권장합니다.(`terminator`, `konsole`, `iTerm2`, ...). [이 글](https://gist.github.com/XVilka/8346728)에서 24-bit 트루컬러 지원하는 터미널들을 찾아보실 수 있습니다. 

사용하고 있는 터미널에서 `COLORTERM`을 `truecolor` 혹은
`24bit`으로 설정 되어있는지 확인하세요. 만약 아니라면, `bat`은 24-bit escape sequence를 지원되는지 여부를 판단 할 수 없습니다. (그리고 8-bit 색상으로 돌아갑니다.) 

### 라인 숫자와 눈금이 잘 보이지 않는 경우

다른 테마를 사용해 보세요. (`bat --list-themes`에서 테마들을 확인해 볼 수 있습니다.) `OneHalfDark` 과 `OneHalfLight` 테마는 눈금과 선의 색을 밝게 합니다. 

### 파일 인코딩

`bat`은 기본적으로 UTF-8과 UTF-8을 제공합니다. 다른 파일 인코딩의 경우, 자동 감지 되지 않으므로 UTF-8로 먼저 변환해 주어야 합니다. 이렇게 할 때, `iconv`를 사용 할 수 있습니다. 예를 들어,  Latin-1 (ISO-8859-1)로 인코딩된 PHP파일이라면 다음과 같이 사용할 수 있습니다.: 
``` bash
iconv -f ISO-8859-1 -t UTF-8 my-file.php | bat
```
참고: `bat`으로 자동 감지가 되지 않는 경우에는 `-l`/`--language` 옵션을 사용할 수도 있습니다. 

## 배포

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

## 메인테이너들

- [sharkdp](https://github.com/sharkdp)
- [eth-p](https://github.com/eth-p)

## 프로젝트 목표와 대안들

`bat`은 아래와 같은 목표를 달성하려고 합니다: 

- 아름답고 발전된 문법 강조 기능 
- Git 연동을 통한 파일 수정 내역 확인 
- (POSIX)`cat`의 대체제 
- 사용자 친화적인 CLI 제공


비슷한 프로그램들을 찾고 있다면, 많은 대안들이 있습니다. 비교는 [이 문서]((doc/alternatives.md))를 참조해주세요. 

## 라이센스
Copyright (c) 2018-2020 [bat-developers](https://github.com/sharkdp/bat).

`bat`는 MIT 라이센스 및 Apache 라이센스 2.0의 조건에 따라 배포됩니다.  

라이센스 세부사항은 [LICENSE-APACHE](LICENSE-APACHE)와 [LICENSE-MIT](LICENSE-MIT)를 참조하세요. 

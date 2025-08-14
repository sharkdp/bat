<p align="center">
  <img src="logo-header.svg" alt="bat - a cat clone with wings"><br>
  <a href="https://github.com/sharkdp/bat/actions?query=workflow%3ACICD"><img src="https://github.com/sharkdp/bat/workflows/CICD/badge.svg" alt="Build Status"></a>
  <img src="https://img.shields.io/crates/l/bat.svg" alt="license">
  <a href="https://crates.io/crates/bat"><img src="https://img.shields.io/crates/v/bat.svg?colorB=319e8c" alt="Version info"></a><br>
  문법 강조와 Git 통합 기능의 <i>cat(1)</i> 클론
</p>

<p align="center">
  <a href="#문법-강조">주요 기능들</a> •
  <a href="#사용법">사용법</a> •
  <a href="#설치">설치</a> •
  <a href="#사용자화">사용자화</a> •
  <a href="#프로젝트-목표와-대안들">프로젝트 목표와 대안들</a><br>
  [<a href="../README.md">English</a>]
  [<a href="README-zh.md">中文</a>]
  [<a href="README-ja.md">日本語</a>]
  [한국어]
  [<a href="README-ru.md">Русский</a>]
</p>

### 문법 강조

`bat`은 다양한 프로그래밍 및 마크업 언어의 문법 강조(syntax highlighting) 기능을
지원합니다:

![Syntax highlighting example](https://imgur.com/rGsdnDe.png)

### Git 통합

`bat`은 `git`을 통해 인덱스와 함께 변경분을 표시합니다
(왼쪽 사이드바를 확인하세요):

![Git integration example](https://i.imgur.com/2lSW4RE.png)

### 비인쇄 문자 처리

`-A`/`--show-all` 옵션을 사용하여 비인쇄 문자를 표시 및 강조할 수 있습니다:

![Non-printable character example](https://i.imgur.com/WndGp9H.png)

### 자동 페이징

`bat`은 기본적으로 한 화면에 비해 출력이 큰 경우 `less`와 같은 페이저(pager)로
출력을 연결(pipe)합니다.
만약 `bat`을 언제나 `cat`처럼 작동하게 하려면 (출력을 페이지하지 않기),
`--paging=never` 옵션을 커맨드 라인이나 설정 파일에 넣을 수 있습니다.
셸(shell) 설정에서 `cat`을 `bat`의 alias로 사용하려면,
`alias cat='bat --paging=never'`를 써서 기본 행동을 유지할 수 있습니다.

### 파일 연결(concatenation)

페이저(pager)를 사용하더라도 `bat`은 파일들을 연결(concatenate)할 수 있습니다
:wink:.
`bat`이 비대화형(non-interactive) 터미널(예를 들어, 다른 프로세스나 파일에
연결(pipe)한 경우)을 감지하면, `bat`은 `--pager` 옵션의 값과 상관없이 `cat`과
동일하게 파일 내용을 그대로 출력합니다.

## 사용법

터미널에 하나의 파일 표시하기

```bash
> bat README.md
```

여러 파일 한 번에 보여주기

```bash
> bat src/*.rs
```

stdin에서 읽고, 자동으로 맞는 문법 결정하기 (참고로, 문법 강조는 파일의 첫
줄만으로 문법이 결정될 수 있을 때만 작동합니다.
이는 보통 `#!/bin/sh`와 같은 셔뱅(shebang)으로 판단합니다.)

```bash
> curl -s https://sh.rustup.rs | bat
```

stdin에서 읽고, 명시적으로 언어 지정하기

```bash
> yaml2json .travis.yml | json_pp | bat -l json
```

비인쇄 문자 표시 및 강조하기
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

### 다른 도구들과 통합하기

#### `fzf`

`bat`을 [`fzf`](https://github.com/junegunn/fzf)의 프리뷰로 쓸 수 있습니다.
이를 위해서는 `bat`의 `--color=always` 옵션으로 항상 컬러 출력이 나오게 해야
합니다.
또한 `--line-range` 옵션으로 긴 파일의 로드 시간을 제한할 수 있습니다:
```bash
fzf --preview 'bat --color=always --style=numbers --line-range=:500 {}'
```
더 많은 정보는
[`fzf`의 `README`](https://github.com/junegunn/fzf#preview-window)를 참고하세요.

#### `find`와 `fd`

`find`의 `-exec` 옵션을 사용하여 모든 검색 결과를 `bat`로 미리 볼 수 있습니다: 
```bash
find … -exec bat {} +
```

[`fd`](https://github.com/sharkdp/fd)를 사용하는 경우, `-X`/`--exec-batch`
옵션을 이용하여 동일하게 사용할 수 있습니다:
```bash
fd … -X bat
```

#### `ripgrep`

[`batgrep`](https://github.com/eth-p/bat-extras/blob/master/doc/batgrep.md)을
통해 `bat`로 [`ripgrep`](https://github.com/BurntSushi/ripgrep)의 검색 결과를
출력할 수 있습니다.

```bash
batgrep needle src/
```

#### `tail -f`

`bat`와 `tail -f`를 함께 사용하여 주어진 파일을 문법 강조하며 지속적으로
모니터할 수 있습니다.
```bash
tail -f /var/log/pacman.log | bat --paging=never -l log
```
참고로 이 작업을 하려면 페이징 기능을 꺼야 합니다.
또한 이 경우 문법을 자동 감지할 수 없기 때문에, 적용할 문법을 직접 지정해야
합니다 (`-l log`).

#### `git`

`bat`과 `git show`를 함께 사용하여 주어진 파일의 이전 버전을 올바른 문법 강조로
볼 수 있습니다:
```bash
git show v0.6.0:src/main.rs | bat -l rs
```

#### `git diff`

`bat`과 `git diff`를 함께 사용하여 수정된 코드 주위의 줄들을 올바른 문법 강조로
볼 수 있습니다:
```bash
batdiff() {
    git diff --name-only --relative --diff-filter=d -z | xargs -0 bat --diff
}
```
이것을 별도의 도구로 쓰고 싶다면
[`bat-extras`](https://github.com/eth-p/bat-extras)의 `batdiff`를 확인해 보세요.

Git과 diff의 더 많은 지원을 원한다면
[`delta`](https://github.com/dandavison/delta)를 확인해 보세요.

#### `xclip`

`bat` 출력에 줄 번호와 Git 수정 내역이 포함되어서 파일의 내용을 복사하기
어려울 수 있습니다.
이 경우에는 `bat`의 `-p`/`--plain` 옵션을 사용하거나 간단히 `xclip`으로 출력을
연결(pipe)하면 됩니다:
```bash
bat main.cpp | xclip
```
`bat`는 출력이 우회되고 있다는 것을 감지하여 파일 내용 그대로를 출력합니다.

#### `man`

`MANPAGER` 환경 변수 설정을 통해 `bat`을 `man`의 컬러 페이저(pager)로 쓸 수
있습니다.

```bash
export MANPAGER="sh -c 'col -bx | bat -l man -p'"
man 2 select
```
(Debian이나 Ubuntu를 사용한다면 `bat`을 `batcat`으로 치환하세요.)

포팻 문제가 발생한다면, `MANROFFOPT="-c"`을 써야 할 수 있습니다.

이 기능을 포함한 새로운 명령어를 선호한다면,
[`batman`](https://github.com/eth-p/bat-extras/blob/master/doc/batman.md)을 쓸
수도 있습니다. 

참고로 [Manpage 문법](../assets/syntaxes/Manpage.sublime-syntax)은 본 저장소에서
개발 중에 있으며, 아직 더 손봐야 합니다.

또한, 이는 Mandoc의 `man` 구현에서
[작동하지 않습니다](https://github.com/sharkdp/bat/issues/1145).

#### `prettier` / `shfmt` / `rustfmt`

[`prettybat`](https://github.com/eth-p/bat-extras/blob/master/doc/prettybat.md)
스크립트는 코드를 포맷하고 `bat`으로 출력하는 래퍼(wrapper)입니다.


## 설치

[![Packaging status](https://repology.org/badge/vertical-allrepos/bat-cat.svg?columns=3&exclude_unsupported=1)](https://repology.org/project/bat-cat/versions)

### Ubuntu에서 (`apt` 사용)
*... 그리고 다른 Debian 기반의 Linux 배포판들에서.*

`bat`은 [Ubuntu](https://packages.ubuntu.com/eoan/bat)와
[Debian](https://packages.debian.org/sid/bat) 패키지 배포 과정에 도입되는 중이며,
Eoan 19.10 버전의 Ubuntu에서부터 제공됩니다.
현재 Debian에서는 불안정한 "Sid" 브랜치에서만 `bat`이 제공됩니다.

만약 충분히 최신 버전의 Ubuntu/Debian이 설치되어 있다면 간단히 다음을 실행하세요:

```bash
apt install bat
```

**중요**: 만약 `bat`을 이와 같이 설치한다면, ([다른 패키지와의 이름
충돌](https://github.com/sharkdp/bat/issues/982)로 인하여) `bat` 대신에
`batcat`이라는 이름의 실행 파일로 설치될 수 있음을 참고하세요.
이에 따른 문제들과 다른 배포판들과의 일관성을 위하여 `bat -> batcat` symlink
혹은 alias를 설정할 수 있습니다:
``` bash
mkdir -p ~/.local/bin
ln -s /usr/bin/batcat ~/.local/bin/bat
```

### Ubuntu에서 (가장 최신 `.deb` 패키지들 사용)
*... 그리고 다른 Debian 기반의 Linux 배포판들에서.*

만약 여러분이 설치한 Ubuntu/Debian에 패키지가 배포되지 않거나 가장 최신 릴리즈된
`bat`을 원한다면, [릴리즈 페이지](https://github.com/sharkdp/bat/releases)에서
다음과 같이 `.deb` 패키지를 받아 설치하세요:

```bash
sudo dpkg -i bat_0.18.3_amd64.deb  # adapt version number and architecture
```

### Alpine Linux에서

적절한 저장소가 활성화되어 있다면, 공식 소스를 통해
[`bat` 패키지](https://pkgs.alpinelinux.org/packages?name=bat)를 설치할 수
있습니다:

```bash
apk add bat
```

### Arch Linux에서

공식 소스를 통해
[`bat` 패키지](https://www.archlinux.org/packages/extra/x86_64/bat/)를
설치할 수 있습니다:

```bash
pacman -S bat
```

### Fedora에서

공식
[Fedora 모듈](https://docs.fedoraproject.org/en-US/modularity/using-modules/)
저장소에서
[`bat` 패키지](https://koji.fedoraproject.org/koji/packageinfo?packageID=27506)를
설치할 수 있습니다:

```bash
dnf install bat
```

### Funtoo Linux에서

dev-kit을 통해 [`bat` 패키지](https://github.com/funtoo/dev-kit/tree/1.4-release/sys-apps/bat)를 설치할 수 있습니다: 

```bash
emerge sys-apps/bat
```

### Gentoo Linux에서

공식 소스를 통해
[`bat` 패키지](https://packages.gentoo.org/packages/sys-apps/bat)를 설치할 수
있습니다:

```bash
emerge sys-apps/bat
```

### Void Linux에서

xbps-install을 이용해 `bat`을 설치할 수 있습니다:
```bash
xbps-install -S bat
```

### Termux에서

pkg를 이용해 `bat`을 설치할 수 있습니다:
```bash
pkg install bat
```

### FreeBSD에서

pkg를 이용하여 미리 컴파일된
[`bat` 패키지](https://www.freshports.org/textproc/bat)를 설치할 수 있습니다:

```bash
pkg install bat
```

또는 FreeBSD 포트에서 직접 빌드할 수도 있습니다:

```bash
cd /usr/ports/textproc/bat
make install
```

### nix를 써서

[nix package manager](https://nixos.org/nix)를 이용해 `bat`을 설치할 수
있습니다:

```bash
nix-env -i bat
```

### openSUSE에서

zypper를 이용해 `bat`을 설치할 수 있습니다:

```bash
zypper install bat
```

### snap 패키지를 써서

지금으로서는 추천하는 snap 패키지가 없습니다.
제공되는 패키지들이 존재할 수는 있지만, 공식적으로 지원되지 않으며
[문제](https://github.com/sharkdp/bat/issues/1519)가 있을 수 있습니다.


### macOS (또는 Linux)에서 Homebrew를 써서

[macOS의 Homebrew](https://formulae.brew.sh/formula/bat) 또는
[Linux의 Homebrew](https://formulae.brew.sh/formula-linux/bat)를 이용하여
`bat`을 설치할 수 있습니다.

```bash
brew install bat
```

### macOS에서 MacPorts를 써서

[MacPorts](https://ports.macports.org/port/bat/summary)를 이용하여 `bat`을
설치할 수 있습니다:

```bash
port install bat
```

### Windows에서

Windows에서 `bat`을 설치할 수 있는 몇 가지 옵션들이 있습니다.
먼저 `bat`을 설치한 후,
["Windows에서 `bat` 사용하기"](#windows에서-bat-사용하기) 섹션을 살펴보세요.

#### 전제 조건

[Visual C++ 재배포 가능](https://support.microsoft.com/en-us/help/2977003/the-latest-supported-visual-c-downloads)
패키지를 설치해야 합니다.

#### Chocolatey를 써서

[Chocolatey](https://chocolatey.org/packages/Bat)를 이용해 `bat`을 설치할 수
있습니다:
```bash
choco install bat
```

#### Scoop을 써서

[scoop](https://scoop.sh/)을 이용해 `bat`을 설치할 수 있습니다:
```bash
scoop install bat
```

#### 사전 빌드된 바이너리들로

[릴리즈 페이지](https://github.com/sharkdp/bat/releases)에서 사전 빌드된
바이너리를 다운받을 수 있습니다.

[Visual C++ 재배포 가능](https://support.microsoft.com/en-us/help/2977003/the-latest-supported-visual-c-downloads)
패키지를 설치해야 합니다.

### 바이너리들로

[릴리즈 페이지](https://github.com/sharkdp/bat/releases)에서 다양한 아키텍처를
위해 사전 빌드된 버전들을 확인할 수 있습니다.
정적 링크 바이너리들은 파일 이름에 `musl` 이 포함된 아카이브들을 확인하세요.

### 소스에서

`bat`의 소스를 빌드하기 위해서는, Rust 1.74.0 이상이 필요합니다.
`cargo`를 이용해 전부 빌드할 수 있습니다:

```bash
cargo install --locked bat
```

참고로 man 페이지나 셸 자동 완성 파일과 같은 부가 파일들은 이 방법으로 설치될 수
없습니다.
이것들은 `cargo`에 의해 생성이 되고 (`build` 밑의) cargo 타켓 폴더에서 찾을 수
있습니다.

## 사용자화

### 문법 강조 테마

`bat --list-themes`을 사용하여 사용 가능한 문법 강조 테마들의 목록을 확인할 수
있습니다.
`TwoDark` 테마를 선택하려면, `--theme=TwoDark` 옵션과 함께 `bat`을 사용하거나
`BAT_THEME` 환경 변수를 `TwoDark`로 설정하세요.
셸 시작 파일에 `export BAT_THEME="TwoDark"` 를 정의해 영구적으로 설정할 수
있습니다.
이 밖에 `bat`의 [설정 파일](#설정-파일)을 이용할 수 있습니다.

만약 다른 테마들을 사용하여 특정 파일을 보고 싶다면, 다음 명령어를 쓸 수
있습니다(이 경우 [`fzf`](https://github.com/junegunn/fzf)가 필요합니다.)

```bash
bat --list-themes | fzf --preview="bat --theme={} --color=always /path/to/file"
```

`bat`은 기본적으로 어두운 배경에 적합합니다.
그러나 밝은 배경의 터미널을 사용한다면 `GitHub`이나 `OneHalfLight`과 같은 테마가
더 잘 어울립니다.
아래 [새로운 테마 추가하기](#새로운-테마-추가하기) 섹션에 따라 커스텀 테마를
사용할 수도 있습니다.

### 8비트 테마

`bat`은 트루컬러 지원이 되더라도 항상
[8비트 색상](https://en.wikipedia.org/wiki/ANSI_escape_code#Colors)을 사용하는
세 개의 테마가 있습니다.

- `ansi`는 어떤 터미널에서도 무난하게 보입니다. 이는 3비트 색상을 사용합니다:
  검정, 빨강, 녹색, 노랑, 파랑, 마젠타, 시안, 하양.
- `base16`은 [base16](https://github.com/tinted-theming/home) 터미널 테마를 위해
  디자인되었습니다.
  이는 [base16 스타일 가이드라인](https://github.com/tinted-theming/home/blob/main/styling.md)에
  따라 4비트 색상(3비트 색상에 밝은 변형 추가)을 사용합니다.
- `base16-256`는 [base16-shell](https://github.com/tinted-theming/base16-shell)을
  위해 디자인되었습니다.
  이는 16부터 21의 일부 밝은 색상을 8비트 색상으로 대치합니다.
  단지 256-색상 터미널을 쓰지만 base16-shell을 쓰지 않는다고 해서 이것을
  사용하지 **마십시오**.

이들 테마는 더 제한적이지만, 트루컬러 테마에 비해 두 장점이 있습니다:

- 이들은 3비트 혹은 4비트 색상을 쓰는 다른 터미널 소프트웨어와 더 잘
  어울립니다.
- 만약 터미널 테마를 바꾼다면, 이미 화면 상의 `bat`의 출력도 이에 맞추어
  업데이트됩니다.

### 출력 스타일

`--style` 옵션을 이용하면 `bat`의 출력 모양을 조절할 수 있습니다.
예를 들어, `--style=numbers,changes`를 통해 Git 변경분과 줄 번호는 출력하지만
격자와 파일 헤더는 출력하지 않을 수 있습니다.
`BAT_STYLE` 환경 변수를 정의하여 이러한 수정을 영구적으로 하거나 `bat`의
[설정 파일](#설정-파일)을 사용하세요.

### 새로운 문법 / 언어 정의 추가하기

만약 `bat`에서 특정 문법이 지원되지 않을 경우, 다음의 절차를 통해 현재 `bat`
설치본에 새로운 문법을 쉽게 추가할 수 있습니다.

`bat`은 문법 강조를 위해 훌륭한
[`syntect`](https://github.com/trishume/syntect/) 라이브러리를 사용합니다.
`syntect`는 임의의 [Sublime Text의 `.sublime-syntax`
파일](https://www.sublimetext.com/docs/3/syntax.html)과 테마를 읽을 수 있습니다.

[Package Control](https://packagecontrol.io/)에 Sublime 문법 패키지를 찾는
방법이 잘 정리되어 있습니다.
일단 문법을 찾았다면:

1. 문법 정의 파일들을 넣을 폴더를 만듭니다:

  ```bash
  mkdir -p "$(bat --config-dir)/syntaxes"
  cd "$(bat --config-dir)/syntaxes"

  # Put new '.sublime-syntax' language definition files
  # in this folder (or its subdirectories), for example:
  git clone https://github.com/tellnobody1/sublime-purescript-syntax
  ```

2. 이제 다음 명령어를 통해 파일들을 파싱(parse)하여 바이너리 캐시를 만듭니다.

  ```bash
  bat cache --build
  ```

3. 마지막으로, `bat --list-languages`로 새로 추가한 언어가 사용 가능한지
  확인합니다.

  만약 기본 설정으로 돌아갈 일이 생긴다면, 다음 명령어를 이용합니다:

  ```bash
  bat cache --clear
  ```

4. 만약 특정 문법이 `bat`에 기본적으로 포함되어 있어야 한다고 생각한다면, 방침과
  절차를 [여기](../doc/assets.md)서 읽은 후 "문법 요청(syntax request)"을 열어
  주세요: [문법 요청하기](https://github.com/sharkdp/bat/issues/new?labels=syntax-request&template=syntax_request.md).

### 새로운 테마 추가하기

이 과정은 새로운 문법 정의 추가 방식과 매우 비슷합니다.

먼저, 새로운 문법 강조 테마 폴더를 만듭니다.
```bash
mkdir -p "$(bat --config-dir)/themes"
cd "$(bat --config-dir)/themes"

# Download a theme in '.tmTheme' format, for example:
git clone https://github.com/greggb/sublime-snazzy

# Update the binary cache
bat cache --build
```

마지막으로 `bat --list-themes`을 통해 새로 추가한 테마들이 사용 가능한지
확인합니다.

### 파일 타입 설정을 추가하거나 변경하기

새로운 파일 이름 패턴을 추가하려면 (혹은 이미 존재하는 것을 변경하려면)
`--map-syntax` 커맨드 라인 옵션을 사용하세요.
이 옵션은 `pattern:syntax` 꼴의 인자를 받습니다.
이때 `pattern`은 파일 이름과 절대 파일 경로를 매치할 글로브(glob) 패턴입니다.
`syntax` 부분은 지원되는 언어의 전체 이름입니다
(`bat --list-languages`를 통해 개요를 확인하세요).

참고: 이 옵션은 커맨드 라인에 넘겨 주는 것보다는 `bat`의 설정 파일에 넣는 것이
좋을 것입니다 (아래를 보세요).

예시: "INI" 문법 강조를 `.conf` 파일 확장자의 모든 파일에 적용하려면, 다음을
사용하세요:
```bash
--map-syntax='*.conf:INI'
```

예시: `.ignore`(완전 일치)이라는 이름의 모든 파일을 "Git Ignore" 문법으로
열려면, 다음을 사용하세요:
```bash
--map-syntax='.ignore:Git Ignore'
```

예시: `/etc/apache2`의 하위 폴더들에 있는 모든 `.conf` 파일들을 "Apache Conf"
문법으로 열려면, 다음을 사용하세요 (이 대응(mapping)은 이미 내장되어 있습니다):
```bash
--map-syntax='/etc/apache2/**/*.conf:Apache Conf'
```

### 다른 페이저 사용하기

`bat`은 환경 변수 `PAGER`에 명시된 페이저를 사용합니다.
이 변수가 정의되어 있지 않다면, `less`가 기본으로 사용됩니다.
만약 다른 페이저를 사용하고 싶다면, `PAGER` 변수를 수정하거나 `BAT_PAGER` 환경
변수를 설정하여 `PAGER`의 설정을 오버라이드(override)할 수 있습니다.

만약 커맨드라인 인수들을 페이저에게 넘겨 주려면, `PAGER`/`BAT_PAGER` 변수로
설정할 수 있습니다:

```bash
export BAT_PAGER="less -RF"
```

환경 변수를 사용하는 대신, `bat`의 [설정 파일](#설정-파일)로 페이저를 설정할
수도 있습니다 (`--pager` 옵션).

**참고**: 기본적으로, 페이저가 `less`로 설정되어 있다면 (그리고 커맨드 라인
옵션이 지정되어 있지 않다면), `bat`은 다음 옵션들을 페이저로 넘겨줍니다:
`-R`/`--RAW-CONTROL-CHARS`, `-F`/`--quit-if-one-screen` 그리고 `-X`/`--no-init`.
마지막 옵션(`-X`)은 530 이전 버전의 `less`에만 사용됩니다.

`-R` 옵션은 ANSI 색상을 올바르게 해석하기 위해 필요합니다.
두 번째 옵션(`-F`)은 출력 크기가 터미널의 세로 크기보다 작을 경우 less가 즉시
종료되도록 합니다.
이는 작은 파일을 다룰 때 페이저를 종료하기 위해 `q`를 누를 필요 없어서
편리합니다.
세 번째 옵션(`-X`)는 예전 버전의 `less`에 있는 `--quit-if-one-screen` 기능의
버그를 고치기 위해 필요합니다.
안타깝게도, 이는 `less`의 마우스 휠 지원과 호환되지 않습니다.

`less`의 예전 버전에서 마우스 휠 기능을 활성화하려면, `-R` 옵션을 넘겨주세요
(위의 예제처럼, 이 옵션은 quit-if-one-screen 기능을 비활성화합니다).
less 530과 이후 버전에서는 그대로 사용할 수 있습니다.

### 들여쓰기

`bat`은 페이저에 의존하지 않고 탭을 4 스페이스로 확장합니다.
이를 변경하려면 간단히 `--tabs` 인자에 표시되기를 원하는 스페이스 개수를
추가하세요.

**참고**: (`bat`의 `--pager` 인자 혹은 `less`의 `LESS` 환경 변수를 통해)
페이저의 탭 길이를 지정하는 것은 효과가 없을 것인데, 이는 페이저가 이미
스페이스로 확장된 탭을 받기 때문입니다.
이 기능은 사이드바에 의한 들여쓰기 문제를 회피하기 위해 추가되었습니다.
`bat`을 `--tabs=0`과 함께 호출하면 이를 오버라이드하여 페이저가 탭을 처리하게
합니다.

### 다크 모드

macOS에서 다크 모드를 사용하고 있다면, `bat`가 OS 테마에 따라 다른 테마를
사용하도록 구성할 수 있습니다.
아래 스니펫은 _다크 모드_에서는 `default` 테마를, _라이트 모드_에서는 `GitHub`
테마를 사용하는 방법입니다.

```bash
alias cat="bat --theme=\$(defaults read -globalDomain AppleInterfaceStyle &> /dev/null && echo default || echo GitHub)"
```

## 설정 파일

`bat`는 설정 파일로도 사용자화 할 수 있습니다.
설정 파일의 위치는 운영 체제에 따라 다릅니다.
아래 커맨드를 통해 시스템의 기본 경로를 확인할 수 있습니다.
```
bat --config-file
```

또는, `BAT_CONFIG_PATH` 환경 변수를 사용하여 `bat`가 설정 파일의 기본 경로
이외의 위치를 사용하도록 할 수 있습니다.
```bash
export BAT_CONFIG_PATH="/path/to/bat.conf"
```

기본 설정 파일은 `--generate-config-file` 옵션으로 생성할 수 있습니다.
```bash
bat --generate-config-file
```

### 포맷

설정 파일은 단순히 커맨드 라인 인자들의 리스트입니다.
`bat --help`로 가능한 모든 옵션과 값들을 확인하세요.
추가적으로, 줄 앞에 `#` 문자를 추가해 주석을 넣을 수 있습니다.

설정 파일 예시:
```bash
# "TwoDark" 테마 설정하기
--theme="TwoDark"

# 줄 번호, Git 변경 내용, 파일 헤더 보이기 (격자 없이)
--style="numbers,changes,header"

# 터미널에서 이탤릭체 쓰기 (일부 터미널에서 미지원)
--italic-text=always

# Arduino .ino 파일에 C++ 문법 쓰기
--map-syntax "*.ino:C++"
```

## Windows에서 `bat` 사용하기

`bat`는 대부분의 경우 Windows에서 기본적으로 잘 작동하지만, 일부 기능은 추가적인
구성이 필요할 수 있습니다.

#### 전제 조건

[Visual C++ 재배포 가능](https://support.microsoft.com/en-us/help/2977003/the-latest-supported-visual-c-downloads)
패키지를 설치해야 합니다.

### 페이징

Windows는 `more` 형식의 매우 제한된 페이저만 포함합니다.
Windows용 `less` 바이너리는
[공식 홈페이지](http://www.greenwoodsoftware.com/less/download.html)나
[Chocolatey](https://chocolatey.org/packages/Less)를 통해 다운로드 받을 수
있습니다.
이를 사용하려면 디렉터리 안의 바이너리를 `PATH`에 넣거나
[환경 변수로 정의](#using-a-different-pager)하세요.
[Chocolatey 패키지](#on-windows)는 `less`를 자동으로 설치합니다.

### 색상

Windows 10은
[v1511](https://en.wikipedia.org/wiki/Windows_10_version_history#Version_1511_(November_Update))부터
기본적으로 `conhost.exe`(Command Prompt)와 PowerShell에서 색상을 지원하며,
최신 버전의 bash에서도 색상을 지원합니다.
이전 버전의 Windows에서는, [ConEmu](https://conemu.github.io/)가 포함된
[Cmder](http://cmder.net/)를 사용할 수 있습니다.

**참고:** Git과 MSYS 버전의 `less`는 Windows에서 색상을 올바르게 해석하지
않습니다.
다른 페이저가 설치되어 있지 않은 경우, `--paging=never`을 넘겨주거나
`BAT_PAGER`을 빈 문자열로 설정하여 페이징을 완전히 비활성화 할 수 있습니다.

### Cygwin

Windows에서의 `bat`은 기본적으로 Cygwin의 unix 스타일 경로(`/cygdrive/*`)를
지원하지 않습니다.
Cygwin 절대 경로를 인자로 받았을 때, `bat`은 다음과 같은 오류를 반환합니다:
`The system cannot find the path specified. (os error 3)`

이는 wrapper를 만들거나 다음 함수를 `.bash_profile`에 추가하여 해결할 수
있습니다:

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

## 문제 해결

### 터미널과 색상

`bat`은 터미널의 트루컬러 지원 여부와 상관 없이 동작합니다.
그러나 대부분 문법 강조 테마의 색상은 8비트 색상에 최적화되어 있지 않습니다.
따라서 24비트 트루컬러 지원이 되는 터미널(`terminator`, `konsole`, `iTerm2`,
...)을 사용하는 것을 적극 권장합니다.
트루컬러를 지원하는 터미널들과 더 자세한 정보는
[이 글](https://gist.github.com/XVilka/8346728)에서 찾아보실 수 있습니다.

사용하고 있는 트루컬러 터미널에서 `COLORTERM` 변수를 `truecolor` 혹은
`24bit`로 설정되어 있는지 확인하세요.
그렇지 않을 경우, `bat`은 24비트 확장열(escape sequence)이 지원되는지 여부를
판단할 수 없습니다 (그리고 8비트 색상을 사용합니다).

### 줄 번호와 격자가 잘 보이지 않는 경우

다른 테마를 사용해 보세요 (`bat --list-themes`에서 목록을 볼 수 있습니다).
`OneHalfDark`와 `OneHalfLight` 테마는 더 밝은 눈금과 선의 색상을 사용합니다.

### 파일 인코딩

`bat`은 기본적으로 UTF-8과 UTF-16을 지원합니다.
다른 모든 종류의 파일 인코딩에 대해서는, 일반적으로 인코딩을 자동으로 판별하는
방법이 없기 때문에 먼저 UTF-8으로 변환해야 할 수 있습니다.
이를 위해 `iconv`를 사용할 수 있습니다.
예시: Latin-1(ISO-8859-1)로 인코딩된 PHP 파일은 다음과 같이 처리할 수 있습니다:
``` bash
iconv -f ISO-8859-1 -t UTF-8 my-file.php | bat
```
참고: `bat`으로 문법 자동 감지가 되지 않는 경우에는 `-l`/`--language` 옵션을
사용할 수 있습니다.

## 개발

```bash
# 모든 서브모듈을 받기 위해 재귀적으로 복제하기
git clone --recursive https://github.com/sharkdp/bat

# (디버그 버전) 빌드
cd bat
cargo build --bins

# 단위 테스트와 통합 테스트 실행
cargo test

# (배포 버전) 설치
cargo install --locked

# 수정된 문법과 테마가 적용된 bat 바이너리 빌드
bash assets/create.sh
cargo install --locked --force
```

`bat`의 pretty-printing 기능을 라이브러리로 사용하는 애플리케이션을 만들고
싶다면, [API 문서](https://docs.rs/bat/)를 살펴보세요.
참고로 `bat`에 라이브러리로써 의존한다면, `regex-onig`나 `regex-fancy`를
기능으로 사용해야 합니다.

## 기여하기

[`CONTRIBUTING.md`](../CONTRIBUTING.md) 가이드를 살펴보세요.

## 메인테이너들

- [sharkdp](https://github.com/sharkdp)
- [eth-p](https://github.com/eth-p)
- [keith-hall](https://github.com/keith-hall)
- [Enselic](https://github.com/Enselic)

## 보안 취약점

만약 `bat`의 취약점을 발견하였다면, [David Peter](https://david-peter.de/)에게 메일로 연락주시기 바랍니다. 

## 프로젝트 목표와 대안들

`bat`은 다음과 같은 목표를 달성하려고 합니다:

- 아름답고 발전된 문법 강조 기능 제공
- Git과의 연동을 통한 파일 변경 내용 확인
- (POSIX) `cat`의 대체제
- 사용자 친화적인 커맨드 라인 인터페이스 제공

비슷한 프로그램들을 찾고 있다면 많은 대안들이 있습니다.
비교는 [이 문서](../doc/alternatives.md)를 참조해 주세요.

## 라이센스
Copyright (c) 2018-2021 [bat-developers](https://github.com/sharkdp/bat).

`bat`는 여러분의 선택에 따라 MIT 라이센스 또는 Apache 라이센스 2.0의 조건에 따라
배포됩니다.

라이센스의 세부사항은 [LICENSE-APACHE](../LICENSE-APACHE)와
[LICENSE-MIT](../LICENSE-MIT)를 참조하세요.

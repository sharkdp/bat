#!/usr/bin/env bash
# Building and packaging for release

set -ex

build() {
    cargo build --target "$TARGET" --release --verbose
}

pack() {
    local tempdir
    local out_dir
    local package_name
    local gcc_prefix

    tempdir=$(mktemp -d 2>/dev/null || mktemp -d -t tmp)
    out_dir=$(pwd)
    package_name="$PROJECT_NAME-$TRAVIS_TAG-$TARGET"

    if [[ $TARGET == "arm-unknown-linux-gnueabihf" ]]; then
        gcc_prefix="arm-linux-gnueabihf-"
    elif [[ $TARGET == "aarch64-unknown-linux-gnu" ]]; then
        gcc_prefix="aarch64-linux-gnu-"
    else
        gcc_prefix=""
    fi

    # create a "staging" directory
    mkdir "$tempdir/$package_name"
    mkdir "$tempdir/$package_name/autocomplete"

    # copying the main binary
    cp "target/$TARGET/release/$PROJECT_NAME" "$tempdir/$package_name/"
    "${gcc_prefix}"strip "$tempdir/$package_name/$PROJECT_NAME"

    # manpage, readme and license
    cp "doc/$PROJECT_NAME.1" "$tempdir/$package_name"
    cp README.md "$tempdir/$package_name"
    cp LICENSE-MIT "$tempdir/$package_name"
    cp LICENSE-APACHE "$tempdir/$package_name"

    # various autocomplete
    # TODO: disabled for now, see issue #372
    # cp target/"$TARGET"/release/build/"$PROJECT_NAME"-*/out/"$PROJECT_NAME".bash "$tempdir/$package_name/autocomplete/${PROJECT_NAME}.bash-completion"
    # cp target/"$TARGET"/release/build/"$PROJECT_NAME"-*/out/"$PROJECT_NAME".fish "$tempdir/$package_name/autocomplete"
    # cp target/"$TARGET"/release/build/"$PROJECT_NAME"-*/out/_"$PROJECT_NAME" "$tempdir/$package_name/autocomplete"

    # archiving
    pushd "$tempdir"
    tar czf "$out_dir/$package_name.tar.gz" "$package_name"/*
    popd
    rm -r "$tempdir"
}

make_deb() {
    local tempdir
    local architecture
    local version
    local dpkgname
    local conflictname
    local gcc_prefix
    local homepage
    local maintainer

    homepage="https://github.com/sharkdp/bat"
    maintainer="David Peter <mail@david-peter.de>"

    case $TARGET in
        x86_64*)
            architecture=amd64
            gcc_prefix=""
            ;;
        i686*)
            architecture=i386
            gcc_prefix=""
            ;;
        aarch64*)
            architecture=arm64
            gcc_prefix="aarch64-linux-gnu-"
            ;;
        arm*hf) 
            architecture=armhf  
            gcc_prefix="arm-linux-gnueabihf-"   
            ;;
        *)
            echo "make_deb: skipping target '${TARGET}'" >&2
            return 0
            ;;
    esac
    version=${TRAVIS_TAG#v}
    if [[ $TARGET = *musl* ]]; then
      dpkgname=$PROJECT_NAME-musl
      conflictname=$PROJECT_NAME
    else
      dpkgname=$PROJECT_NAME
      conflictname=$PROJECT_NAME-musl
    fi

    tempdir=$(mktemp -d 2>/dev/null || mktemp -d -t tmp)

    # copy the main binary
    install -Dm755 "target/$TARGET/release/$PROJECT_NAME" "$tempdir/usr/bin/$PROJECT_NAME"
    "${gcc_prefix}"strip "$tempdir/usr/bin/$PROJECT_NAME"

    # manpage
    install -Dm644 "doc/$PROJECT_NAME.1" "$tempdir/usr/share/man/man1/$PROJECT_NAME.1"
    gzip --best "$tempdir/usr/share/man/man1/$PROJECT_NAME.1"

    # readme and license
    install -Dm644 README.md "$tempdir/usr/share/doc/$PROJECT_NAME/README.md"
    install -Dm644 LICENSE-MIT "$tempdir/usr/share/doc/$PROJECT_NAME/LICENSE-MIT"
    install -Dm644 LICENSE-APACHE "$tempdir/usr/share/doc/$PROJECT_NAME/LICENSE-APACHE"
    cat > "$tempdir/usr/share/doc/$PROJECT_NAME/copyright" <<EOF
Format: http://www.debian.org/doc/packaging-manuals/copyright-format/1.0/
Upstream-Name: $PROJECT_NAME
Source: $homepage

Files: *
Copyright: $maintainer
License: Apache-2.0 or MIT

License: Apache-2.0
 On Debian systems, the complete text of the Apache-2.0 can be found in the
 file /usr/share/common-licenses/Apache-2.0.

License: MIT
 Permission is hereby granted, free of charge, to any
 person obtaining a copy of this software and associated
 documentation files (the "Software"), to deal in the
 Software without restriction, including without
 limitation the rights to use, copy, modify, merge,
 publish, distribute, sublicense, and/or sell copies of
 the Software, and to permit persons to whom the Software
 is furnished to do so, subject to the following
 conditions:
 .
 The above copyright notice and this permission notice
 shall be included in all copies or substantial portions
 of the Software.
 .
 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF
 ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
 TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
 PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT
 SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR
 IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 DEALINGS IN THE SOFTWARE.
EOF

    # Control file
    mkdir "$tempdir/DEBIAN"
    cat > "$tempdir/DEBIAN/control" <<EOF
Package: $dpkgname
Version: $version
Section: utils
Priority: optional
Maintainer: David Peter <mail@david-peter.de>
Architecture: $architecture
Provides: $PROJECT_NAME
Conflicts: $conflictname
Description: A cat(1) clone with wings.
 A cat(1) clone with syntax highlighting and Git integration.
EOF

    fakeroot dpkg-deb --build "$tempdir" "${dpkgname}_${version}_${architecture}.deb"
}


main() {
    build
    pack
    if [[ $TARGET = *linux* ]]; then
      make_deb
    fi
}

main

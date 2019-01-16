#!/bin/bash

ASSET_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# Always remove the local cache to avoid any confusion
bat cache --clear

# TODO: Remove this (and the reverse part below) when
# https://github.com/trishume/syntect/issues/222 has been fixed
JAVADOC_FILE="${ASSET_DIR}/syntaxes/Packages/Java/JavaDoc.sublime-syntax"
JAVADOC_PATCH="${ASSET_DIR}/JavaDoc.sublime-syntax.patch"
patch "$JAVADOC_FILE" "$JAVADOC_PATCH"

bat cache --init --blank --source="$ASSET_DIR" --target="$ASSET_DIR"

patch -R "$JAVADOC_FILE" "$JAVADOC_PATCH"

#!/bin/bash

ASSET_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

DEFAULT_MARKDOWN_SYNTAX="$ASSET_DIR/syntaxes/Packages/Markdown"

rm -rf "$DEFAULT_MARKDOWN_SYNTAX"

bat cache --init --source="$ASSET_DIR" --target="$ASSET_DIR"

git -C "$ASSET_DIR/syntaxes/Packages" checkout "$DEFAULT_MARKDOWN_SYNTAX"

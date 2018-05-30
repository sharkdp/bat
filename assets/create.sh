#!/bin/bash

ASSET_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

bat cache --init --source="$ASSET_DIR" --target="$ASSET_DIR"

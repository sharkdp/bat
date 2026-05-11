#!/usr/bin/env bash
set -euo pipefail

ASSET_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
REPO_DIR="$ASSET_DIR/.."

# Ensure submodules are initialized.
update_submodules() {
	local submodule
	local submodule_prompt=unspecified
	local submodule_path

	{
		while { read -r submodule && read -r submodule_path; } <&3; do
			if ! [[ -d "${REPO_DIR}/.git/modules/${submodule}" ]] && [[ -d "${REPO_DIR}/${submodule_path}" ]]; then
				if [[ "$submodule_prompt" = "unspecified" ]]; then
					echo "One or more submodules were found to be uninitialized."
					printf "Initialize and update them? [Y/n] "
					read -r submodule_prompt
				fi

				case "$submodule_prompt" in
					y|yes|'') {
						git -C "$REPO_DIR" submodule update --init "$submodule_path"
					};;
					n|no) {
						return
					};;
					*) {
						echo "Unknown answer. Not updating submodules."
					};;
				esac
			fi
		done
	} 3< <(git config --file "${REPO_DIR}/.gitmodules" --null --get-regexp path | xargs -0 printf "%s\n" | sed 's/^submodule.//;s/.path$//')
}

if [ -t 0 ]; then
	update_submodules
fi

# Always remove the local cache to avoid any confusion
bat cache --clear

# TODO:
# - Remove the JavaDoc patch once https://github.com/trishume/syntect/issues/222 has been fixed
# - Remove the C# patch once https://github.com/sublimehq/Packages/pull/2331 has been merged

# Apply patches
(
    cd "$ASSET_DIR"
    for patch in patches/*.patch; do
        patch --strip=0 < "$patch"
    done
)

reverse_patches() {
    (
        cd "$ASSET_DIR"
        for patch in patches/*.patch; do
            patch --strip=0 --reverse <"$patch"
        done
    )
}

# Make sure to always reverse patches, even if the `bat cache` command fails or aborts
trap reverse_patches EXIT

bat cache --build --blank --acknowledgements --source="$ASSET_DIR" --target="$ASSET_DIR"

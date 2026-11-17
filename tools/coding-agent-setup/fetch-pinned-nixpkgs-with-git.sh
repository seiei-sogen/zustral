#!/usr/bin/env bash
# Fetches the nixpkgs tag that shell.nix pins, with git, into the place where shell.nix looks
# for it: ~/.cache/zustral/nixpkgs-<tag>. After it, `nix-shell --command '...'` works where
# the tarball of nixpkgs cannot be downloaded (the Claude Code cloud environment, issue #42).
#
# Safe to run again: it does nothing when the checkout is already there.

set -euo pipefail
cd "$(dirname "$0")/../.."

tag=$(sed -n 's/^ *nixpkgsTag = "\(.*\)";$/\1/p' shell.nix)
[[ -n $tag ]] || { echo "shell.nix does not name a nixpkgsTag" >&2; exit 1; }

checkout="$HOME/.cache/zustral/nixpkgs-$tag"
if [[ -f $checkout/default.nix ]]; then
  echo "already there: $checkout"
  exit 0
fi

rm -rf "$checkout.partial"
mkdir -p "$(dirname "$checkout")"
git -c advice.detachedHead=false clone --quiet --depth 1 --branch "$tag" https://github.com/NixOS/nixpkgs.git "$checkout.partial"
mv "$checkout.partial" "$checkout"
echo "fetched: $checkout"

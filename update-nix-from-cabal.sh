#!/usr/bin/env bash
# Regenerates default.nix files from .cabal files.
# Run this after changing any .cabal file.
#
# Why: IHP uses pre-generated nix files instead of callCabal2nix to avoid
# Import From Derivation (IFD). IFD causes nix to build cabal2nix during
# evaluation, making derivation hashes platform-dependent and breaking
# caching across machines.
# See: https://github.com/NixOS/nixpkgs/issues/36190

set -euo pipefail
cd "$(dirname "$0")"

for cabal_file in */*.cabal; do
  dir=$(dirname "$cabal_file")
  echo "Generating $dir/default.nix from $cabal_file"
  # Run cabal2nix from within the package directory so that src = ./. points
  # to the package root (not a subdirectory). The overlay overrides src anyway,
  # but this keeps the generated file correct if used standalone.
  (cd "$dir" && cabal2nix .) > "./$dir/default.nix"
done

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

# --- Local IHP packages ---
for cabal_file in */*.cabal; do
  dir=$(dirname "$cabal_file")
  echo "Generating $dir/default.nix from $cabal_file"
  # Run cabal2nix from within the package directory so that src = ./. points
  # to the package root (not a subdirectory). The overlay overrides src anyway,
  # but this keeps the generated file correct if used standalone.
  (cd "$dir" && cabal2nix .) > "./$dir/default.nix"
done

# --- ihp-zip: only third-party Haskell package that still lives in
# NixSupport/hackage/. Everything else (hasql, postgresql-types,
# wai-session-*, etc.) now comes from the nixpkgs fork at
# digitallyinduced/nixpkgs:ihp-nixpkgs and is wired up via rebinds in
# NixSupport/overlay.nix — nothing to regenerate there.
mkdir -p NixSupport/hackage

hackage_packages=(
  "ihp-zip 0.1.0"
)

for entry in "${hackage_packages[@]}"; do
  name="${entry%% *}"
  ver="${entry##* }"
  echo "Generating NixSupport/hackage/${name}.nix from Hackage: ${name}-${ver}"
  cabal2nix "cabal://${name}-${ver}" > "NixSupport/hackage/${name}.nix"
done

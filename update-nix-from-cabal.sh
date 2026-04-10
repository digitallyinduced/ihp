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

# --- Third-party Hackage packages ---
# Pre-generated to avoid IFD (Import From Derivation).
# Update versions here when upgrading dependencies.
mkdir -p NixSupport/hackage

hackage_packages=(
  "wai-session-maybe 1.0.0"
  "wai-session-clientsession-deferred 1.0.0"
  "postgresql-binary 0.15.0.1"
  "postgresql-connection-string 0.1.0.6"
  "hasql 1.10.2.3"
  "hasql-pool 1.4.2"
  "hasql-dynamic-statements 0.5.1"
  "hasql-transaction 1.2.2"
  "hasql-notifications 0.2.5.0"
  "temporary-ospath 1.3"
  "postgresql-simple-postgresql-types 0.1.1"
  "postgresql-types-algebra 0.1"
  "postgresql-types 0.1.2"
  "hasql-mapping 0.1"
  "hasql-postgresql-types 0.2"
  "ihp-zip 0.1.0"
)

for entry in "${hackage_packages[@]}"; do
  name="${entry%% *}"
  ver="${entry##* }"
  echo "Generating NixSupport/hackage/${name}.nix from Hackage: ${name}-${ver}"
  cabal2nix "cabal://${name}-${ver}" > "NixSupport/hackage/${name}.nix"
done

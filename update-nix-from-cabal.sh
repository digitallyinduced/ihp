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

# --- Hasql 2 and adapters ---
# Sources are locked flake inputs; generated derivations take the source as an
# argument so evaluation requires neither cabal2nix nor machine-specific paths.
mkdir -p NixSupport/hasql
hasql_packages=(
  hasql hasql-pool hasql-transaction hasql-dynamic-statements
  hasql-implicits hasql-notifications pqi pqi-native pqi-ffi
)
for name in "${hasql_packages[@]}"; do
  package_source=$(nix eval --raw --impure --expr \
    "(builtins.getFlake (toString ./.)).inputs.\"${name}-source\".outPath")
  echo "Generating NixSupport/hasql/${name}.nix from its locked source"
  cabal2nix --extra-arguments source "$package_source" \
    | sed 's@^  src = .*;@  src = source;@' > "NixSupport/hasql/${name}.nix"
done

# --- Third-party Hackage packages ---
# Pre-generated to avoid IFD (Import From Derivation).
# Update versions here when upgrading dependencies.
mkdir -p NixSupport/hackage

hackage_packages=(
  "wai-session-maybe 1.0.0"
  "wai-session-clientsession-deferred 1.0.0"
  "postgresql-connection-string 0.1.0.6"
  "postgresql-syntax 0.5.0.3"
  "temporary-ospath 1.3"
  "ptr-poker 0.1.3"
  "postgresql-simple-postgresql-types 0.1.1"
  "postgresql-types-algebra 0.1"
  "postgresql-types 0.1.2"
  "hasql-mapping 0.1"
  "hasql-postgresql-types 0.2"
  "ihp-zip 0.1.2"
  "ghc-tcplugin-api 0.19.0.0"
  "ghc-typelits-natnormalise 0.9.6"
  "ghc-typelits-knownnat 0.8.4"
)
# postgresql-binary and the compatible hasql-mapping release continue to
# resolve from nixpkgs. The Hasql 2 family is generated from locked sources above.

for entry in "${hackage_packages[@]}"; do
  name="${entry%% *}"
  ver="${entry##* }"
  echo "Generating NixSupport/hackage/${name}.nix from Hackage: ${name}-${ver}"
  cabal2nix "cabal://${name}-${ver}" > "NixSupport/hackage/${name}.nix"
done

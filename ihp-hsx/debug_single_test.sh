#!/bin/bash
cd /mnt/h/bac_à_guigui_v2/ihp/ihp-hsx
echo "Running specific test to see exact format..."

# Run only one test to see the exact error message format
nix-shell -p 'haskellPackages.ghcWithPackages (pkgs: with pkgs; [megaparsec text template-haskell hspec case-insensitive hashable scientific parser-combinators])' --run 'cd /mnt/h/bac_à_guigui_v2/ihp/ihp-hsx && cabal test --test-show-details=direct --test-options="--match \"should fail on invalid html tags\""'

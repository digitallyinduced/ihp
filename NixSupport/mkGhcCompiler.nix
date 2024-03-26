{ pkgs
, ihp
, ghcCompiler
, dontCheckPackages ? []
, doJailbreakPackages ? []
, dontHaddockPackages ? []
, manualOverrides ? _: _: { }
, haskellPackagesDir ? ./haskell-packages
, filter
, ... }:

let
  composeExtensionsList = pkgs.lib.fold pkgs.lib.composeExtensions (_: _: { });

  generatedOverrides = haskellPackagesNew: haskellPackagesOld:
    let
      toPackage = dir: file: _: {
        name = builtins.replaceStrings [ ".nix" ] [ "" ] file;

        value = haskellPackagesNew.callPackage ("${dir}/${file}") { };
      };
      makePackageSet = dir: pkgs.lib.mapAttrs' (toPackage dir) (builtins.readDir dir);
    in {
      "ihp" = ((haskellPackagesNew.callPackage "${toString ihp}/ihp.nix") { inherit filter; });
    } // (makePackageSet haskellPackagesDir) // (makePackageSet "${ihp}/NixSupport/haskell-packages/.");

  makeOverrides =
    function: names: haskellPackagesNew: haskellPackagesOld:
    let
      toPackage = name: {
        inherit name;

        value = function haskellPackagesOld.${name};
      };
    in
    builtins.listToAttrs (map toPackage names);

  ihpDontCheckPackages = [
    "pcre-heavy" # https://github.com/NixOS/nixpkgs/pull/299118
  ];
  ihpDoJailbreakPackages = [
    "inflections" # Can be removed after https://github.com/NixOS/nixpkgs/pull/298337 is merged
    "string-random" # https://github.com/NixOS/nixpkgs/pull/299122
    "stripe-concepts" # https://github.com/NixOS/nixpkgs/pull/299129
    "stripe-signature" # https://github.com/NixOS/nixpkgs/pull/299129
  ];
  ihpDontHaddockPackages = [];
in ghcCompiler.override {
  overrides = composeExtensionsList [
    generatedOverrides

    # Overrides provided by IHP
    (makeOverrides pkgs.haskell.lib.dontCheck ihpDontCheckPackages)
    (makeOverrides pkgs.haskell.lib.doJailbreak ihpDoJailbreakPackages)
    (makeOverrides pkgs.haskell.lib.dontHaddock ihpDontHaddockPackages)

    # Project specific overrides
    (makeOverrides pkgs.haskell.lib.dontCheck   dontCheckPackages )
    (makeOverrides pkgs.haskell.lib.doJailbreak doJailbreakPackages)
    (makeOverrides pkgs.haskell.lib.dontHaddock dontHaddockPackages)
    manualOverrides

    (self: super: { websockets = super.websockets_0_13_0_0; })
  ];
}

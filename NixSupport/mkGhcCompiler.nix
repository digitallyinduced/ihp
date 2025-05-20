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

  ihpDontCheckPackages = [];
  ihpDoJailbreakPackages = [];
  ihpDontHaddockPackages = [];
in ghcCompiler.override {
  overrides = composeExtensionsList [
    generatedOverrides

    # Overrides provided by IHP
    (makeOverrides pkgs.haskell.lib.dontCheck ihpDontCheckPackages)
    (makeOverrides pkgs.haskell.lib.doJailbreak ihpDoJailbreakPackages)
    (makeOverrides pkgs.haskell.lib.dontHaddock ihpDontHaddockPackages)

    (self: super: {
      fsnotify = super.callHackageDirect { pkg = "fsnotify"; ver = "0.4.3.0"; sha256 = "sha256-6SJ8w2p0HNhMPjdQzxx4oqsyI48/C/K7wh+kLNy9/fM="; } {};
    })

    # Project specific overrides
    (makeOverrides pkgs.haskell.lib.dontCheck   dontCheckPackages )
    (makeOverrides pkgs.haskell.lib.doJailbreak doJailbreakPackages)
    (makeOverrides pkgs.haskell.lib.dontHaddock dontHaddockPackages)
    manualOverrides
  ];
}

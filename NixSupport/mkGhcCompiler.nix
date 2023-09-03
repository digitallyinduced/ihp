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
    in (makePackageSet haskellPackagesDir) // (makePackageSet "${ihp}/NixSupport/haskell-packages/.");

  makeOverrides =
    function: names: haskellPackagesNew: haskellPackagesOld:
    let
      toPackage = name: {
        inherit name;

        value = function haskellPackagesOld.${name};
      };
    in
    builtins.listToAttrs (map toPackage names);

  ihpDontCheckPackages = [ "mmark" "mmark-ext" "mutable-containers" "hiedb" "hls-fourmolu-plugin" "alex"];
  ihpDoJailbreakPackages = [ "haskell-to-elm" "ip" "ghc-syntax-highlighter" "relude" "hs-brotli" "tuples" "singletons-th" "singletons-base" "inflections" "postgresql-simple" "with-utf8" "chell" "typerep-map" "microlens-th" "dlist" "commutative-semigroups" "indexed-traversable" "parallel" "postgresql-libpq" "ghc-tcplugins-extra" "ghc-trace-events" "inspection-testing" "hie-compat" "ghc-byteorder" "primitive" "some" "splitmix" "tagged" "tar" "th-abstraction" "type-equality" "unliftio-core" "assoc" "ghc-lib-parser" "boring"];
  ihpDontHaddockPackages = [ ];
in ghcCompiler.override {
  overrides = composeExtensionsList [
    manualOverrides

    # Overrides provided by IHP
    (makeOverrides pkgs.haskell.lib.dontCheck ihpDontCheckPackages)
    (makeOverrides pkgs.haskell.lib.doJailbreak ihpDoJailbreakPackages)
    (makeOverrides pkgs.haskell.lib.dontHaddock ihpDontHaddockPackages)



    # Project specific overrides
    (makeOverrides pkgs.haskell.lib.dontCheck   dontCheckPackages )
    (makeOverrides pkgs.haskell.lib.doJailbreak doJailbreakPackages)
    (makeOverrides pkgs.haskell.lib.dontHaddock dontHaddockPackages)
    

    generatedOverrides

    (haskellPackagesNew: haskellPackagesOld: {
      "ihp" = ((haskellPackagesOld.callPackage "${toString ihp}/ihp.nix") { inherit filter; });
    })

    # (self: super: { haskell-language-server = pkgs.haskell.lib.appendConfigureFlag super.haskell-language-server "--enable-executable-dynamic"; })
    (self: super: { ormolu = if pkgs.system == "aarch64-darwin" then pkgs.haskell.lib.overrideCabal super.ormolu (_: { enableSeparateBinOutput = false; }) else super.ormolu; })
  ];
}

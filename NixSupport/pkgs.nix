let
  dontCheckPackages = [
    "ghc-mod"
    "cabal-helper"
    "generic-lens"
    "filesystem-conduit"
    "tz"
    "typerep-map"
  ];

  doJailbreakPackages = [
    "ghc-mod"
    "filesystem-conduit"
    "turbohaskell"
  ];

  dontHaddockPackages = [];

  nixPkgsRev = "b6ef10f6a81d1653b27bd8cff0593b55a7081106";
  nixPkgsSha256 = "1dvqrhp0c85n7n9n79gv3hkyw6i32gwhpqgdyxkaqzf13ckmdrjm";

  compiler = "ghc865";

  generatedOverrides = haskellPackagesNew: haskellPackagesOld:
    let
      toPackage = dir: file: _: {
        name = builtins.replaceStrings [ ".nix" ] [ "" ] file;

        value = haskellPackagesNew.callPackage ("${dir}/${file}") {};
      };
      makePackageSet = dir: pkgs.lib.mapAttrs' (toPackage dir) (builtins.readDir dir);
    in
      { "turbohaskell" = ((haskellPackagesNew.callPackage ./../../TurboHaskell/turbohaskell.nix) { }); } // (makePackageSet ./haskell-packages/.) // (makePackageSet ./../../TurboHaskell/NixSupport/haskell-packages/.);

  makeOverrides =
    function: names: haskellPackagesNew: haskellPackagesOld:
      let
        toPackage = name: {
          inherit name;

          value = function haskellPackagesOld.${name};
        };
      in
      builtins.listToAttrs (map toPackage names);

  composeExtensionsList = pkgs.lib.fold pkgs.lib.composeExtensions (_: _: {});

  # More exotic overrides go here
  manualOverrides = haskellPackagesNew: haskellPackagesOld: {
    turbohaskell = pkgs.haskell.lib.doJailbreak (pkgs.haskell.lib.allowInconsistentDependencies haskellPackagesOld.turbohaskell);
    time_1_9_3 = pkgs.haskell.lib.dontCheck haskellPackagesOld.time_1_9_3;
  };

  #mkDerivation = args: super.mkDerivation (args // {
  #    enableLibraryProfiling = true;
  #});
  config = {
    allowBroken = true;
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" =
          pkgs.haskell.packages."${compiler}".override {
            overrides = composeExtensionsList [
              generatedOverrides
              (makeOverrides pkgs.haskell.lib.dontCheck   dontCheckPackages  )
              (makeOverrides pkgs.haskell.lib.doJailbreak doJailbreakPackages)
              (makeOverrides pkgs.haskell.lib.dontHaddock dontHaddockPackages)
              manualOverrides
            ];
          };
        }
        ;
      }
      ;
    };
  };


  pkgs = (import ((import <nixpkgs> {}).fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = nixPkgsRev;
    sha256 = nixPkgsSha256;
  })) { inherit config; };

in
pkgs

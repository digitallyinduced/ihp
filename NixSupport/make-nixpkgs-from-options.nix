# This function is imported from the apps Config/nix/nixpkgs-config.nix
# It takes a few parameter as an input and returns a nixpkgs version used for building
# the project
{ ihp
, haskellPackagesDir
, dontCheckPackages ? []
, doJailbreakPackages ? []
, dontHaddockPackages ? []
, nixPkgsRev ? "c7f75838c360473805afcf5fb2fa65e678efd94b"
, nixPkgsSha256 ? "04vx1j2gybm1693a8wxw6bpcsd4h1jdw541vwic8nfm3n80r4ckm"
, compiler ? "ghc8103"
, manualOverrides ? haskellPackagesNew: haskellPackagesOld: { } # More exotic overrides go here
}:
let
  generatedOverrides = haskellPackagesNew: haskellPackagesOld:
    let
      toPackage = dir: file: _: {
        name = builtins.replaceStrings [ ".nix" ] [ "" ] file;

        value = haskellPackagesNew.callPackage ("${dir}/${file}") {};
      };
      makePackageSet = dir: pkgs.lib.mapAttrs' (toPackage dir) (builtins.readDir dir);
    in
      { "ihp" = ((haskellPackagesNew.callPackage "${ihp}/ihp.nix") { }); } // (makePackageSet haskellPackagesDir) // (makePackageSet "${ihp}/NixSupport/haskell-packages/.");

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

  ihpDontCheckPackages = [];
  ihpDoJailbreakPackages = [];
  ihpDontHaddockPackages = [];

  config = {
    allowBroken = true;
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" =
          pkgs.haskell.packages."${compiler}".override {
            overrides = composeExtensionsList [
              generatedOverrides
              
              # Overrides provided by IHP
              (makeOverrides pkgs.haskell.lib.dontCheck   ihpDontCheckPackages  )
              (makeOverrides pkgs.haskell.lib.doJailbreak ihpDoJailbreakPackages)
              (makeOverrides pkgs.haskell.lib.dontHaddock ihpDontHaddockPackages)

              # Project specific overrides
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

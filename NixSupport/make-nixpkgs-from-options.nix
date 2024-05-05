# This function is imported from the apps Config/nix/nixpkgs-config.nix
# It takes a few parameter as an input and returns a nixpkgs version used for building
# the project
{ ihp
, haskellPackagesDir
, dontCheckPackages ? []
, doJailbreakPackages ? []
, dontHaddockPackages ? []
, nixPkgsRev ? "54b4bb956f9891b872904abdb632cea85a033ff2"
, nixPkgsSha256 ? "sha256-Wx3Y6arRJD1pd3c8SnD7dfW7KWuCr/r248P/5XLaMdM"
, compiler ? "ghc98"
, manualOverrides ? haskellPackagesNew: haskellPackagesOld: { } # More exotic overrides go here
, additionalNixpkgsOptions ? {}
}:
let
  composeExtensionsList = pkgs.lib.fold pkgs.lib.composeExtensions (_: _: {});
  mkGhcCompiler = import ./mkGhcCompiler.nix;

  config = {
    allowBroken = true;
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = mkGhcCompiler {
            inherit pkgs ihp dontCheckPackages doJailbreakPackages dontHaddockPackages manualOverrides haskellPackagesDir;

            ghcCompiler = pkgs.haskell.packages."${compiler}";
            filter = (import ((import <nixpkgs> {}).fetchFromGitHub {
              owner = "numtide";
              repo = "nix-filter";
              rev = "d6381c442f79f2f1fdfde00521c3d15d6c21218e";
              sha256 = "sha256-ciSZqliNLDattmjR/1yQ0mJHZSkXWZlnIR6TcdDGFMa";
            }));
          };
        };
      };
    };
  };

  pkgs = (import ((import <nixpkgs> {}).fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = nixPkgsRev;
    sha256 = nixPkgsSha256;
  })) ({ inherit config; } // additionalNixpkgsOptions);

in
    pkgs

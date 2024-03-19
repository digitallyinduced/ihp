# This function is imported from the apps Config/nix/nixpkgs-config.nix
# It takes a few parameter as an input and returns a nixpkgs version used for building
# the project
{ ihp
, haskellPackagesDir
, dontCheckPackages ? []
, doJailbreakPackages ? []
, dontHaddockPackages ? []
, nixPkgsRev ? "10c80b993425f377c86c2bb36da9f10b79de6fe2"
, nixPkgsSha256 ? "sha256-yWxZq0v5Hj4bxtYsy/Pzh9g6N8nuSHCCwHpu6QNOX9E"
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

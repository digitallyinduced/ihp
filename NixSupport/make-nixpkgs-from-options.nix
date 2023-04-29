# This function is imported from the apps Config/nix/nixpkgs-config.nix
# It takes a few parameter as an input and returns a nixpkgs version used for building
# the project
{ ihp
, haskellPackagesDir
, dontCheckPackages ? []
, doJailbreakPackages ? []
, dontHaddockPackages ? []
, nixPkgsRev ? "a95ed9fe764c3ba2bf2d2fa223012c379cd6b32e"
, nixPkgsSha256 ? "sha256-eKyxW4OohHQx9Urxi7TQlFBTDWII+F+x2hklDOQPB51="
, compiler ? "ghc944"
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
            inherit pkgs ihp dontCheckPackages doJailbreakPackages dontHaddockPackages manualOverrides;

            ghcCompiler = pkgs.haskell.packages."${compiler}";
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

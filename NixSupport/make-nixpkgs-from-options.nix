# This function is imported from the apps Config/nix/nixpkgs-config.nix
# It takes a few parameter as an input and returns a nixpkgs version used for building
# the project
{ ihp
, haskellPackagesDir
, dontCheckPackages ? ["mmark" "mmark-ext"]
, doJailbreakPackages ? ["haskell-to-elm" "mmark-cli"]
, dontHaddockPackages ? []
, nixPkgsRev ? "5efc8ca954272c4376ac929f4c5ffefcc20551d5"
, nixPkgsSha256 ? "sha256-pHTwvnN4tTsEKkWlXQ8JMY423epos8wUOhthpwJjtpc="
, compiler ? "ghc921"
, manualOverrides ? haskellPackagesNew: haskellPackagesOld: { } # More exotic overrides go here
, additionalNixpkgsOptions ? {}
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
      { "ihp" = ((haskellPackagesNew.callPackage "${toString ihp}/ihp.nix") { }); } // (makePackageSet haskellPackagesDir) // (makePackageSet "${toString ihp}/NixSupport/haskell-packages/.");

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

              (self: super: { haskell-language-server = pkgs.haskell.lib.appendConfigureFlag super.haskell-language-server "--enable-executable-dynamic"; })
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
  })) ({ inherit config; } // additionalNixpkgsOptions);

in
    pkgs

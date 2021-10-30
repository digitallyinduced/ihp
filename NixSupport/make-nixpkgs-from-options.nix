# This function is imported from the apps Config/nix/nixpkgs-config.nix
# It takes a few parameter as an input and returns a nixpkgs version used for building
# the project
{ ihp
, haskellPackagesDir
, dontCheckPackages ? ["mmark" "mmark-ext" "cabal-doctest" "blaze-markup" "mutable-containers" "mime-mail" "network-byte-order" "pointed" "ghc-lib" "ghc-api-compat"]
, doJailbreakPackages ? ["blaze-markup" "cabal-doctest" "postgresql-libpq" "protolude" "postgresql-simple" "vault" "websockets" "unliftio" "iproute" "blaze-markup" "czipwith" "vector-instances" "ghc-api-compat" "ghc-trace-events"]
, dontHaddockPackages ? []
, nixPkgsRev ? "dbfe42bc77c7d782beb2609e5219809eae2f7fb9"
, nixPkgsSha256 ? "0im78bhf512q1dr0ha9jy6zczqawscv6kmnb0iis20wl61skl66c"
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

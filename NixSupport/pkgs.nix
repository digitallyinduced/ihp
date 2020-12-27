let
    dontCheckPackages = [
        "cabal-helper"
        "generic-lens"
        "filesystem-conduit"
        "tz"
        "typerep-map"
        "trifecta"
        "hackage-security"
    ];

    doJailbreakPackages = [
        "filesystem-conduit"
        "http-media"
        "aeson"
        "wreq"
        "ghcide"
        "brittany"
    ];

    dontHaddockPackages = [];

    nixPkgsRev = "c7f75838c360473805afcf5fb2fa65e678efd94b";
    nixPkgsSha256 = "04vx1j2gybm1693a8wxw6bpcsd4h1jdw541vwic8nfm3n80r4ckm";

    compiler = "ghc8103";

    generatedOverrides = haskellPackagesNew: haskellPackagesOld:
        let
            toPackage = dir: file: _: {
                name  = builtins.replaceStrings [ ".nix" ] [ "" ] file;
                value = haskellPackagesNew.callPackage ("${dir}/${file}") { };
            };
            makePackageSet = dir: pkgs.lib.mapAttrs' (toPackage dir) (builtins.readDir dir);
        in
            (makePackageSet ./haskell-packages/.);

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

    manualOverrides = haskellPackagesNew: haskellPackagesOld: {
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
                };
            };
        };
    };


    pkgs = (import ((import <nixpkgs> { }).fetchFromGitHub {
        owner = "NixOS";
        repo = "nixpkgs";
        rev = nixPkgsRev;
        sha256 = nixPkgsSha256;
    })) { inherit config; };

in pkgs
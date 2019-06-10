let
    dontCheckPackages = [
      "ghc-mod"
      "cabal-helper"
      "generic-lens"
      "filesystem-conduit"
      "tz"
      "http2-client"
      "push-notify-apn"
    ];

    doJailbreakPackages = [
      "ghc-mod"
      "filesystem-conduit"
      "push-notify-apn"
      "diframework"
    ];

    dontHaddockPackages = [
    ];

    nixPkgsRev = "dfd8f84aef129f1978e446b5d45ef05cd4421821";
    nixPkgsSha256 = "0av0q7xyv76jq2csbg10x8gcnlnadlppvlx616s7qz7jahkmymrl";

    compiler = "ghc844";

          generatedOverrides = haskellPackagesNew: haskellPackagesOld:
            let
              toPackage = dir: file: _: {
                name  = builtins.replaceStrings [ ".nix" ] [ "" ] file;

                value = haskellPackagesNew.callPackage ("${dir}/${file}") { };
              };
              makePackageSet = dir: pkgs.lib.mapAttrs' (toPackage dir) (builtins.readDir dir);
            in
              (makePackageSet ./haskell-packages/.) // (makePackageSet ./../../src/Foundation/NixSupport/haskell-packages/.);

          makeOverrides =
            function: names: haskellPackagesNew: haskellPackagesOld:
              let
                toPackage = name: {
                  inherit name;

                  value = function haskellPackagesOld.${name};
                };

            in
              builtins.listToAttrs (map toPackage names);

          composeExtensionsList =
            pkgs.lib.fold pkgs.lib.composeExtensions (_: _: {});

          # More exotic overrides go here
          manualOverrides = haskellPackagesNew: haskellPackagesOld: {
            diframework =  pkgs.haskell.lib.doJailbreak (pkgs.haskell.lib.allowInconsistentDependencies haskellPackagesOld.diframework);
            time_1_9_2 = pkgs.haskell.lib.dontCheck haskellPackagesOld.time_1_9_2;
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
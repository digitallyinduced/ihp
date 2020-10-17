let
    dontCheckPackages = [
        "cabal-helper"
        "filesystem-conduit"
        "tz"
        "http2-client"
        "typerep-map"
        "hslogger"
    ];

    doJailbreakPackages = [
        "filesystem-conduit"
        "ihp"
        "http-media"
        "countable-inflections"
        "haskell-language-server"
        "hslogger"
    ];

    dontHaddockPackages = [
    ];

    nixPkgsRev = "c985bf793e6ab7d54a9182381b4b610fe0ae6936";
    nixPkgsSha256 = "0zsj9imjbnhkb65r169xxqmjgqd5593insrvncvabg1iqdsrcxz1";

    compiler = "ghc883";

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

    # More exotic overrides go here
    manualOverrides = haskellPackagesNew: haskellPackagesOld: {
      ihp = pkgs.haskell.lib.doJailbreak (pkgs.haskell.lib.allowInconsistentDependencies haskellPackagesOld.ihp);
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
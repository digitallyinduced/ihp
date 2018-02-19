{ compiler, haskellDeps ? (p: []) }:

let
    config = {
        packageOverrides = super: let self = super.pkgs; pkgs = super.pkgs; in
            {
                haskell = super.haskell // {
                    packages = super.haskell.packages // {
                        ${compiler} = super.haskell.packages.${compiler}.override {
                            overrides = self: super: {
                                ghc-mod = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.doJailbreak (self.callPackage ./ghc-mod.nix { }));
                                cabal-helper = pkgs.haskell.lib.dontCheck (self.callPackage ./cabal-helper.nix { });
                            };
                        };
                    };
                };
            };
        };

    pkgs = import ((import <nixpkgs> { }).fetchFromGitHub {
      owner = "NixOS";
      repo = "nixpkgs";
      rev = "017154391b4a2a4e418640d3a7677274fe981dc9";
      sha256 = "0anjjsv4js286l4m3ysz2qpqv85db0d2fih4wy9pc5z53ds1qf9g";
    }) { config = config; };

    ghc = pkgs.haskell.packages.${compiler};
    allHaskellPackages = ghc.ghcWithPackages (p: builtins.concatLists [(haskellDeps p) [pkgs.haskell.lib.doJailbreak p.ghc-mod]] );
    allNativePackages = [pkgs.darwin.apple_sdk.frameworks.Cocoa pkgs.postgresql];
in
    pkgs.stdenv.mkDerivation {
        name = "app";
        src = ./../../../.;
        buildInputs = builtins.concatLists [allNativePackages [allHaskellPackages]];
        shellHook = "eval $(egrep ^export ${allHaskellPackages}/bin/ghc)";
    }


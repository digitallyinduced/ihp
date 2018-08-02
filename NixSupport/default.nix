{ compiler, haskellDeps ? (p: []) }:

let
    config = {
        packageOverrides = super: let self = super.pkgs; pkgs = super.pkgs; in
            {
                haskell = super.haskell // {
                    packages = super.haskell.packages // {
                        ${compiler} = super.haskell.packages.${compiler}.override {
                            overrides = self: super: {
                                mkDerivation = args: super.mkDerivation (args // {
                                    enableLibraryProfiling = true;
                                });
                                ghc-mod = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.doJailbreak (self.callPackage ./ghc-mod.nix { }));
                                cabal-helper = pkgs.haskell.lib.dontCheck (self.callPackage ./cabal-helper.nix { });
                                generic-lens = pkgs.haskell.lib.dontCheck (self.callPackage ./generic-lens.nix { });
                                time_1_9_1 = pkgs.haskell.lib.dontCheck super.time_1_9_1;
                            };
                        };
                    };
                };
            };
        };

    pkgs = import ((import <nixpkgs> { }).fetchFromGitHub {
      owner = "NixOS";
      repo = "nixpkgs";
      rev = "d6c6c7fcec6dbd2b8ab14f0b35d56c7733872baa";
      sha256 = "1bq59575b58lmh5w8bh7pwp17c3p2nm651bz1i3q224c4zsj9294";
    }) { config = config; };

    ghc = pkgs.haskell.packages.${compiler};
    allHaskellPackages = ghc.ghcWithPackages (p: builtins.concatLists [(haskellDeps p) [pkgs.haskell.lib.doJailbreak p.ghc-mod p.hlint p.http-media]] );
    allNativePackages = builtins.concatLists [ [pkgs.postgresql] (if pkgs.stdenv.isDarwin then [pkgs.darwin.apple_sdk.frameworks.Cocoa pkgs.llvm_39] else [pkgs.llvm_39]) ];
in
    pkgs.stdenv.mkDerivation {
        name = "app";
        src = ./../../../.;
        buildInputs = builtins.concatLists [allNativePackages [allHaskellPackages]];
        shellHook = "eval $(egrep ^export ${allHaskellPackages}/bin/ghc)";
    }

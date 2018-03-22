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
      rev = "592fb2c5a16da017fb5cb0d56214c9b3939a2c31";
      sha256 = "0gbmzj8mywkhbxv2fmy801qvnj9m68rx7nq2n1md5vgzvyc75vq5";
    }) { config = config; };

    ghc = pkgs.haskell.packages.${compiler};
    allHaskellPackages = ghc.ghcWithPackages (p: builtins.concatLists [(haskellDeps p) [pkgs.haskell.lib.doJailbreak p.ghc-mod]] );
    allNativePackages = builtins.concatLists [ [pkgs.postgresql] (if pkgs.stdenv.isDarwin then [pkgs.darwin.apple_sdk.frameworks.Cocoa] else [pkgs.llvm_39]) ];
in
    pkgs.stdenv.mkDerivation {
        name = "app";
        src = ./../../../.;
        buildInputs = builtins.concatLists [allNativePackages [allHaskellPackages]];
        shellHook = "eval $(egrep ^export ${allHaskellPackages}/bin/ghc)";
    }


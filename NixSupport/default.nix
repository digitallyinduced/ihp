{ compiler, haskellDeps ? (p: []), otherDeps ? (p: []) }:

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
                                filesystem-conduit = pkgs.haskell.lib.doJailbreak (pkgs.haskell.lib.dontCheck (self.callPackage ./filesystem-conduit.nix { }));
                                hs-pkpass = pkgs.haskell.lib.doJailbreak (pkgs.haskell.lib.dontCheck (self.callPackage ./hs-pkpass.nix { }));
                                time_1_9_1 = pkgs.haskell.lib.dontCheck super.time_1_9_1;
                                tz = pkgs.haskell.lib.dontCheck super.tz;
                                http2-client = pkgs.haskell.lib.dontCheck super.http2-client;
                                push-notify-apn = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.doJailbreak (self.callPackage ./push-notify-apn.nix { }));
                            };
                        };
                    };
                };
            };
        };

    pkgs = import ((import <nixpkgs> { }).fetchFromGitHub {
      owner = "NixOS";
      repo = "nixpkgs";
      rev = "d592f291ed2959a819e91c3da1f6d3bed62d4ff5";
      sha256 = "1273vm8jwyrhnfcxcdzl404pbwm0njq8jl7d0kncixxkibap6nsx";
    }) { config = config; };

    ghc = pkgs.haskell.packages.${compiler};
    allHaskellPackages = ghc.ghcWithPackages (p: builtins.concatLists [(haskellDeps p) [pkgs.haskell.lib.doJailbreak p.ghc-mod p.hlint p.http-media]] );
    allNativePackages = builtins.concatLists [ (otherDeps pkgs) [pkgs.postgresql] (if pkgs.stdenv.isDarwin then [pkgs.darwin.apple_sdk.frameworks.Cocoa pkgs.llvm_39] else [pkgs.llvm_39]) ];
in
    pkgs.stdenv.mkDerivation {
        name = "app";
        src = ./../../../.;
        buildInputs = builtins.concatLists [allNativePackages [allHaskellPackages]];
        shellHook = "eval $(egrep ^export ${allHaskellPackages}/bin/ghc)";
    }

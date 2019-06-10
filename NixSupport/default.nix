{ compiler, haskellDeps ? (p: []), otherDeps ? (p: []), projectPath ? ./. }:

let
    pkgs = import "${projectPath}/Config/nix/nixpkgs-config.nix";

    ghc = pkgs.haskell.packages.${compiler};
    allHaskellPackages = ghc.ghcWithPackages (p: haskellDeps p);
    allNativePackages = builtins.concatLists [ (otherDeps pkgs) [pkgs.postgresql] (if pkgs.stdenv.isDarwin then [pkgs.darwin.apple_sdk.frameworks.Cocoa pkgs.llvm_39] else [pkgs.llvm_39]) ];
in
    pkgs.stdenv.mkDerivation {
        name = "app";
        src = projectPath;
        buildInputs = builtins.concatLists [[allHaskellPackages] allNativePackages];
        shellHook = "eval $(egrep ^export ${allHaskellPackages}/bin/ghc)";
    }

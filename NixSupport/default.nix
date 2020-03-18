{ compiler, haskellDeps ? (p: []), otherDeps ? (p: []), projectPath ? ./. }:

let
    pkgs = import "${projectPath}/Config/nix/nixpkgs-config.nix";

    gitignore = pkgs.callPackage (pkgs.fetchFromGitHub {
        owner = "siers";
        repo = "nix-gitignore";
        rev = "d69e4f620ec9d35ddb125ee495f6a5dee519c905";
        sha256 = "0x55qgirpkzm0yagyqqxi8l7yc3g20bx42iayz124n09cz7sp7mp";
    }) {};

    ghc = pkgs.haskell.packages.${compiler};
    allHaskellPackages = ghc.ghcWithPackages (p: haskellDeps p);
    allNativePackages = builtins.concatLists [ (otherDeps pkgs) [pkgs.postgresql] (if pkgs.stdenv.isDarwin then [pkgs.darwin.apple_sdk.frameworks.Cocoa] else [pkgs.llvm_39]) ];
in
    pkgs.stdenv.mkDerivation {
        name = "app";
        src = gitignore.gitignoreSource [] projectPath;
        buildInputs = builtins.concatLists [[allHaskellPackages] allNativePackages];
        shellHook = "eval $(egrep ^export ${allHaskellPackages}/bin/ghc)";
    }

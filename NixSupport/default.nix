{ compiler ? "ghc883", ihp, haskellDeps ? (p: []), otherDeps ? (p: []), projectPath ? ./. }:

let
    pkgs = import "${toString projectPath}/Config/nix/nixpkgs-config.nix" { ihp = ihp; };
    ghc = pkgs.haskell.packages.${compiler};
    allHaskellPackages = ghc.ghcWithPackages (p: builtins.concatLists [ [p.haskell-language-server] (haskellDeps p) ] );
    allNativePackages = builtins.concatLists [ (otherDeps pkgs) [pkgs.postgresql] (if pkgs.stdenv.isDarwin then [] else []) ];
in
    pkgs.stdenv.mkDerivation {
        name = "app";
        buildPhase = ''
          make -B build/bin/RunUnoptimizedProdServer
        '';
        installPhase = ''
          mkdir -p $out
          cp -r build/bin $out/bin
        '';
        dontFixup = true;
        src = (import <nixpkgs> {}).nix-gitignore.gitignoreSource [] projectPath;
        buildInputs = builtins.concatLists [[allHaskellPackages] allNativePackages];
        shellHook = "eval $(egrep ^export ${allHaskellPackages}/bin/ghc)";
    }

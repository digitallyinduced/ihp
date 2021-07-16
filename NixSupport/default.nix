{ compiler ? "ghc8103", ihp, haskellDeps ? (p: []), otherDeps ? (p: []), projectPath ? ./., withHoogle ? false }:

let
    pkgs = import "${toString projectPath}/Config/nix/nixpkgs-config.nix" { ihp = ihp; };
    ghc = pkgs.haskell.packages.${compiler};
    allHaskellPackages =
      (if withHoogle
      then ghc.ghcWithHoogle
      else ghc.ghcWithPackages)
        (p: builtins.concatLists [ [p.haskell-language-server] (haskellDeps p) ] );
    allNativePackages = builtins.concatLists [ (otherDeps pkgs) [pkgs.postgresql] (if pkgs.stdenv.isDarwin then [] else []) ];
in
    pkgs.stdenv.mkDerivation {
        name = "app";
        buildPhase = ''
          mkdir -p build
          rm -f build/ihp-lib

          mkdir -p IHP
          ln -s "${ihp}/lib/IHP" build/ihp-lib
          ln -s "${ihp}/lib" IHP/lib # Avoid the Makefile calling 'which RunDevServer'

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

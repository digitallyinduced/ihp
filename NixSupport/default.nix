{ compiler ? "ghc921"
, ihp
, haskellDeps ? (p: [])
, otherDeps ? (p: [])
, projectPath ? ./.
, withHoogle ? false
, additionalNixpkgsOptions ? {}
}:

let
    pkgs = import "${toString projectPath}/Config/nix/nixpkgs-config.nix" { ihp = ihp; additionalNixpkgsOptions = additionalNixpkgsOptions; };
    ghc = pkgs.haskell.packages.${compiler};
    allHaskellPackages =
      (if withHoogle
      then ghc.ghcWithHoogle
      else ghc.ghcWithPackages)
        (p: builtins.concatLists [ [p.haskell-language-server] (haskellDeps p) ] );
    allNativePackages = builtins.concatLists [ (otherDeps pkgs) [pkgs.postgresql pkgs.makeWrapper] (if pkgs.stdenv.isDarwin then [] else []) ];
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
          mkdir -p "$out"
          mkdir -p $out/bin

          mv build/bin/RunUnoptimizedProdServer $out/bin/RunUnoptimizedProdServer
          makeWrapper $out/bin/RunUnoptimizedProdServer $out/bin/RunProdServer --prefix PATH : ${pkgs.lib.makeBinPath (otherDeps pkgs)}

          mkdir -p "$out/lib/build"
          cp -R "${ihp}/lib" "$out/lib/build/ihp-lib"
          mv static "$out/lib/static"
        '';
        dontFixup = true;
        src = (import <nixpkgs> {}).nix-gitignore.gitignoreSource [] projectPath;
        buildInputs = builtins.concatLists [[allHaskellPackages] allNativePackages];
        shellHook = "eval $(egrep ^export ${allHaskellPackages}/bin/ghc)";
    }

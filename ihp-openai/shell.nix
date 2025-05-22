let
    pkgs = (import ./../NixSupport/make-nixpkgs-from-options.nix) {
        ihp = ./../.;
        haskellPackagesDir = ./../NixSupport/haskell-packages;
    };
    ghc = pkgs.haskell.packages.ghc98;
    haskellDeps = ghc.ghcWithPackages (p: with p; [
        text
        bytestring
        http-streams
        retry

        # Development Specific Tools
        cabal-install
    ]);
in
    pkgs.stdenv.mkDerivation {
        name = "ihp-hsx";
        src = ./.;
        buildInputs = [haskellDeps];
        shellHook = "eval $(egrep ^export ${haskellDeps}/bin/ghc)";
    }

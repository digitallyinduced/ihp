let
    pkgs = (import ./../NixSupport/make-nixpkgs-from-options.nix) {
        ihp = ./../.;
        haskellPackagesDir = ./../NixSupport/haskell-packages;
    };
    ghc = pkgs.haskell.packages.ghc8107;
    haskellDeps = ghc.ghcWithPackages (p: with p; [
        classy-prelude
        string-conversions
        blaze-html
        blaze-markup
        text
        bytestring
        basic-prelude
        megaparsec
        template-haskell
        haskell-src-meta
        containers

        # Development Specific Tools
        hspec
        cabal-install
    ]);
in
    pkgs.stdenv.mkDerivation {
        name = "ihp-hsx";
        src = ./.;
        buildInputs = [haskellDeps];
        shellHook = "eval $(egrep ^export ${haskellDeps}/bin/ghc)";
    }

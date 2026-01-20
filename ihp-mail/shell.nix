let
    pkgs = (import ./../NixSupport/make-nixpkgs-from-options.nix) {
        ihp = ./../.;
        haskellPackagesDir = ./../NixSupport/haskell-packages;
    };
    ghc = pkgs.haskellPackages;
    haskellDeps = ghc.ghcWithPackages (p: with p; [
        classy-prelude
        string-conversions
        blaze-html
        text
        bytestring
        basic-prelude
        mime-mail
        mime-mail-ses
        smtp-mail
        http-client
        http-client-tls
        network
        typerep-map

        # Development Specific Tools
        hspec
        cabal-install
    ]);
in
    pkgs.stdenv.mkDerivation {
        name = "ihp-mail";
        src = ./.;
        buildInputs = [haskellDeps];
        shellHook = "eval $(egrep ^export ${haskellDeps}/bin/ghc)";
    }

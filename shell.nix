let
    pkgs = (import ./NixSupport/make-nixpkgs-from-options.nix) {
        ihp = ./.;
        haskellPackagesDir = ./NixSupport/haskell-packages;
    };
    ghc = pkgs.haskell.packages.ghc8107;
    haskellDeps = ghc.ghcWithPackages (p: with p; [
        # Copied from ihp.nix
        base
        classy-prelude
        directory
        string-conversions
        warp
        wai
        mtl
        blaze-html
        blaze-markup
        wai-extra
        http-types
        inflections
        text
        postgresql-simple
        wai-middleware-static
        wai-util
        aeson
        uuid
        wai-session
        wai-session-clientsession
        clientsession
        pwstore-fast
        template-haskell
        haskell-src-meta
        random-strings
        interpolate
        websockets
        wai-websockets
        mime-mail
        mime-mail-ses
        smtp-mail
        attoparsec
        case-insensitive
        http-media
        cookie
        process
        unix
        fsnotify
        countable-inflections
        typerep-map
        basic-prelude
        data-default
        regex-tdfa
        resource-pool
        wreq
        deepseq
        uri-encode
        parser-combinators
        ip
        fast-logger
        minio-hs
        temporary
        wai-cors
        random
        async-pool
        cereal-text
        neat-interpolation
        unagi-chan

        # Development Specific Tools (not in ihp.nix)
        mmark-cli
        hspec
    ]);
in
    pkgs.stdenv.mkDerivation {
        name = "ihp-dev";
        src = ./.;
        buildInputs = [haskellDeps pkgs.entr pkgs.nodejs];
        shellHook = "eval $(egrep ^export ${haskellDeps}/bin/ghc)";
    }

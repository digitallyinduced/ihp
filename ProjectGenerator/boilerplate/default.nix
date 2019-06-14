let
    haskellEnv = import ./src/TurboHaskell/NixSupport/default.nix {
        compiler = "ghc844";
        haskellDeps = p: with p; [
            cabal-install
            base
            classy-prelude
            directory
            free
            string-conversions
            wai
            mtl
            blaze-html
            blaze-markup
            wai
            mtl
            text
            postgresql-simple
            wai-util
            aeson
            uuid
            hlint
            parsec
            template-haskell
            interpolate
            uri-encode
            generic-lens
            tz
            turbohaskell
        ];
        otherDeps = p: with p; [
            imagemagick
        ];
        projectPath = ./.;
    };
in
    haskellEnv
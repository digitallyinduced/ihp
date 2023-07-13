/*
flake-parts module for setting the local IHP devenv shell
this is different from the devenv environment used by IHP apps!
that is defined in flake-module.nix
*/

{
    perSystem = { pkgs, lib, ... }: {
        devenv.shells.default = {
            packages = with pkgs; [ entr nodejs-18_x ];
            containers = lib.mkForce {};  # https://github.com/cachix/devenv/issues/528

            languages.haskell.enable = true;
            languages.haskell.package =
                let
                    ghcCompiler = import ./NixSupport/mkGhcCompiler.nix {
                        inherit pkgs;
                        ghcCompiler = pkgs.haskell.packages.ghc944;
                        ihp = ./.;
                    };
                in
                    ghcCompiler.ghc.withPackages (p: with p; [
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
                        wai-app-static
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
                        cereal-text
                        neat-interpolation
                        unagi-chan
                        with-utf8

                        # Development Specific Tools (not in ihp.nix)
                        mmark-cli
                        hspec
                        ihp-hsx
                        ihp-postgresql-simple-extra
                    ]);

            scripts.tests.exec = ''
                runghc $(make -f lib/IHP/Makefile.dist print-ghc-extensions) -iIDE Test/Main.hs
            '';

            scripts.fastbuild.exec = ''
                cabal build --flag FastBuild
            '';

            scripts.build-ihp-new.exec = ''
                cd ProjectGenerator
                make tarball.tar.gz
            '';

            scripts.build-guide.exec = ''
                cd Guide
                export LANG=en_US.UTF-8
                make guide.tar.gz
            '';

            scripts.build-api-reference.exec = ''
                chmod +x build-haddock
                ./build-haddock
            '';
        };
    };
}

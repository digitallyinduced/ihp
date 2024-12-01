/*
flake-parts module for setting the local IHP devenv shell
this is different from the devenv environment used by IHP apps!
that is defined in flake-module.nix
*/
{ inputs }:
{
    perSystem = { nix-filter, pkgs, lib, ... }: let
        ghcCompiler = import ./NixSupport/mkGhcCompiler.nix {
            inherit pkgs;
            ghcCompiler = pkgs.haskell.packages.ghc983;
            ihp = ./.;
            filter = inputs.nix-filter.lib;
        };
    in {

        apps.migrate = {
            type = "app";
            program = "${ghcCompiler.ihp-migrate}/bin/migrate";
        };

        devenv.shells.default = {
            packages = with pkgs; [];
            containers = lib.mkForce {};  # https://github.com/cachix/devenv/issues/528

            languages.haskell.enable = true;
            languages.haskell.package =
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
                        with-utf8_1_1_0_0

                        # Development Specific Tools (not in ihp.nix)
                        hspec
                        ihp-hsx
                        ihp-postgresql-simple-extra
                    ]);

            scripts.tests.exec = ''
                runghc $(make -f lib/IHP/Makefile.dist print-ghc-extensions) -iihp-ide -iihp-ssc Test/Main.hs
            '';

            scripts.fastbuild.exec = ''
                cabal build --flag FastBuild
            '';

            scripts.build-ihp-new.exec = ''
                cd ProjectGenerator
                make tarball.tar.gz
            '';

            scripts.build-api-reference.exec = ''
                chmod +x build-haddock
                ./build-haddock
            '';

            languages.haskell.stack = null; # Stack is not used in IHP
            languages.haskell.languageServer = ghcCompiler.haskell-language-server;
        };

        packages = {
            default = ghcCompiler.ihp;
            ide = ghcCompiler.ihp-ide;
            ssc = ghcCompiler.ihp-ssc;
            migrate = ghcCompiler.ihp-migrate;
            datasync-typescript = ghcCompiler.ihp-datasync-typescript;
        };
    };
}

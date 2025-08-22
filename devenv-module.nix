/*
flake-parts module for setting the local IHP devenv shell
this is different from the devenv environment used by IHP apps!
that is defined in flake-module.nix
*/
{ self, inputs }:
{
    perSystem = { system, nix-filter, pkgs, lib, ... }:
    {
        _module.args.pkgs = import inputs.nixpkgs { inherit system; overlays = [ self.overlays.default ]; config = { }; };

        apps.migrate = {
            type = "app";
            program = "${pkgs.ghc.ihp-migrate}/bin/migrate";
        };

        devenv.shells.default = {
            packages = with pkgs; [];
            containers = lib.mkForce {};  # https://github.com/cachix/devenv/issues/528

            languages.haskell.enable = true;
            languages.haskell.package =
                    pkgs.ghc.ghc.withPackages (p: with p; [
                        # Copied from ihp.nix
                        base
                        classy-prelude
                        directory
                        string-conversions
                        warp
                        warp-systemd
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
                        hspec
                        ihp-hsx
                        ihp-postgresql-simple-extra


                        http-streams
                        HsOpenSSL
                        http-streams
                        io-streams
                        network-uri
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

            languages.haskell.stack.enable = false; # Stack is not used in IHP
            languages.haskell.languageServer = pkgs.ghc.haskell-language-server;
        };

        packages = {
            default = pkgs.ghc.ihp;
            ide = pkgs.ghc.ihp-ide;
            ssc = pkgs.ghc.ihp-ssc;
            migrate = pkgs.ghc.ihp-migrate;
            datasync-typescript = pkgs.ghc.ihp-datasync-typescript;
        };
    };
}

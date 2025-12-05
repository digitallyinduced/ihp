/*
flake-parts module for setting the local IHP devenv shell
this is different from the devenv environment used by IHP apps!
that is defined in flake-module.nix
*/
{ self, inputs }:
{
    perSystem = { config, system, nix-filter, pkgs, lib, ... }:
    let
                    hsDataDir = package:
                            let
                                ghcName   = package.passthru.compiler.haskellCompilerName;         # e.g. "ghc-9.8.4"
                                shareRoot = "${package.data}/share/${ghcName}";
                                # Pick the only dir ending with "-${ghcName}", e.g. "aarch64-osx-ghc-9.8.4"
                                sys = lib.head (lib.filter (n: lib.hasSuffix "-${ghcName}" n) (builtins.attrNames (builtins.readDir shareRoot)));
                            in
                                "${shareRoot}/${sys}/${package.name}";
    in
    {
        _module.args.pkgs = import inputs.nixpkgs { inherit system; overlays = [ self.overlays.default ]; config = { }; };

        apps.migrate = {
            type = "app";
            program = "${pkgs.ghc.ihp-migrate}/bin/migrate";
            meta = {
                description = "Apply migrations to the database";
            };
        };

        # Use `nix flake check --impure` to run tests and check that all ihp packages are building succesfully
        checks = {
            # Checks devShells
            ihp-devShell = self.devShells.${system}.default;
            boilerplate-devShell = inputs.ihp-boilerplate.devShells.${system}.default;
        }
            # Add all package outputs to the checks
            // (lib.filterAttrs (n: v: lib.isDerivation v && n != "default") self.packages.${system})
            
            # Add all ihp-boilerplate packages to the checks
            // (lib.mapAttrs' (n: v: { name = "boilerplate-${n}"; value = v; }) (lib.filterAttrs (n: v: lib.isDerivation v
                        && n != "default"
                        && n != "unoptimized-docker-image" && n != "optimized-docker-image" # Docker imagee builds are very slow, so we ignore them
                    ) inputs.ihp-boilerplate.packages.${system}))
        ;

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

            scripts.fastbuild.exec = ''
                cabal build --flag FastBuild
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
            ihp-new = pkgs.callPackage ./ihp-new/default.nix {};
            ihp-sitemap = pkgs.ghc.ihp-sitemap;
            ihp-datasync = pkgs.ghc.ihp-datasync;
            ihp-job-dashboard = pkgs.ghc.ihp-job-dashboard;
            wai-asset-path = pkgs.ghc.wai-asset-path;
            wai-flash-messages = pkgs.ghc.wai-flash-messages;
            ihp-imagemagick = pkgs.ghc.ihp-imagemagick;
            ihp-hspec = pkgs.ghc.ihp-hspec;
            
            run-script = pkgs.stdenv.mkDerivation {
                pname = "run-script";
                version = "1.0.0";
                src = ./ihp-ide/exe/IHP/CLI/run-script;
                dontUnpack = true;
                installPhase = ''
                    mkdir -p $out/bin
                    cp $src $out/bin/run-script
                    chmod +x $out/bin/run-script
                '';
            };

            guide =
                let
                    node-modules = pkgs.mkYarnModules {
                        pname = "guide-node_modules";
                        packageJSON = ./Guide/package.json;
                        yarnLock = ./Guide/yarn.lock;
                        version = "1.0.0";
                    };
                in
                    pkgs.stdenv.mkDerivation {
                        name = "ihp-guide";
                        src = ./Guide;
                        nativeBuildInputs = with pkgs; [ haskellPackages.mmark-cli pkgs.esbuild ];
                        buildPhase = ''
                            # build HTML from all *.markdown using the template
                            for f in *.markdown; do
                                out_html="''${f%.markdown}.html"
                                mmark -i "$f" -o "$out_html" \
                                    --template layout.html \
                                    --ext-skylighting --ext-ghc-highlighter \
                                    --ext-punctuation --ext-toc 2-6
                            done

                            cp ${node-modules}/node_modules/bootstrap/dist/css/bootstrap.min.css bootstrap.css
                            cp ${node-modules}/node_modules/instantclick/dist/instantclick.min.js instantclick.js

                            # build search.js via esbuild
                            NODE_PATH=${node-modules}/node_modules \
                            ${pkgs.esbuild}/bin/esbuild search.jsx \
                                --bundle \
                                --preserve-symlinks \
                                --minify-whitespace \
                                --define:process.env.NODE_ENV=\"production\" \
                                --minify \
                                --legal-comments=none \
                                --outfile=search.js
                        '';
                        installPhase = ''
                            mkdir -p $out
                            cp -r *.html *.css *.js images $out/
                        '';
                        LOCALE_ARCHIVE = pkgs.lib.optionalString pkgs.stdenv.isLinux "${pkgs.glibcLocales}/lib/locale/locale-archive";
                        LANG = "en_US.UTF-8";
                        allowedReferences = [];
                };


            reference =
                pkgs.stdenv.mkDerivation {
                    name = "ihp-reference";
                    src = self;
                    nativeBuildInputs = with pkgs; [ pkgs.ghc.ihp ];
                    buildPhase = ''
                        cp -r ${pkgs.ghc.ihp.doc}/share/doc/ihp-*/html haddock-build
                        chmod -R u+w haddock-build

                        cd haddock-build

                        # Add favicon
                        find . -type f \( -iname "*.html" \) -exec sed -i 's#<head>#<head><link rel="shortcut icon" type="image\/x-icon" href="https:\/\/ihp.digitallyinduced.com\/ihp-logo.svg"\/>#g' '{}' +

                        # Add ihp-haddock.css
                        cp ../ihp-haddock.css ihp-haddock.css
                        find . -type f \( -iname "*.html" \) -exec sed -i 's#<\/head>#<link href="ihp-haddock.css" rel="stylesheet"/><\/head>#g' '{}' +

                        # Link title to index
                        find . -type f \( -iname "*.html" \) -exec sed -i 's#<span class=\"caption\">IHP Api Reference</span>#<a href=\"index.html\" class=\"caption\"><img src=\"https://ihp.digitallyinduced.com/Guide/images/ihp-logo-readme.svg\"/>IHP Api Reference</a>#g' '{}' +

                        cp -R . $out
                    '';
                    # allowedReferences = [];
            };

            datasync-js = pkgs.mkYarnPackage {
                name = "datasync-js";
                src = let filter = inputs.nix-filter.lib; in filter {
                    root = "${self}/ihp-datasync/data/DataSync";
                };
                postConfigure = ''
                    yarn run test
                    yarn run typecheck
                '';
            };

            # The IHP boilerplate's Makefile depends on an IHP env var that contains
            # the Makefile.dist (ihp-ide) and several JS and CSS files (ihp)
            ihp-env-var-backwards-compat =
                pkgs.symlinkJoin {
                    name = "ihp-env-var-backwards-compat";
                    paths = [
                        (hsDataDir pkgs.ghc.ihp-ide.data + "/lib/IHP")
                        (hsDataDir pkgs.ghc.ihp-ide.data)
                        (hsDataDir pkgs.ghc.ihp.data)
                    ];
                };
        };
    };
}

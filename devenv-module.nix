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
                                ghcName   = package.passthru.compiler.haskellCompilerName;         # e.g. "ghc-9.10.1"
                                shareRoot = "${package.data}/share/${ghcName}";
                                # Pick the first (typically only) platform-specific directory, filtering out "doc"
                                dirs = builtins.filter (d: d != "doc") (builtins.attrNames (builtins.readDir shareRoot));
                                sys = lib.head dirs;
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
            # Required for devenv v1.11+ to fix flake check
            process.manager.implementation = "process-compose";
            process.managers.process-compose.enable = true;

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

                        # Packages needed for ghci to load IHP modules
                        slugger
                        megaparsec
                        cryptohash
                        pwstore-fast
                        vault

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

            ihp-static =
                let
                    jquery = version: hash: pkgs.fetchurl {
                        url = "https://code.jquery.com/jquery-${version}";
                        sha256 = hash;
                    };
                    bootstrapBase = version: "https://cdn.jsdelivr.net/npm/bootstrap@${version}/dist";
                    bootstrapCss = version: hash: pkgs.fetchurl {
                        url = "${bootstrapBase version}/css/bootstrap.min.css";
                        sha256 = hash;
                    };
                    bootstrapJs = version: hash: pkgs.fetchurl {
                        url = "${bootstrapBase version}/js/bootstrap.min.js";
                        sha256 = hash;
                    };
                    bootstrap538css = bootstrapCss "5.3.8" "1pnyvfp2n8qzyp167h9rvg93msmcf09hsl6hnpwy2gkskkcjflyq";
                    bootstrap538js  = bootstrapJs  "5.3.8" "01r295gwq51aq6kb0znj7yymaz6lry4h914kivc7y939bn4i83vv";
                    bootstrap521css = bootstrapCss "5.2.1" "1l7sy8l7vbbck4mrbvwpq2ssk1hmk2h0qa4gpz5ygsm491iwjcr9";
                    bootstrap521js  = bootstrapJs  "5.2.1" "0cx8khby1jg3xcmjbqas79zdsfi7jmvjs00ypi4d140ycch9z1wh";
                    bootstrap45cssmap = pkgs.fetchurl {
                        url = "https://cdn.jsdelivr.net/npm/bootstrap@4.5.0/dist/css/bootstrap.min.css.map";
                        sha256 = "17jzy6x2shrj805r0czhsz0psjxm5vlv70ipybk8rqz4k7lbcm6j";
                    };
                    bootstrap45jsmap = pkgs.fetchurl {
                        url = "https://cdn.jsdelivr.net/npm/bootstrap@4.5.0/dist/js/bootstrap.min.js.map";
                        sha256 = "1fagbzf8qmf338r7qzap55xbj84nsrn91ad4pbq39hwrmi21fb5j";
                    };
                    # Select2 — pinned to develop branch (includes jQuery 4 support, PR #6332)
                    select2commit = "595494a72fee67b0a61c64701cbb72e3121f97b9";
                    select2js = pkgs.fetchurl {
                        url = "https://raw.githubusercontent.com/select2/select2/${select2commit}/dist/js/select2.min.js";
                        sha256 = "1y0j8qwdzkhcp1kn04fbr4hl89x87wi1lkhnirpd5gkc2sb06764";
                    };
                    select2css = pkgs.fetchurl {
                        url = "https://raw.githubusercontent.com/select2/select2/${select2commit}/dist/css/select2.min.css";
                        sha256 = "1fi1qdiybi30adg8vcqqa6n0jz7c658kv0z39bkvm2ama0i4g2s2";
                    };
                in
                    pkgs.symlinkJoin {
                        name = "ihp-static";
                        paths = [
                            (hsDataDir pkgs.ghc.ihp.data + "/static")
                            (pkgs.linkFarm "ihp-vendor-js" [
                                # jQuery — current version
                                { name = "vendor/jquery-4.0.0.min.js"; path = jquery "4.0.0.min.js" "1amdfbjdqncpv9x00n8f8xg43nvaapwfiq6kypx8nzyrkbm4d99r"; }
                                { name = "vendor/jquery-4.0.0.slim.min.js"; path = jquery "4.0.0.slim.min.js" "01x39qb1rbdz6n2y5j7yd3i420f6x9aw6miki2wny8n7bnzsjcgh"; }
                                # jQuery — backwards compatibility
                                { name = "vendor/jquery-3.7.1.min.js"; path = jquery "3.7.1.min.js" "06hb7y19azzim1k53d1gw78fq6whw7s1qj7hpxf08sqz4kfr76pw"; }
                                { name = "vendor/jquery-3.7.1.slim.min.js"; path = jquery "3.7.1.slim.min.js" "1ks0qcs51imwgxf88j1g89isdcznxzc50iv5wjb90fky82ryyqcj"; }
                                { name = "vendor/jquery-3.6.0.min.js"; path = jquery "3.6.0.min.js" "0vpylcvvq148xv92k4z2yns3nya80qk1kfjsqs29qlw9fgxj65gz"; }
                                { name = "vendor/jquery-3.6.0.slim.min.js"; path = jquery "3.6.0.slim.min.js" "04p56k5isbhhiv8j25jxqhynchw4qxixnvisfm41kdm23j9bkdxv"; }
                                { name = "vendor/jquery-3.5.0.min.js"; path = jquery "3.5.0.min.js" "1965r7pswaffbrj42iwyr7ar922gx4yjfgy7w1w41di5mvcwvp64"; }
                                { name = "vendor/jquery-3.2.1.slim.min.js"; path = jquery "3.2.1.slim.min.js" "16739f77k16kxyf3ngi6841j07wmjc7qm8jbvjik66xihw494rck"; }

                                # Bootstrap 5.3.8 — versioned path for apps
                                { name = "vendor/bootstrap-5.3.8/bootstrap.min.css"; path = bootstrap538css; }
                                { name = "vendor/bootstrap-5.3.8/bootstrap.min.js";  path = bootstrap538js; }
                                # Bootstrap 5.3.8 — generic path (used by IDE ToolServer)
                                { name = "vendor/bootstrap.min.css"; path = bootstrap538css; }
                                { name = "vendor/bootstrap.min.js";  path = bootstrap538js; }
                                # Bootstrap 5.2.1 — backwards compatibility
                                { name = "vendor/bootstrap-5.2.1/bootstrap.min.css"; path = bootstrap521css; }
                                { name = "vendor/bootstrap-5.2.1/bootstrap.min.js";  path = bootstrap521js; }
                                # Bootstrap 4.5 — backwards compatibility (source maps)
                                { name = "vendor/bootstrap.min.css.map"; path = bootstrap45cssmap; }
                                { name = "vendor/bootstrap.min.js.map";  path = bootstrap45jsmap; }

                                # Select2 (develop branch with jQuery 4 support)
                                { name = "vendor/select2.min.js";  path = select2js; }
                                { name = "vendor/select2.min.css"; path = select2css; }
                            ])
                        ];
                    };
            schema-compiler = pkgs.ghc.ihp-schema-compiler;
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
            ihp-welcome = pkgs.ghc.ihp-welcome;
            ihp-mail = pkgs.ghc.ihp-mail;
            
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

            # ihp-pro
            stripe = pkgs.ghc.ihp-stripe;
            sentry = pkgs.ghc.ihp-sentry;
            oauth-github = pkgs.ghc.ihp-oauth-github;
            oauth-google = pkgs.ghc.ihp-oauth-google;

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
                    nativeBuildInputs = with pkgs; [ pkgs.ghc.ihp-with-docs ];
                    buildPhase = ''
                        cp -r ${pkgs.ghc.ihp-with-docs.doc}/share/doc/ihp-*/html haddock-build
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

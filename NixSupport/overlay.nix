{ self, inputs, forceLocal ? false }:
let
    flakeRoot = self;

    ihpOverrides = final: self: super:
        let
            filter = inputs.nix-filter.lib;
            # Disable profiling for faster local builds
            fastBuild = pkg: final.haskell.lib.disableLibraryProfiling pkg;

            filteredSrc = name: filter {
                root = "${toString flakeRoot}/${name}";
                include = [ (filter.matchExt "hs") (filter.matchExt "cabal") (filter.matchExt "csv") (filter.matchExt "md") filter.isDirectory "LICENSE" "data" ];
            };

            # Uses pre-generated default.nix files to avoid IFD (Import From Derivation).
            # IFD causes nix to build cabal2nix during evaluation, making derivation
            # hashes platform-dependent and breaking caching across machines.
            # To regenerate: run ./update-nix-from-cabal.sh after changing .cabal files
            # or upgrading third-party Hackage dependency versions.
            localPackage = name: fastBuild (
                final.haskell.lib.overrideSrc
                    (super.callPackage "${flakeRoot}/${name}/default.nix" {})
                    { src = filteredSrc name; }
            );

            # Pre-generated nix files for third-party Hackage packages, avoiding IFD.
            # To regenerate: run ./update-nix-from-cabal.sh after changing versions.
            # Note: unlike localPackage, these keep profiling enabled since downstream
            # nixpkgs packages (e.g. jsonifier) may require profiling libraries.
            hackagePackage = name: self.callPackage "${flakeRoot}/NixSupport/hackage/${name}.nix" {};

            # Use the nixpkgs version if available (i.e. published on Hackage and
            # picked up by the nixpkgs all-cabal-hashes snapshot), otherwise fall
            # back to building from the local source tree.  Pass --arg forceLocal true
            # to always use the local version (useful during development).
            hackageOrLocal = name:
                if forceLocal || !(super ? ${name})
                then localPackage name
                else fastBuild super.${name};

            # For quick testing during development, you can use callCabal2nix directly
            # (slower eval due to IFD, but no generated files needed):
            #   localPackageIFD = name: fastBuild (super.callCabal2nix name (filteredSrc name) {});

            # ihp-with-docs has haddock for reference docs
            localPackageWithHaddock = name:
                final.haskell.lib.disableLibraryProfiling (
                    final.haskell.lib.overrideSrc
                        (super.callPackage "${flakeRoot}/${name}/default.nix" {})
                        { src = filteredSrc name; }
                );
        in {
            ihp = localPackage "ihp";
            ihp-with-docs = localPackageWithHaddock "ihp";
            ihp-router = localPackage "ihp-router";
            ihp-pagehead = localPackage "ihp-pagehead";
            ihp-pglistener = localPackage "ihp-pglistener";
            ihp-modal = localPackage "ihp-modal";
            ihp-ide = localPackage "ihp-ide";
            ihp-schema-compiler = localPackage "ihp-schema-compiler";
            ihp-postgres-parser = localPackage "ihp-postgres-parser";
            ihp-mail = localPackage "ihp-mail";
            ihp-migrate = (localPackage "ihp-migrate").overrideAttrs (old: { mainProgram = "migrate"; });
            ihp-openai = localPackage "ihp-openai";
            ihp-ssc = localPackage "ihp-ssc";
            ihp-zip = fastBuild (hackagePackage "ihp-zip");
            ihp-hsx = localPackage "ihp-hsx";
            ihp-graphql = localPackage "ihp-graphql";
            ihp-datasync-typescript = localPackage "ihp-datasync-typescript";
            ihp-sitemap = localPackage "ihp-sitemap";
            ihp-typed-sql = localPackage "ihp-typed-sql";
            ihp-datasync = localPackage "ihp-datasync";
            ihp-job-dashboard = localPackage "ihp-job-dashboard";
            wai-asset-path = localPackage "wai-asset-path";
            wai-flash-messages = localPackage "wai-flash-messages";
            wai-request-params = localPackage "wai-request-params";
            wai-early-return = localPackage "wai-early-return";
            ihp-imagemagick = localPackage "ihp-imagemagick";
            ihp-hspec = localPackage "ihp-hspec";
            ihp-welcome = localPackage "ihp-welcome";
            ihp-log = localPackage "ihp-log";

            # Forks of wai-session / wai-session-clientsession with deferred
            # session decryption and optional Set-Cookie (Maybe ByteString).
            # https://hackage.haskell.org/package/wai-session-maybe
            # https://hackage.haskell.org/package/wai-session-clientsession-deferred
            wai-session-maybe = hackagePackage "wai-session-maybe";
            wai-session-clientsession-deferred = hackagePackage "wai-session-clientsession-deferred";

            # HsOpenSSL 0.11.7.10 fails to compile against openssl 3.6.1 on Linux
            # because the C compiler escalates `-Wpointer-sign` to an error (the
            # OpenSSL 3.6.1 headers tightened up `char*` vs `unsigned char*`).
            # nixpkgs already passes `-Wno-error=incompatible-pointer-types`; we
            # extend that with `-Wno-error=pointer-sign` until upstream HsOpenSSL
            # / nixpkgs covers it.
            HsOpenSSL = final.haskell.lib.appendConfigureFlags super.HsOpenSSL [
                "--ghc-option=-optc=-Wno-error=pointer-sign"
            ];

            # countable-inflections: nixpkgs at the pinned rev already ships
            # 0.3.2 (in the cached haskellPackages.ihp closure), so we drop the
            # previous git-src override and consume `super.countable-inflections`
            # verbatim for a cache hit. Restore the override only if a reverted
            # nixpkgs pin no longer carries 0.3.2.

            # Hasql 1.10 ecosystem.
            #
            # These attrs mirror the upstream `ihpHasqlScope` from
            # NixOS/nixpkgs#519795 (configuration-common.nix) so that
            # derivations are bit-identical to nixpkgs' Hydra-built
            # `haskellPackages.ihp` closure and resolve straight from
            # cache.nixos.org. Keep in sync with configuration-common.nix.
            postgresql-connection-string = hackagePackage "postgresql-connection-string";

            hasql                    = super.hasql_1_10_3;
            hasql-pool               = super.hasql-pool_1_4_2;
            hasql-dynamic-statements = super.hasql-dynamic-statements_0_5_1;
            hasql-transaction        = super.hasql-transaction_1_2_2;
            hasql-notifications      = super.hasql-notifications_0_2_5_0;
            postgresql-binary        = super.postgresql-binary_0_15_0_1;
            # text-builder 1.0.0.5 is needed by postgresql-simple-postgresql-types
            text-builder             = super.text-builder_1_0_0_5;

            # hasql-interpolate: upstream 1.0.1.0 requires hasql <1.10; use fork with hasql 1.10 support
            # https://github.com/awkward-squad/hasql-interpolate/pull/27
            # Uses overrideCabal instead of callCabal2nix to avoid IFD and Hackage cabal revision fetch failures
            hasql-interpolate = final.haskell.lib.dontCheck (final.haskell.lib.doJailbreak (final.haskell.lib.overrideCabal super.hasql-interpolate (old: {
                src = builtins.fetchTarball {
                    url = "https://github.com/ChrisPenner/hasql-interpolate/archive/bb4666fdb7e0fef9f67702cb198e45d0a1de0ab9.tar.gz";
                    sha256 = "1v3i4n4szxpir28a4vlhd2a0sl04fxkiw9wlyxcvd3vbrd9s2b8c";
                };
                revision = null;
                editedCabalFile = null;
            })));

            # Fork of temporary using OsPath instead of FilePath
            temporary-ospath = hackagePackage "temporary-ospath";

            # postgresql-simple-postgresql-types and hasql-mapping are marked
            # broken in nixpkgs; unmark them to mirror upstream ihpHasqlScope's
            # `unmarkBroken` (configuration-nix.nix applies dontCheck for
            # postgresql-simple-postgresql-types).
            postgresql-simple-postgresql-types = final.haskell.lib.markUnbroken super.postgresql-simple-postgresql-types;
            hasql-mapping = final.haskell.lib.markUnbroken super.hasql-mapping;
        };
in
final: prev: {
    # Default: GHC 9.10 (binary-cached via nixpkgs haskellPackages)
    ghc = final.haskellPackages.override {
        overrides = ihpOverrides final;
    };

    # Experimental: GHC 9.12 (not yet binary-cached; builds from source)
    ghc912 = final.haskell.packages.ghc912.override {
        overrides = final.lib.composeManyExtensions [
            (ihpOverrides final)
            (self: super: {
                # say tests fail due to CRLF newline handling changes
                say = final.haskell.lib.dontCheck super.say;

                # text-icu tests fail due to newer ICU BlockCode enum range
                text-icu = final.haskell.lib.dontCheck super.text-icu;

                # cryptonite tests have a flaky failure (1 of 1548)
                cryptonite = final.haskell.lib.dontCheck super.cryptonite;
            })
        ];
    };

    # GHC 9.14 — opt-in for apps using the digitallyinduced binary cache.
    # To use: set `ihp.ghcCompiler = pkgs.ghc914;` in your flake-module config.
    ghc914 =
        if prev.haskell.packages ? ghc914
        then final.haskell.packages.ghc914.override {
            overrides = final.lib.composeManyExtensions [
                (ihpOverrides final)
                (self: super: {
                    say = final.haskell.lib.dontCheck super.say;
                    text-icu = final.haskell.lib.dontCheck super.text-icu;
                    cryptonite = final.haskell.lib.dontCheck super.cryptonite;

                    # relude doctests fail due to changed GHC error messages in 9.14
                    relude = final.haskell.lib.dontCheck super.relude;

                    # Upgrade ghc-tcplugin-api to 0.19 (supports GHC 9.14)
                    ghc-tcplugin-api = self.callCabal2nix "ghc-tcplugin-api"
                        (final.fetchzip {
                            url = "https://hackage.haskell.org/package/ghc-tcplugin-api-0.19.0.0/ghc-tcplugin-api-0.19.0.0.tar.gz";
                            sha256 = "sha256-2jm1Q2lmaG6vtRnxcvxf4U2gvQdVkDL0h8PWaTpDWJA=";
                        }) {};

                    # Upgrade ghc-typelits-natnormalise to 0.9.6 (supports GHC 9.14)
                    ghc-typelits-natnormalise = final.haskell.lib.dontCheck (self.callCabal2nix "ghc-typelits-natnormalise"
                        (final.fetchzip {
                            url = "https://hackage.haskell.org/package/ghc-typelits-natnormalise-0.9.6/ghc-typelits-natnormalise-0.9.6.tar.gz";
                            sha256 = "sha256-a1afS4iJrB9hVp3FK+fozbWVxIt75H/gO6Q+PeoV53k=";
                        }) {});

                    # Upgrade ghc-typelits-knownnat to 0.8.4 (supports GHC 9.14)
                    ghc-typelits-knownnat = final.haskell.lib.dontCheck (self.callCabal2nix "ghc-typelits-knownnat"
                        (final.fetchzip {
                            url = "https://hackage.haskell.org/package/ghc-typelits-knownnat-0.8.4/ghc-typelits-knownnat-0.8.4.tar.gz";
                            sha256 = "sha256-PyYMUvJ8/miqusNl7+xay8OJqtK1/uHNQEiLr1utieg=";
                        }) {});
                })
                # GHC 9.14 ships base-4.22, containers-0.8, template-haskell-2.24.
                # Many nixpkgs packages have tight upper bounds on these boot libraries.
                (let
                    jailbreak = names: self: super:
                        builtins.listToAttrs (map (name: {
                            inherit name;
                            value = final.haskell.lib.doJailbreak super.${name};
                        }) (builtins.filter (name: super ? ${name}) names));
                in jailbreak [
                    # cabal-install 3.16.1.0 / cabal-install-solver want Cabal &
                    # Cabal-syntax >=3.16.1.0, but GHC 9.14 ships the 3.16.0.0 boot
                    # libs (a patch-release skew) — drop the bound.
                    "cabal-install" "cabal-install-solver" "cabal-install-parsers"
                    "cabal-add"
                    # hlint -> extensions pins Cabal-syntax <3.15, so nixpkgs builds
                    # the Cabal-syntax_3_14_2_0 attr — which caps containers <0.8 /
                    # time <1.15 and fails on GHC 9.14's containers-0.8 / time-1.15.
                    # Jailbreaking lets that pinned version build on the new boot libs.
                    "Cabal-syntax_3_14_2_0"
                    "lucid" "lucid2" "clay" "tasty-hspec" "config-ini" "fsnotify"
                    "string-interpolate" "rebase" "rerebase" "with-utf8" "minio-hs"
                    "sandwich" "brick" "postgresql-simple" "hasql-dynamic-statements"
                    "hasql-implicits" "warp-systemd" "ghc-trace-events"
                    "algebraic-graphs" "hie-bios" "stan" "modern-uri"
                    "ghc-lib-parser" "ghc-lib-parser-ex" "ghc-syntax-highlighter"
                    "colourista" "extensions" "trial" "trial-optparse-applicative"
                    "trial-tomland" "tomland" "validation-selective" "slist"
                    "ihp-zip" "warp-systemd"
                ])
            ];
        }
        else throw "ghc914 is not available in this nixpkgs";
}

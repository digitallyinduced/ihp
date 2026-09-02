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
            wai-csp = localPackage "wai-csp";

            postgresql-syntax = (final.haskell.lib.doJailbreak
                (hackagePackage "postgresql-syntax")).overrideAttrs (old: {
                    patches = (old.patches or []) ++ [
                        ./patches/postgresql-syntax-quickcheck-2.16.patch
                    ];
                });

            # wai-session-maybe / wai-session-clientsession-deferred (deferred
            # session decryption + optional Set-Cookie) are shipped by the pinned
            # nixpkgs at 1.0.0, so we consume them from the default set verbatim
            # for a cache hit instead of the previous hackagePackage forks.

            # HsOpenSSL 0.11.7.10 fails to compile against openssl 3.6.1+ on Linux
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

            # Hasql 1.10 is the pinned nixpkgs default, including hasql 1.10.3.7
            # and hasql-pool 1.4.2.3 with the poisoned-connection fixes from
            # #2765. The surrounding ecosystem (including hasql-interpolate,
            # postgresql-binary, text-builder and postgresql-connection-string)
            # also needs no overrides.

            # temporary-ospath is shipped by the pinned nixpkgs at 1.3, so it
            # also resolves from the default set with no override needed.

            # postgresql-simple-postgresql-types and hasql-mapping are unbroken in
            # the pinned nixpkgs, so no markUnbroken overrides are needed.

            # The PostGIS-enabled postgresql-types source below still requires
            # postgresql-types-algebra <0.2. nixpkgs has moved to 0.2, so retain
            # the compatible 0.1 release until that source updates its bounds.
            postgresql-types-algebra = final.haskell.lib.doJailbreak
                (hackagePackage "postgresql-types-algebra");
            postgresql-simple-postgresql-types = final.haskell.lib.dontCheck
                (hackagePackage "postgresql-simple-postgresql-types");
            hasql-postgresql-types = final.haskell.lib.doJailbreak
                (hackagePackage "hasql-postgresql-types");

            # postgresql-types with PostGIS Geometry (merged in
            # nikita-volkov/postgresql-types#69). Pin to git master until a
            # Hackage release ships Geometry; cabal version is still 0.1.3.2.
            # dontCheck: tests need a live PostgreSQL server.
            postgresql-types = final.haskell.lib.overrideCabal
                (final.haskell.lib.addBuildDepend
                    (final.haskell.lib.dontCheck super.postgresql-types)
                    self.postgresql-types-algebra)
                (old: {
                    version = "0.1.3.2";
                    src = builtins.fetchTarball {
                        url = "https://github.com/nikita-volkov/postgresql-types/archive/d8b2fe0ff3ab5d6731eced13d4b8be1d54694259.tar.gz";
                        sha256 = "1j90y1z8qq8lvcam4h1k17zrirqqi80gshv91pmqsgaqjg099gp7";
                    };
                    sha256 = null;
                    revision = null;
                    editedCabalFile = null;
                });
        };
in
final: prev: {
    # nix-prefetch-darcs consumes the top-level darcs attribute directly. Keep it
    # on the same bounds-relaxed build as the default GHC package set.
    darcs = final.ghc.darcs;

    # Default: GHC 9.12 — the pinned nixpkgs `haskellPackages` compiler.
    # The dontCheck overrides below apply to that default build.
    ghc = final.haskellPackages.override {
        overrides = final.lib.composeManyExtensions [
            (ihpOverrides final)
            (self: super: {
                # say tests fail due to CRLF newline handling changes
                say = final.haskell.lib.dontCheck super.say;

                # text-icu tests fail due to newer ICU BlockCode enum range
                text-icu = final.haskell.lib.dontCheck super.text-icu;

                # cryptonite tests have a flaky failure (1 of 1548)
                cryptonite = final.haskell.lib.dontCheck super.cryptonite;

                # The GHC 9.12 RC package set builds HLS 2.14 against Cabal 3.16,
                # while its ormolu/fourmolu/stylish-haskell plugins still use
                # Cabal 3.14. These are isolated plugin dependencies, but Cabal's
                # multiple-version warning is fatal in the nixpkgs Haskell
                # builder unless explicitly allowed.
                haskell-language-server = final.haskell.lib.allowInconsistentDependencies
                    super.haskell-language-server;

                # darcs 2.18.5 caps http-client-tls <0.4 and tls <2.2, while
                # the RC3 package set provides newer compatible releases. A full
                # doJailbreak conflicts with nixpkgs' patched darcs.cabal, so only
                # relax these two bounds after the nixpkgs patches are applied.
                darcs = super.darcs.overrideAttrs (old: {
                    postPatch = (old.postPatch or "") + ''
                        substituteInPlace darcs.cabal \
                            --replace-fail "http-client-tls   >= 0.3.5 && < 0.4" "http-client-tls   >= 0.3.5" \
                            --replace-fail "tls               >= 2.0.6 && < 2.2" "tls               >= 2.0.6"
                    '';
                });
            })
        ];
    };

    # `ghc912` is an alias of the default `ghc` set: the pinned nixpkgs default
    # compiler is already GHC 9.12, so a separate set would be an exact duplicate.
    # The alias keeps `pkgs.ghc912.*` references working; drop it once they migrate
    # to `pkgs.ghc`.
    ghc912 = final.ghc;

    # GHC 9.14 — opt-in for apps using the digitallyinduced binary cache.
    # To use: set `ihp.ghcCompiler = pkgs.ghc914;` in your flake-module config.
    ghc914 =
        if prev.haskell.packages ? ghc914
        then final.haskell.packages.ghc914.override {
            overrides = final.lib.composeManyExtensions [
                # The RC3 nixpkgs snapshot updated ghc-exactprint to 1.14.1.0,
                # while its GHC 9.14 configuration still references the removed
                # 1.14.0.0 attribute. Keep the old name as a compatibility alias
                # until nixpkgs updates configuration-ghc-9.14.x.nix.
                (self: super: {
                    ghc-exactprint_1_14_0_0 = final.haskell.lib.dontCheck super.ghc-exactprint_1_14_1_0;
                })
                (ihpOverrides final)
                (self: super: {
                    say = final.haskell.lib.dontCheck super.say;
                    text-icu = final.haskell.lib.dontCheck super.text-icu;
                    cryptonite = final.haskell.lib.dontCheck super.cryptonite;

                    # relude doctests fail due to changed GHC error messages in 9.14
                    relude = final.haskell.lib.dontCheck super.relude;

                    # HLS pulls this in; its tests import a hidden containers-0.8 module.
                    enummapset = final.haskell.lib.dontCheck super.enummapset;

                    # 0.19 supports GHC 9.14; nixpkgs still pins an older release.
                    ghc-tcplugin-api = self.callPackage "${flakeRoot}/NixSupport/hackage/ghc-tcplugin-api.nix" {};

                    # 0.9.6 supports GHC 9.14; nixpkgs still pins an older release.
                    ghc-typelits-natnormalise = final.haskell.lib.dontCheck
                        (self.callPackage "${flakeRoot}/NixSupport/hackage/ghc-typelits-natnormalise.nix" {});

                    # 0.8.4 supports GHC 9.14; nixpkgs still pins an older release.
                    ghc-typelits-knownnat = final.haskell.lib.dontCheck
                        (self.callPackage "${flakeRoot}/NixSupport/hackage/ghc-typelits-knownnat.nix" {});
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
                    # darcs 2.18.5 caps http-client-tls <0.4 and tls <2.2, while
                    # the RC3 package set provides newer compatible releases.
                    "darcs"
                    "lucid" "lucid2" "clay" "tasty-hspec" "config-ini" "fsnotify"
                    "string-interpolate" "rebase" "rerebase" "with-utf8" "minio-hs"
                    "sandwich" "brick" "postgresql-simple" "hasql-dynamic-statements"
                    "hasql-implicits" "warp-systemd" "ghc-trace-events"
                    "algebraic-graphs" "hie-bios" "stan" "modern-uri"
                    "ghc-lib-parser" "ghc-lib-parser-ex" "ghc-syntax-highlighter"
                    "colourista" "extensions" "trial" "trial-optparse-applicative"
                    "trial-tomland" "tomland" "validation-selective" "slist"
                    "ihp-zip"
                ])
            ];
        }
        else throw "ghc914 is not available in this nixpkgs";
}

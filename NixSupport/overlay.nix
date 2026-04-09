{ self, inputs }:
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
            # To regenerate: run ./update-nix-from-cabal.sh after changing .cabal files.
            localPackage = name: fastBuild (
                final.haskell.lib.overrideSrc
                    (super.callPackage "${flakeRoot}/${name}/default.nix" {})
                    { src = filteredSrc name; }
            );

            # Pre-generated nix file for ihp-zip (an IHP-maintained fork of zip that
            # doesn't live on Hackage). The rest of third-party Haskell deps now come
            # from the nixpkgs fork at digitallyinduced/nixpkgs:ihp-nixpkgs — see
            # flake.nix and the rebinds below.
            # To regenerate ihp-zip.nix: run ./update-nix-from-cabal.sh.
            hackagePackage = name: self.callPackage "${flakeRoot}/NixSupport/hackage/${name}.nix" {};

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
            ihp-context = localPackage "ihp-context";
            ihp-pagehead = localPackage "ihp-pagehead";
            ihp-log = localPackage "ihp-log";
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

            # Hasql 1.10 ecosystem: rebind from version-suffixed attrs that the
            # nixpkgs fork (digitallyinduced/nixpkgs:ihp-nixpkgs) ships as
            # extra-packages. This keeps haskellPackages.hasql etc. pointing at
            # the versions IHP needs without touching the global stackage pins.
            # Postgrest's own overrideScope in configuration-common.nix pins to
            # super.hasql_1_6_4_4 so it is unaffected by these rebinds.
            # dontCheck: upstream test suites connect to a running PostgreSQL.
            hasql = final.haskell.lib.dontCheck super.hasql_1_10_3;
            hasql-dynamic-statements = final.haskell.lib.dontCheck super.hasql-dynamic-statements_0_5_1;
            hasql-notifications = final.haskell.lib.dontCheck super.hasql-notifications_0_2_5_0;
            hasql-pool = final.haskell.lib.dontCheck super.hasql-pool_1_4_2;
            hasql-transaction = final.haskell.lib.dontCheck super.hasql-transaction_1_2_2;
            postgresql-binary = final.haskell.lib.dontCheck super.postgresql-binary_0_15_0_1;
            text-builder = super.text-builder_1_0_0_5;

            # postgresql-types family (nikita-volkov): nixpkgs ships the right
            # versions, they just need test skips and version-bound relaxation.
            hasql-mapping = final.haskell.lib.unmarkBroken super.hasql-mapping;
            hasql-postgresql-types = final.haskell.lib.dontHaddock (final.haskell.lib.doJailbreak super.hasql-postgresql-types);
            postgresql-simple-postgresql-types = final.haskell.lib.dontCheck (final.haskell.lib.doJailbreak super.postgresql-simple-postgresql-types);
            postgresql-types = final.haskell.lib.dontCheck (final.haskell.lib.doJailbreak super.postgresql-types);
            postgresql-types-algebra = final.haskell.lib.doJailbreak super.postgresql-types-algebra;
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

    # Experimental: GHC 9.14 (bleeding edge, expected to fail)
    # Only defined when nixpkgs includes the ghc914 package set.
    # Attribute always exists but throws if ghc914 is unavailable in nixpkgs.
    ghc914 =
        if prev.haskell.packages ? ghc914
        then final.haskell.packages.ghc914.override {
            overrides = final.lib.composeManyExtensions [
                (ihpOverrides final)
                (self: super: {
                    say = final.haskell.lib.dontCheck super.say;
                    text-icu = final.haskell.lib.dontCheck super.text-icu;
                    cryptonite = final.haskell.lib.dontCheck super.cryptonite;
                })
            ];
        }
        else throw "ghc914 is not available in this nixpkgs";
}

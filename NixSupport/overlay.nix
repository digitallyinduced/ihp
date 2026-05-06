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
            ihp-context = localPackage "ihp-context";
            ihp-router = localPackage "ihp-router";
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
            wai-csp = localPackage "wai-csp";

            # Forks of wai-session / wai-session-clientsession with deferred
            # session decryption and optional Set-Cookie (Maybe ByteString).
            # https://hackage.haskell.org/package/wai-session-maybe
            # https://hackage.haskell.org/package/wai-session-clientsession-deferred
            wai-session-maybe = hackagePackage "wai-session-maybe";
            wai-session-clientsession-deferred = hackagePackage "wai-session-clientsession-deferred";

            # Can be removed after v0.3.2 is on hackage
            # https://github.com/tippenein/countable-inflections/pull/6
            countable-inflections = final.haskell.lib.overrideSrc super.countable-inflections {
                version = "0.3.2";
                src = final.fetchFromGitHub {
                    owner = "tippenein";
                    repo = "countable-inflections";
                    rev = "9cae03513ad76783c226509f5c00dfe7989893e8";
                    hash = "sha256-Pd9wQgEtc3e39c0iJR347kdawbyShDEtQqEzrIEu0eQ=";
                };
            };

            # Hasql 1.10 ecosystem upgrade for postgresql-types binary encoding support.
            # Pre-generated nix files in NixSupport/hackage/ to avoid IFD.
            # dontCheck on postgresql/hasql packages: their tests require a running PostgreSQL server.
            postgresql-binary = final.haskell.lib.dontCheck (hackagePackage "postgresql-binary");
            postgresql-connection-string = hackagePackage "postgresql-connection-string";

            hasql = final.haskell.lib.dontCheck (final.haskell.lib.doJailbreak (hackagePackage "hasql"));
            hasql-pool = final.haskell.lib.dontCheck (hackagePackage "hasql-pool");
            hasql-dynamic-statements = final.haskell.lib.dontCheck (hackagePackage "hasql-dynamic-statements");
            # hasql-implicits 0.2.0.2 is the default on nixpkgs-unstable, no override needed
            hasql-transaction = final.haskell.lib.dontCheck (hackagePackage "hasql-transaction");
            hasql-notifications = final.haskell.lib.dontCheck (hackagePackage "hasql-notifications");
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

            # ptr-poker 0.1.3 is the default on nixpkgs-unstable, no override needed
            # postgresql-simple-postgresql-types: bridge providing FromField/ToField instances
            # for all postgresql-types types (Point, Polygon, Inet, Interval, etc.) in postgresql-simple
            postgresql-simple-postgresql-types = final.haskell.lib.dontCheck (final.haskell.lib.doJailbreak (hackagePackage "postgresql-simple-postgresql-types"));
            # ptr-peeker is marked broken in nixpkgs but is needed by postgresql-types
            # https://github.com/nikita-volkov/ptr-peeker/issues/10
            ptr-peeker = final.haskell.lib.dontCheck (final.haskell.lib.markUnbroken super.ptr-peeker);
            # postgresql-types-algebra 0.1 matches the nixpkgs default; only
            # doJailbreak is needed to widen its `ptr-peeker ^>=0.1` bound so
            # it accepts the markUnbroken ptr-peeker 0.2 above.
            postgresql-types-algebra = final.haskell.lib.doJailbreak super.postgresql-types-algebra;
            # dontCheck: tests require a running PostgreSQL server
            postgresql-types = final.haskell.lib.dontCheck (final.haskell.lib.doJailbreak (hackagePackage "postgresql-types"));
            hasql-mapping = final.haskell.lib.doJailbreak (hackagePackage "hasql-mapping");
            hasql-postgresql-types = final.haskell.lib.dontHaddock (final.haskell.lib.doJailbreak (hackagePackage "hasql-postgresql-types"));
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

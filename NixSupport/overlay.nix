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
            # To regenerate: run ./update-nix-from-cabal.sh after changing .cabal files.
            localPackage = name: fastBuild (
                final.haskell.lib.overrideSrc
                    (super.callPackage "${flakeRoot}/${name}/default.nix" {})
                    { src = filteredSrc name; }
            );

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
            ihp-context = hackageOrLocal "ihp-context";
            ihp-pagehead = hackageOrLocal "ihp-pagehead";
            ihp-log = hackageOrLocal "ihp-log";
            ihp-pglistener = hackageOrLocal "ihp-pglistener";
            ihp-modal = hackageOrLocal "ihp-modal";
            ihp-ide = localPackage "ihp-ide";
            ihp-schema-compiler = hackageOrLocal "ihp-schema-compiler";
            ihp-postgres-parser = hackageOrLocal "ihp-postgres-parser";
            ihp-mail = hackageOrLocal "ihp-mail";
            ihp-migrate = (localPackage "ihp-migrate").overrideAttrs (old: { mainProgram = "migrate"; });
            ihp-openai = localPackage "ihp-openai";
            ihp-ssc = hackageOrLocal "ihp-ssc";
            ihp-zip = fastBuild (super.callCabal2nix "ihp-zip" (final.fetchFromGitHub { owner = "digitallyinduced"; repo = "ihp-zip"; rev = "1c0d812d12d21269f83d6480a6ec7a8cdd054485"; sha256 = "0y0dj8ggi1jqzy74i0d6k9my8kdvfi516zfgnsl7znicwq9laald"; }) {});
            ihp-hsx = localPackage "ihp-hsx";
            ihp-graphql = hackageOrLocal "ihp-graphql";
            ihp-datasync-typescript = hackageOrLocal "ihp-datasync-typescript";
            ihp-sitemap = hackageOrLocal "ihp-sitemap";
            ihp-typed-sql = hackageOrLocal "ihp-typed-sql";
            ihp-datasync = hackageOrLocal "ihp-datasync";
            ihp-job-dashboard = hackageOrLocal "ihp-job-dashboard";
            wai-asset-path = hackageOrLocal "wai-asset-path";
            wai-flash-messages = hackageOrLocal "wai-flash-messages";
            wai-request-params = hackageOrLocal "wai-request-params";
            ihp-imagemagick = hackageOrLocal "ihp-imagemagick";
            ihp-hspec = hackageOrLocal "ihp-hspec";
            ihp-welcome = hackageOrLocal "ihp-welcome";

            # Forks of wai-session / wai-session-clientsession with deferred
            # session decryption and optional Set-Cookie (Maybe ByteString).
            # https://hackage.haskell.org/package/wai-session-maybe
            # https://hackage.haskell.org/package/wai-session-clientsession-deferred
            wai-session-maybe = super.callHackageDirect {
                pkg = "wai-session-maybe";
                ver = "1.0.0";
                sha256 = "sha256-DCdGNnwE9OC/E2ancM9tgxV1KNnAm/h0rJRd2hOa7ls=";
            } {};
            wai-session-clientsession-deferred = super.callHackageDirect {
                pkg = "wai-session-clientsession-deferred";
                ver = "1.0.0";
                sha256 = "sha256-l/AQrZCJklAEYl6v8rtx00ohLkYgylFtxaihtszJkkc=";
            } {};

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
            # callHackageDirect used because these versions are newer than the nixpkgs all-cabal-hashes snapshot.
            # dontCheck on postgresql/hasql packages: their tests require a running PostgreSQL server.
            postgresql-binary = final.haskell.lib.dontCheck (self.callHackageDirect { pkg = "postgresql-binary"; ver = "0.15.0.1"; sha256 = "02cj87xbhpq4jn0ys3pdscblan69d5f1vcsgb5y2piw310x7d6xb"; } {});
            postgresql-connection-string = self.callHackageDirect { pkg = "postgresql-connection-string"; ver = "0.1.0.6"; sha256 = "07iykhnjzryqqc1mccnmqf7lkg12rb4dq5azvrpfq6qaf6a6r0r1"; } {};

            hasql = final.haskell.lib.dontCheck (final.haskell.lib.doJailbreak (self.callHackageDirect { pkg = "hasql"; ver = "1.10.2.3"; sha256 = "1j52ia75168n88rrraf4g20grdl3qak8r426rav87kjjjqx3717v"; } {}));
            hasql-pool = final.haskell.lib.dontCheck (self.callHackageDirect { pkg = "hasql-pool"; ver = "1.4.2"; sha256 = "0gw8brk3kwb1s58s0npbmszh5byqv0frjyaql7mgkc317x67c049"; } {});
            hasql-dynamic-statements = final.haskell.lib.dontCheck (self.callHackageDirect { pkg = "hasql-dynamic-statements"; ver = "0.5.1"; sha256 = "13c04wb1635361wrszn2kn4s5ygl7yzv8yn6bvpxgm2j7hr0v94q"; } {});
            hasql-implicits = self.callHackageDirect { pkg = "hasql-implicits"; ver = "0.2.0.2"; sha256 = "0nyz96mgrc4i7x3q8wwv6zq8qpwam13f5y1rlbh102jp2ygb2mjy"; } {};
            hasql-transaction = final.haskell.lib.dontCheck (self.callHackageDirect { pkg = "hasql-transaction"; ver = "1.2.2"; sha256 = "0y1clnyw76rszsdvz0fxj2az036bmw1whp6pqchyjamnbkmf37d3"; } {});
            hasql-notifications = final.haskell.lib.dontCheck (self.callHackageDirect { pkg = "hasql-notifications"; ver = "0.2.5.0"; sha256 = "11jkrngiy175wc5hqx8pgagj4fdg42ry7afp4g4rr5hw8h43zg48"; } {});
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

            # postgresql-types for proper binary encoders of Point, Polygon, Inet, Interval
            ptr-poker = self.callHackageDirect { pkg = "ptr-poker"; ver = "0.1.3"; sha256 = "0jl9df0kzsq5gd6fhfqc8my4wy7agg5q5jw4q92h4b7rkdf3hix7"; } {};
            # postgresql-simple-postgresql-types: bridge providing FromField/ToField instances
            # for all postgresql-types types (Point, Polygon, Inet, Interval, etc.) in postgresql-simple
            postgresql-simple-postgresql-types = final.haskell.lib.dontCheck (final.haskell.lib.doJailbreak (self.callHackageDirect { pkg = "postgresql-simple-postgresql-types"; ver = "0.1.1"; sha256 = "09xqrcpp56jbfjqk9njw6l7aw13qi2838rwqg2xc483sjp0jxxzd"; } {}));
            # ptr-peeker is marked broken in nixpkgs but is needed by postgresql-types
            # https://github.com/nikita-volkov/ptr-peeker/issues/10
            ptr-peeker = final.haskell.lib.dontCheck (final.haskell.lib.markUnbroken super.ptr-peeker);
            postgresql-types-algebra = final.haskell.lib.doJailbreak (self.callHackageDirect { pkg = "postgresql-types-algebra"; ver = "0.1"; sha256 = "0ishl9dag7w73bclpaja4wj3s6jf8958jls2ffn1a6h3p9v40pfv"; } {});
            # dontCheck: tests require a running PostgreSQL server
            postgresql-types = final.haskell.lib.dontCheck (final.haskell.lib.doJailbreak (self.callHackageDirect { pkg = "postgresql-types"; ver = "0.1.2"; sha256 = "1plkc0pjhlbml5innkla44jad1jx8f876kw5ckz168jxvzrkb4jc"; } {}));
            hasql-mapping = final.haskell.lib.doJailbreak (self.callHackageDirect { pkg = "hasql-mapping"; ver = "0.1"; sha256 = "1l6p7sbw6wwkk964bs3hljmja1kwy0b9gld24g71dcbjs24hchcf"; } {});
            hasql-postgresql-types = final.haskell.lib.dontHaddock (final.haskell.lib.doJailbreak (self.callHackageDirect { pkg = "hasql-postgresql-types"; ver = "0.2"; sha256 = "0841s41izgjg4qfw6s74bwhixby3rwj167a2p8vbwp4xlf8wzaz6"; } {}));
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

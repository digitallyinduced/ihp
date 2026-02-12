{ self, inputs }:
let
    flakeRoot = self;

    ihpOverrides = final: self: super:
        let
            filter = inputs.nix-filter.lib;
            # Disable profiling and haddock for faster local builds
            fastBuild = pkg: final.haskell.lib.disableLibraryProfiling (final.haskell.lib.dontHaddock pkg);

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
            ihp-postgresql-simple-extra = localPackage "ihp-postgresql-simple-extra";
            ihp-ssc = localPackage "ihp-ssc";
            ihp-zip = fastBuild (super.callCabal2nix "ihp-zip" (final.fetchFromGitHub { owner = "digitallyinduced"; repo = "ihp-zip"; rev = "1c0d812d12d21269f83d6480a6ec7a8cdd054485"; sha256 = "0y0dj8ggi1jqzy74i0d6k9my8kdvfi516zfgnsl7znicwq9laald"; }) {});
            ihp-hsx = localPackage "ihp-hsx";
            ihp-graphql = localPackage "ihp-graphql";
            ihp-datasync-typescript = localPackage "ihp-datasync-typescript";
            ihp-sitemap = localPackage"ihp-sitemap";
            ihp-datasync = localPackage "ihp-datasync";
            ihp-job-dashboard = localPackage"ihp-job-dashboard";
            wai-asset-path = localPackage "wai-asset-path";
            wai-flash-messages = localPackage "wai-flash-messages";
            wai-request-params = localPackage "wai-request-params";
            ihp-imagemagick = localPackage "ihp-imagemagick";
            ihp-hspec = localPackage "ihp-hspec";
            ihp-welcome = localPackage "ihp-welcome";

            # Lazy session middleware: defer cookie decryption until first access,
            # skip Set-Cookie when session is unmodified.
            # PRs: https://github.com/singpolyma/wai-session/pull/17
            #       https://github.com/singpolyma/wai-session-clientsession/pull/5
            wai-session = final.haskell.lib.appendPatch super.wai-session
                (builtins.fetchurl {
                    url = "https://github.com/singpolyma/wai-session/commit/c0142c100975d7f4ba7516f5235d30a3e88e32a2.patch";
                    sha256 = "0ymv7zwg4wdkdyz8wrfyjcprjq9iyik7iiaazsi1d6vhgm3fv6ls";
                });
            wai-session-clientsession = final.haskell.lib.appendPatch super.wai-session-clientsession
                (builtins.fetchurl {
                    url = "https://github.com/singpolyma/wai-session-clientsession/commit/8b383eac381b6a96ceaa7b3a282b0fe856452f78.patch";
                    sha256 = "02127wd8y26aps34kmvxxvyyzg94p3i9f3drf7zwp8phdncb19pg";
                });

            # Can be removed after v0.3.2 is on hackage
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
            # callCabal2nix used because these versions are newer than the nixpkgs all-cabal-hashes snapshot.

            # Dependencies required by hasql 1.10
            postgresql-binary = final.haskell.lib.dontCheck (fastBuild (self.callCabal2nix "postgresql-binary" (builtins.fetchTarball {
                url = "https://hackage.haskell.org/package/postgresql-binary-0.15/postgresql-binary-0.15.tar.gz";
                sha256 = "11ysy91rsvdx9n7cjpyhp23ikv3h9b40k6rdggykhjkdv7vdvhj3";
            }) {}));
            # Patched to fix parsing of empty password in URI format (user:@host)
            # See: https://github.com/nikita-volkov/postgresql-connection-string/pull/3
            postgresql-connection-string = fastBuild (self.callCabal2nix "postgresql-connection-string" (builtins.fetchTarball {
                url = "https://github.com/mpscholten/postgresql-connection-string/archive/bb9bfb8cfff39e0e87aa24208d0e34a0cefb13cc.tar.gz";
                sha256 = "1jw3ka11anvx9prr5iliqwi42h5jlwbq8i0xlarzq2hgg3sc0sqp";
            }) {});

            hasql = final.haskell.lib.dontCheck (final.haskell.lib.doJailbreak (fastBuild (self.callCabal2nix "hasql" (builtins.fetchTarball {
                url = "https://hackage.haskell.org/package/hasql-1.10.2/hasql-1.10.2.tar.gz";
                sha256 = "1hzp59kzskqq3i0qfrhla7qdmnysyp9njkyryi8vd99wbz3skyn2";
            }) {})));
            hasql-pool = final.haskell.lib.dontCheck (fastBuild (self.callCabal2nix "hasql-pool" (builtins.fetchTarball {
                url = "https://hackage.haskell.org/package/hasql-pool-1.4.1/hasql-pool-1.4.1.tar.gz";
                sha256 = "08my6djljjgpkxgk4xc3z314ad0rf6g4yvv470rmm12nbzj2g66a";
            }) {}));
            hasql-dynamic-statements = final.haskell.lib.dontCheck (fastBuild (self.callCabal2nix "hasql-dynamic-statements" (builtins.fetchTarball {
                url = "https://hackage.haskell.org/package/hasql-dynamic-statements-0.5.0.1/hasql-dynamic-statements-0.5.0.1.tar.gz";
                sha256 = "1vdydp8n0zq3mwkzids64b86d9q2l11yc8df4brhmwx06qmvq3sc";
            }) {}));
            hasql-implicits = fastBuild (self.callCabal2nix "hasql-implicits" (builtins.fetchTarball {
                url = "https://hackage.haskell.org/package/hasql-implicits-0.2.0.2/hasql-implicits-0.2.0.2.tar.gz";
                sha256 = "0nyz96mgrc4i7x3q8wwv6zq8qpwam13f5y1rlbh102jp2ygb2mjy";
            }) {});
            hasql-transaction = final.haskell.lib.dontCheck (fastBuild (self.callCabal2nix "hasql-transaction" (builtins.fetchTarball {
                url = "https://hackage.haskell.org/package/hasql-transaction-1.2.2/hasql-transaction-1.2.2.tar.gz";
                sha256 = "0y1clnyw76rszsdvz0fxj2az036bmw1whp6pqchyjamnbkmf37d3";
            }) {}));
            hasql-notifications = final.haskell.lib.dontCheck (fastBuild (self.callCabal2nix "hasql-notifications" (builtins.fetchTarball {
                url = "https://hackage.haskell.org/package/hasql-notifications-0.2.5.0/hasql-notifications-0.2.5.0.tar.gz";
                sha256 = "11jkrngiy175wc5hqx8pgagj4fdg42ry7afp4g4rr5hw8h43zg48";
            }) {}));

            # postgresql-types for proper binary encoders of Point, Polygon, Inet, Interval
            # ptr-poker 0.1.3 needed for toByteString (older nixpkgs version has writeToByteString)
            # Not using fastBuild because jsonifier needs profiling libs
            ptr-poker = self.callCabal2nix "ptr-poker" (builtins.fetchTarball {
                url = "https://hackage.haskell.org/package/ptr-poker-0.1.3/ptr-poker-0.1.3.tar.gz";
                sha256 = "0jl9df0kzsq5gd6fhfqc8my4wy7agg5q5jw4q92h4b7rkdf3hix7";
            }) {};
            # ptr-peeker is marked broken in nixpkgs but is needed by postgresql-types
            ptr-peeker = final.haskell.lib.dontCheck (final.haskell.lib.markUnbroken super.ptr-peeker);
            postgresql-types-algebra = final.haskell.lib.doJailbreak (fastBuild (self.callCabal2nix "postgresql-types-algebra" (builtins.fetchTarball {
                url = "https://hackage.haskell.org/package/postgresql-types-algebra-0.1/postgresql-types-algebra-0.1.tar.gz";
                sha256 = "0ishl9dag7w73bclpaja4wj3s6jf8958jls2ffn1a6h3p9v40pfv";
            }) {}));
            # Using fork with Tsvector support: https://github.com/nikita-volkov/postgresql-types/pull/63
            postgresql-types = final.haskell.lib.dontCheck (final.haskell.lib.doJailbreak (fastBuild (self.callCabal2nix "postgresql-types" (builtins.fetchTarball {
                url = "https://github.com/digitallyinduced/postgresql-types/archive/3100a6630c2f7f410514c46137ef49dc992c75e2.tar.gz";
                sha256 = "1iyb7dqk0nxzhyp627hd3xh0hj7hc1dhfsvwf6cj50iqgw3lnfgv";
            }) {})));
            # hasql-mapping provides the IsScalar typeclass for hasql encoder/decoder integration
            # Not on Hackage, only on GitHub. Patched to export IsScalar(..) from Hasql.Mapping
            # so that hasql-postgresql-types can define orphan instances.
            hasql-mapping = final.haskell.lib.doJailbreak (fastBuild (final.haskell.lib.overrideCabal (self.callCabal2nix "hasql-mapping" (builtins.fetchTarball {
                url = "https://github.com/nikita-volkov/hasql-mapping/archive/307dfb5f25ba28d8408fac3aa160ca4ba702acc9.tar.gz";
                sha256 = "1ww54his5d3wfh3amdk9zk5w6v4pdgljlzifnqga3lwn1gasbsvr";
            }) {}) (old: {
                postPatch = (old.postPatch or "") + ''
                    substituteInPlace src/library/Hasql/Mapping.hs \
                        --replace-warn "import Hasql.Mapping.IsScalar (IsScalar)" \
                                       "import Hasql.Mapping.IsScalar (IsScalar(..))" \
                        --replace-warn "( IsScalar," \
                                       "( IsScalar(..),"
                '';
            })));
            # Patched to add Tsvector instance (added in postgresql-types fork)
            hasql-postgresql-types = final.haskell.lib.doJailbreak (fastBuild (final.haskell.lib.overrideCabal (self.callCabal2nix "hasql-postgresql-types" (builtins.fetchTarball {
                url = "https://github.com/nikita-volkov/hasql-postgresql-types/archive/b8cb8fe1e7eb.tar.gz";
                sha256 = "0fffxiavxn70nis9rqgx2z9rp030x1afdr7qj8plwncif3qvsv1f";
            }) {}) (old: {
                postPatch = (old.postPatch or "") + ''
                    cat >> src/library/Hasql/PostgresqlTypes.hs << 'TSVECTOR_INSTANCE'

                    instance Hasql.Mapping.IsScalar Tsvector where
                      encoder = Core.encoder
                      decoder = Core.decoder
                    TSVECTOR_INSTANCE
                '';
            })));
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
}

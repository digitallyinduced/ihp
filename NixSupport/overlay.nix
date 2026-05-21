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
            # nixpkgs is pinned (see flake.nix) to a staging-next commit
            # containing NixOS/nixpkgs#519795, which (a) adds `dontCheck` for
            # the versioned hasql attrs in configuration-nix.nix and (b) unmarks
            # hasql-mapping / postgresql-simple-postgresql-types inside its
            # upstream IHP scope. We therefore consume these attrs verbatim
            # (no local dontCheck) so the derivations are bit-identical to
            # nixpkgs' Hydra-built `haskellPackages.ihp` closure and resolve
            # straight from cache.nixos.org instead of rebuilding hasql + GHC
            # + the Haskell dep tree from source. The mirrored attrs below
            # match the upstream `ihpHasqlScope` exactly — keep them in sync
            # with configuration-common.nix. Revert together with the flake.nix
            # pin once #519795 reaches the nixpkgs-unstable channel.
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

            # postgresql-types / ptr-peeker bridge stack.
            #
            # At the pinned #519795 nixpkgs rev, Hydra builds IHP's entire
            # third-party closure green (haskellPackages.ihp) and uploads it to
            # cache.nixos.org — including ptr-peeker 0.2.0.1, postgresql-types
            # 0.1.3.2, postgresql-types-algebra and hasql-postgresql-types.
            # ptr-peeker is no longer in broken.yaml and the `ptr-peeker ^>=0.1`
            # bound problems that needed doJailbreak are gone at these versions,
            # so ptr-peeker, postgresql-types, postgresql-types-algebra and
            # hasql-postgresql-types are consumed verbatim from `super` (no
            # entry here at all). Any transform would change the derivation hash
            # and force a from-source rebuild of that package *and its whole
            # reverse-dependency cone*, defeating the cache. Keep in sync with
            # the upstream IHP scope; revert with the flake.nix pin.
            #
            # postgresql-simple-postgresql-types and hasql-mapping are the only
            # exceptions: both are genuinely in broken.yaml, so they need
            # markUnbroken — mirroring upstream ihpHasqlScope's `unmarkBroken`
            # exactly (configuration-nix.nix already applies the dontCheck for
            # postgresql-simple-postgresql-types). No doJailbreak / dontHaddock:
            # those would diverge from the cached upstream derivations.
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

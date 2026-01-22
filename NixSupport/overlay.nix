{ self, inputs }:
let
    flakeRoot = self;
in
final: prev: {
    ghc = final.haskellPackages.override {
        overrides = self: super:
            let
                filter = inputs.nix-filter.lib;
                # Disable profiling and haddock for faster local builds
                fastBuild = pkg: final.haskell.lib.disableLibraryProfiling (final.haskell.lib.dontHaddock pkg);
                localPackage = name: fastBuild (super.callCabal2nix name (filter { root = "${toString flakeRoot}/${name}"; include = [ (filter.matchExt "hs") (filter.matchExt "cabal") (filter.matchExt "md") filter.isDirectory "LICENSE" "data" ]; }) {});
                # ihp-with-docs has haddock for reference docs
                localPackageWithHaddock = name: final.haskell.lib.disableLibraryProfiling (super.callCabal2nix name (filter { root = "${toString flakeRoot}/${name}"; include = [ (filter.matchExt "hs") (filter.matchExt "cabal") (filter.matchExt "md") filter.isDirectory "LICENSE" "data" ]; }) {});
        in {
            ihp = localPackage "ihp";
            ihp-with-docs = localPackageWithHaddock "ihp";
            ihp-context = localPackage "ihp-context";
            ihp-pagehead = localPackage "ihp-pagehead";
            ihp-log = localPackage "ihp-log";
            ihp-modal = localPackage "ihp-modal";
            ihp-ide = localPackage "ihp-ide";
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
            ihp-imagemagick = localPackage "ihp-imagemagick";
            ihp-hspec = localPackage "ihp-hspec";
            ihp-welcome = localPackage "ihp-welcome";

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
        };
    };
}
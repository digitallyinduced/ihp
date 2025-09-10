{ self, inputs }:
let
    flakeRoot = self;
in
final: prev: {
    ghc = final.haskell.packages.ghc98.override {
        overrides = self: super: {
            ihp = super.callCabal2nix "ihp" "${toString flakeRoot}/ihp" {};
            ihp-ide = super.callCabal2nix "ihp-ide" "${toString flakeRoot}/ihp-ide" {};
            ihp-migrate = (super.callCabal2nix "ihp-migrate" "${toString flakeRoot}/ihp-migrate" {}).overrideAttrs (old: { mainProgram = "migrate"; });
            ihp-openai = super.callCabal2nix "ihp-openai" "${toString flakeRoot}/ihp-openai" {};
            ihp-postgresql-simple-extra = super.callCabal2nix "ihp-postgresql-simple-extra" "${toString flakeRoot}/ihp-postgresql-simple-extra" {};
            ihp-ssc = super.callCabal2nix "ihp-ssc" "${toString flakeRoot}/ihp-ssc" {};
            ihp-zip = super.callCabal2nix "ihp-zip" (final.fetchFromGitHub { owner = "digitallyinduced"; repo = "ihp-zip"; rev = "1c0d812d12d21269f83d6480a6ec7a8cdd054485"; sha256 = "0y0dj8ggi1jqzy74i0d6k9my8kdvfi516zfgnsl7znicwq9laald"; }) {};
            ihp-hsx = super.callCabal2nix "ihp-hsx" "${toString flakeRoot}/ihp-hsx" {};
            ihp-graphql = super.callCabal2nix "ihp-graphql" "${toString flakeRoot}/ihp-graphql" {};
            ihp-datasync-typescript = super.callCabal2nix "ihp-datasync-typescript" "${toString flakeRoot}/ihp-datasync-typescript" {};
            ihp-sitemap = super.callCabal2nix "ihp-sitemap" "${toString flakeRoot}/ihp-sitemap" {};
            
            ihp-datasync = super.callCabal2nix "ihp-datasync" "${toString flakeRoot}/ihp-datasync" {};
            ihp-job-dashboard = super.callCabal2nix "ihp-job-dashboard" "${toString flakeRoot}/ihp-job-dashboard" {};

            fsnotify = final.haskell.lib.dontCheck (super.callHackageDirect { pkg = "fsnotify"; ver = "0.4.3.0"; sha256 = "sha256-6SJ8w2p0HNhMPjdQzxx4oqsyI48/C/K7wh+kLNy9/fM="; } {});
            
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
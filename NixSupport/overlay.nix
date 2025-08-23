{ self, inputs }:
let
    flakeRoot = self;
in
final: prev: {
    ghc = final.haskell.packages.ghc98.override {
        overrides = self: super: {
            ihp = super.callPackage "${toString flakeRoot}/ihp.nix" { filter = inputs.nix-filter.lib; };
            ihp-ide = super.callPackage "${toString flakeRoot}/NixSupport/haskell-packages/ihp-ide.nix" {};
            ihp-migrate = super.callPackage "${toString flakeRoot}/NixSupport/haskell-packages/ihp-migrate.nix" {};
            ihp-openai = super.callPackage "${toString flakeRoot}/NixSupport/haskell-packages/ihp-openai.nix" {};
            ihp-postgresql-simple-extra = super.callPackage "${toString flakeRoot}/NixSupport/haskell-packages/ihp-postgresql-simple-extra.nix" {};
            ihp-ssc = super.callPackage "${toString flakeRoot}/NixSupport/haskell-packages/ihp-ssc.nix" {};
            ihp-zip = super.callPackage "${toString flakeRoot}/NixSupport/haskell-packages/ihp-zip.nix" {};
            # ihp-hsx = super.callPackage "${toString flakeRoot}/NixSupport/haskell-packages/ihp-hsx.nix" {};
            ihp-graphql = super.callPackage "${toString flakeRoot}/NixSupport/haskell-packages/ihp-graphql.nix" {};
            ihp-datasync-typescript = super.callPackage "${toString flakeRoot}/NixSupport/haskell-packages/ihp-datasync-typescript.nix" {};

            # ihp-pro
            ihp-stripe = super.callPackage "${toString flakeRoot}/NixSupport/haskell-packages/ihp-stripe.nix" {};
            ihp-sentry = super.callPackage "${toString flakeRoot}/NixSupport/haskell-packages/ihp-sentry.nix" {};
            ihp-oauth-github = super.callPackage "${toString flakeRoot}/NixSupport/haskell-packages/ihp-oauth-github.nix" {};
            ihp-oauth-google = super.callPackage "${toString flakeRoot}/NixSupport/haskell-packages/ihp-oauth-google.nix" {};

            fsnotify = final.haskell.lib.dontCheck (super.callHackageDirect { pkg = "fsnotify"; ver = "0.4.3.0"; sha256 = "sha256-6SJ8w2p0HNhMPjdQzxx4oqsyI48/C/K7wh+kLNy9/fM="; } {});
            
            # Can be removed after https://github.com/tippenein/countable-inflections/pull/6 is merged
            countable-inflections = final.haskell.lib.overrideSrc super.countable-inflections {
                version = "0.3.0-idempotent-pluralize";
                src = final.fetchFromGitHub {
                    owner = "mpscholten";
                    repo = "countable-inflections";
                    rev = "eea519973d6b59291054beab161bcc948b1b580f";
                    hash = "sha256-Pd9wQgEtc3e39c0iJR347kdawbyShDEtQqEzrIEu0eQ=";
                };
            };
        };
    };
}
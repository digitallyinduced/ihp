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
            ihp-hsx = super.callPackage "${toString flakeRoot}/NixSupport/haskell-packages/ihp-hsx.nix" {};
            ihp-graphql = super.callPackage "${toString flakeRoot}/NixSupport/haskell-packages/ihp-graphql.nix" {};

            fsnotify = super.callHackageDirect { pkg = "fsnotify"; ver = "0.4.3.0"; sha256 = "sha256-6SJ8w2p0HNhMPjdQzxx4oqsyI48/C/K7wh+kLNy9/fM="; } {};
        };
    };
}
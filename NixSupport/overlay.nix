{ self, inputs }:

final: prev: {
    ghc = import ./mkGhcCompiler.nix {
        pkgs = final;
        ghcCompiler = final.haskell.packages.ghc98;
        ihp = self;
        filter = inputs.nix-filter.lib;
    };
}
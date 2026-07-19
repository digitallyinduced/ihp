{ pkgs }:

let
    lib = pkgs.lib;
in
{
    ghc = pkgs.haskellPackages.ghc;
    cabal2nix-unwrapped = pkgs.cabal2nix-unwrapped;
    jailbreak-cabal = pkgs.haskellPackages.buildHaskellPackages.jailbreak-cabal;
    gmp6 = lib.getLib (pkgs.gmp6.override { withStatic = true; });
    libffi = lib.getLib (pkgs.libffi.overrideAttrs (_old: {
        dontDisableStatic = true;
    }));
    ncurses = lib.getLib (pkgs.ncurses.override { enableStatic = true; });
    zlib = lib.getLib pkgs.zlib;
    libpq = pkgs.libpq.dev;
    openssl = lib.getLib pkgs.openssl;
    icu = pkgs.icu.static;
} // lib.optionalAttrs (pkgs ? numactl) {
    numactl = lib.getLib (pkgs.numactl.overrideAttrs (oldAttrs: {
        configureFlags = (oldAttrs.configureFlags or []) ++ [
            "--enable-static"
            "--disable-shared"
        ];
    }));
}

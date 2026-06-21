{ pkgs }:

let
    lib = pkgs.lib;
in
{
    ghc = pkgs.haskellPackages.ghc;
    cabal2nix-unwrapped = pkgs.cabal2nix-unwrapped;
    jailbreak-cabal = pkgs.haskellPackages.buildHaskellPackages.jailbreak-cabal;
    gmp6 = pkgs.gmp6.override { withStatic = true; };
    libffi = pkgs.libffi.overrideAttrs (_old: {
        dontDisableStatic = true;
    });
    ncurses = pkgs.ncurses.override { enableStatic = true; };
    zlib = pkgs.zlib.static;
    libpq = pkgs.libpq;
    openssl = pkgs.openssl;
} // lib.optionalAttrs (pkgs ? numactl) {
    numactl = pkgs.numactl.overrideAttrs (oldAttrs: {
        configureFlags = (oldAttrs.configureFlags or []) ++ [
            "--enable-static"
            "--disable-shared"
        ];
    });
}

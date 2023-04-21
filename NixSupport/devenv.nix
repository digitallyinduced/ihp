{ pkgs, config, lib, ... }:

let
  cfg = config.ihp;
  types = lib.types;
in
{
  options.ihp = {
    enable = lib.mkEnableOption "Enable IHP support";

    ghcCompiler = lib.mkOption {
      default = pkgs.haskell.packages.ghc944;

      description = ''
        The GHC compiler to use for IHP.
      '';
    };

    haskellPackages = lib.mkOption {
      default = p: with p; [
        cabal-install
        base
        wai
        text
        hlint
        ihp
      ];
      description = ''
        List of Haskell packages to be installed in the IHP environment.
      '';
    };
  };

  config = let
    ghcCompiler = import ./mkGhcCompiler.nix {
      inherit pkgs;
      inherit (cfg) ghcCompiler;
      ihp = ../.;
    };
  in lib.mkIf cfg.enable {
    packages = [
      pkgs.postgresql_13
    ];

    languages.haskell.enable = true;
    languages.haskell.package = ghcCompiler.ghc.withPackages cfg.haskellPackages;

    scripts.start.exec = ''
      ${ghcCompiler.ihp}/bin/RunDevServer
    '';

    processes.devServer.exec = "start";
  };
}

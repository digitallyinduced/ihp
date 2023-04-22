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

    projectPath = lib.mkOption {
      type = types.path;
      description = ''
        Path to the IHP project.
      '';
    };
  };

  config =
    let
      ghcCompiler = import ./mkGhcCompiler.nix {
        inherit pkgs;
        inherit (cfg) ghcCompiler;
        ihp = ../.;
      };
    in
    lib.mkIf cfg.enable {
      packages = [ ghcCompiler.ihp ]; 

      languages.haskell.enable = true;
      languages.haskell.package = ghcCompiler.ghc.withPackages cfg.haskellPackages;

      scripts.start.exec = ''
        ${ghcCompiler.ihp}/bin/RunDevServer
      '';

      processes.devServer.exec = "start";

      env.IHP_DEVENV = "1";
      env.DATABASE_URL = "postgres:///app?host=${config.env.PGHOST}";

      services.postgres.enable = true;
      services.postgres.initialDatabases = [
        {
          name = "app";
          schema = pkgs.runCommand "ihp-schema" {} ''
            touch $out

            echo "-- IHPSchema.sql" >> $out
            echo "" >> $out
            cat ${../lib/IHP/IHPSchema.sql} | sed -e s'/--.*//' | sed -e s'/$/\\/' >> $out
            echo "" >> $out
            echo "-- Application/Schema.sql"
            echo "" >> $out
            cat ${cfg.projectPath}/Application/Schema.sql | sed -e s'/--.*//' | sed -e s'/$/\\/' >> $out
            echo "" >> $out
            echo "-- Application/Fixtures.sql" >> $out
            echo "" >> $out
            cat ${cfg.projectPath}/Application/Fixtures.sql | sed -e s'/--.*//' | sed -e s'/$/\\/' >> $out
          '';
        }
      ];
    };
}

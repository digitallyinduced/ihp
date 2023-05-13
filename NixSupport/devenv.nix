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

    dontCheckPackages = lib.mkOption {
      default = [];
      description = ''
        List of Haskell package names whose tests are skipped during build
      '';
    };
    
    doJailbreakPackages = lib.mkOption {
      default = [];
      description = ''
        List of Haskell package names who are jailbreaked before build
      '';
    };
    dontHaddockPackages = lib.mkOption {
      default = [];
      description = ''
        List of Haskell package names whose haddock is not build during app build
      '';
    };
  };

  config =
    let
      ghcCompiler = import ./mkGhcCompiler.nix {
        inherit pkgs;
        inherit (cfg) ghcCompiler dontCheckPackages doJailbreakPackages dontHaddockPackages;
        ihp = ../.;
        haskellPackagesDir = cfg.projectPath + "/Config/nix/haskell-packages";
      };
    in
    lib.mkIf cfg.enable {
      packages = [ ghcCompiler.ihp pkgs.postgresql_13 ]; 

      languages.haskell.enable = true;
      languages.haskell.package = ghcCompiler.ghc.withPackages cfg.haskellPackages;

      scripts.start.exec = ''
        ${ghcCompiler.ihp}/bin/RunDevServer
      '';

      processes.devServer.exec = "start";

      # Disabled for now
      # Can be re-enabled once postgres is provided by devenv instead of IHP
      # env.IHP_DEVENV = "1";
      # env.DATABASE_URL = "postgres:///app?host=${config.env.PGHOST}";

      # Disabled for now
      # As the devenv postgres uses a different location for the socket
      # this would break lots of known commands such as `make db`
      services.postgres.enable = false;
      services.postgres.initialDatabases = [
        {
          name = "app";
          schema = pkgs.runCommand "ihp-schema" {} ''
            touch $out

            echo "-- IHPSchema.sql" >> $out
            echo "" >> $out
            cat ${../lib/IHP/IHPSchema.sql} | sed -e s'/--.*//' | sed -e s'/$/\\/' >> $out
            echo "" >> $out
            echo "-- Application/Schema.sql" >> $out
            echo "" >> $out
            cat ${cfg.projectPath + "/Application/Schema.sql"} | sed -e s'/--.*//' | sed -e s'/$/\\/' >> $out
            echo "" >> $out
            echo "-- Application/Fixtures.sql" >> $out
            echo "" >> $out
            cat ${cfg.projectPath + "/Application/Fixtures.sql"} | sed -e s'/--.*//' | sed -e s'/$/\\/' >> $out
          '';
        }
      ];
    };
}

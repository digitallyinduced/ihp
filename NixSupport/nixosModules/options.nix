# Running IHP app + a local Postgres connected to it
{ config, pkgs, modulesPath, lib, ihpApp, ... }:
{
    options.services.ihp = {
        enable = mkEnableOption "IHP";
        domain = mkOption {
            type = types.str;
            default = "localhost";
        };
        
        migrations = mkOption {
            type = types.path;
        };
        
        schema = mkOption {
            type = types.path;
        };
        
        fixtures = mkOption {
            type = types.path;
        };
        
        httpsEnabled = mkOption {
            type = types.boolean;
            default = true;
        };
        
        databaseName = mkOption {
            type = types.str;
            default = "app";
        };
        
        databaseUser = mkOption {
            type = types.str;
            default = "ihp";
        };
        
        # https://ihp.digitallyinduced.com/Guide/database-migrations.html#skipping-old-migrations
        minimumRevision = mkOption {
            type = types.int;
            default = 0;
        };
        
        ihpEnv = mkOption {
            type = types.str;
            default = "Production";
        };
        
        appPort = mkOption {
            type = types.int;
            default = 8000;
        };
        
        requestLoggerIPAddrSource = mkOption {
            type = types.str;
            default = "FromHeader";
        };
        
        sessionSecret = mkOption {
            type = types.str;
        };
        
        addionalEnvVars = mkOption {
            type = types.attrs;
            default = {};
        };
    };
}


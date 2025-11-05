# Running IHP app + a local Postgres connected to it
{ self, config, pkgs, modulesPath, lib, ... }:
with lib;
{
    options.services.ihp = {
        enable = mkEnableOption "IHP";
        domain = mkOption {
            type = types.str;
            default = "localhost";
        };

        baseUrl = mkOption {
            type = types.str;
            default = "https://${config.services.ihp.domain}";
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
            type = types.bool;
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

        databaseUrl = mkOption {
            type = types.str;
        };

        # https://ihp.digitallyinduced.com/Guide/database-migrations.html#skipping-old-migrations
        minimumRevision = mkOption {
            type = types.int;
            default = 0;
        };

        # https://ihp.digitallyinduced.com/Guide/database-migrations.html#ihp-migrations-dir
        ihpMigrationDir = mkOption {
            type = types.str;
            default = "Application/Migration/";
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
            description = ''
                It's recommended to use sessionSecretFile instead
            '';
        };

        sessionSecretFile = mkOption {
            type = types.path;
            default = "/var/ihp/session.aes";
            description = ''
                The session secret is stored here.

                If the file doesn't exists, the service will generate a new key automatically.

                When the key changes all users need to relogin.
            '';
        };

        additionalEnvVars = mkOption {
            type = types.attrs;
            default = {};
        };

        package = mkOption {
            type = types.package;
            default = if config.services.ihp.optimized then self.packages."${pkgs.system}".optimized-prod-server else self.packages."${pkgs.system}".default;
        };

        optimized = mkOption {
            type = types.bool;
            default = false;
        };

        rtsFlags = mkOption {
            type = types.str;
            default = "-A96m -n4m -N";
        };
    };
}

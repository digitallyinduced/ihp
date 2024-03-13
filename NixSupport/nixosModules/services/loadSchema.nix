{ self, config, pkgs, ihp, ... }:
let cfg = config.services.ihp;
in
{
    systemd.services.loadSchema = {
        serviceConfig = {
            Type = "oneshot";
        };
        script = ''
            DB_URL=''${DATABASE_URL:-''${DEFAULT_DATABASE_URL}}
            psql "$DB_URL" < ${self.packages.x86_64-linux.ihp-schema}/IHPSchema.sql
            psql "$DB_URL" < ${self.packages.x86_64-linux.schema}/Schema.sql
        '';
        environment = {
            DEFAULT_DATABASE_URL = cfg.databaseUrl;
        };
    };
}
{ config, pkgs, lib, ihp, ... }:
let cfg = config.services.ihp;
in
{
    systemd.services.migrate = lib.mkIf (cfg.migrations != null) {
        serviceConfig = {
            Type = "oneshot";
            ExecStart = ihp.apps."${pkgs.system}".migrate.program;
        };
        environment = {
            DATABASE_URL = cfg.databaseUrl;
            MINIMUM_REVISION = "${toString cfg.minimumRevision}";
            IHP_MIGRATION_DIR = "${cfg.migrations}/";
        };
    };
}

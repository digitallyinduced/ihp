{ config, pkgs, ihp, ... }:
let cfg = config.services.ihp;
in
{
    systemd.services.migrate =
        let migrateApp = pkgs.stdenv.mkDerivation {
                name = "migrate-app";
                src = cfg.migrations;
                buildPhase = ''
                    mkdir -p $out/Application/Migration
                    find "$src" -mindepth 1 -type f -exec cp {} $out/Application/Migration \;
                '';
            };
        in {
            serviceConfig = {
                Type = "oneshot";
            };
            script = ''
                cd ${migrateApp}
                ${ihp.apps.x86_64-linux.migrate.program}
            '';
            environment = {
                DATABASE_URL = cfg.databaseUrl;
                MINIMUM_REVISION = "${toString cfg.minimumRevision}";
            };
    };
}
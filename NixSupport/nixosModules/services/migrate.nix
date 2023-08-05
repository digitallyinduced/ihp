{ config, pkgs, ... }:
let cfg = config.services.ihp;
in
{
    systemd.services.migrate =
        let migrateApp = pkgs.stdenv.mkDerivation {
                name = "migrate-app";
                src = cfg.migrations;
                buildPhase = ''
                    mkdir -p $out/Application/Migration
                    cp $src/* $out/Application/Migration
                '';
            };
        in {
        serviceConfig = {
            Type = "oneshot";
        };
        script = ''
            cd ${migrateApp}
            ${self.packages.x86_64-linux.migrate}/bin/migrate
        '';
        environment = {
            DATABASE_URL = cfg.databaseUrl;
            MINIMUM_REVISION = "${cfg.minimumRevision}";
        };
    };
}
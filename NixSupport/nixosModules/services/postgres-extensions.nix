{ config, pkgs, lib, ... }:
let
    cfg = config.services.ihp;

    # Extension names referenced by CREATE EXTENSION statements in Application/Schema.sql
    schemaExtensions =
        let
            matchLine = line: builtins.match ''[[:space:]]*[cC][rR][eE][aA][tT][eE][[:space:]]+[eE][xX][tT][eE][nN][sS][iI][oO][nN]([[:space:]]+[iI][fF][[:space:]]+[nN][oO][tT][[:space:]]+[eE][xX][iI][sS][tT][sS])?[[:space:]]+["']?([a-zA-Z0-9_-]+).*'' line;
            extensionsInLine = line: let match = matchLine line; in if match == null then [] else [ (builtins.elemAt match 1) ];
        in
            lib.unique (lib.concatMap extensionsInLine (lib.splitString "\n" (builtins.readFile cfg.schema)));

    undeclaredExtensions = lib.subtractLists cfg.postgresExtensions schemaExtensions;
in
{
    assertions = [
        {
            assertion = undeclaredExtensions == [];
            message = ''
                Application/Schema.sql uses PostgreSQL extensions that are not declared in your NixOS configuration: ${lib.concatStringsSep ", " undeclaredExtensions}

                In production the schema and migrations run as the app database user, which cannot create most extensions. Declare them in your deployment configuration:

                    services.ihp.postgresExtensions = [ ${lib.concatMapStringsSep " " (extension: ''"${extension}"'') schemaExtensions} ];

                IHP then creates the extensions as the postgres superuser before migrations run.
            '';
        }
    ];

    systemd.services.postgres-extensions = lib.mkIf (cfg.postgresExtensions != []) {
        description = "Provision PostgreSQL Extensions for IHP App";
        wantedBy = [ "multi-user.target" ];
        # 'systemctl start migrate' (run by deploy-to-nixos) pulls this in first.
        # The requires-symlink is inert when migrate.service doesn't exist.
        requiredBy = [ "migrate.service" ];
        before = [ "migrate.service" ];
        # On nixpkgs >= 25.11 initialScript/ensureDatabases run in postgresql-setup.service;
        # on older nixpkgs that unit doesn't exist and wants/after on it are ignored.
        after = [ "postgresql.service" "postgresql-setup.service" ];
        requires = [ "postgresql.service" ];
        wants = [ "postgresql-setup.service" ];
        path = [ config.services.postgresql.package ];
        environment.PGPORT = builtins.toString config.services.postgresql.settings.port;
        serviceConfig = {
            Type = "oneshot";
            User = "postgres";
            RemainAfterExit = true;
        };
        script = lib.concatMapStringsSep "\n" (extension:
            ''psql --dbname ${lib.escapeShellArg cfg.databaseName} --command ${lib.escapeShellArg ''CREATE EXTENSION IF NOT EXISTS "${extension}"''}''
        ) cfg.postgresExtensions;
    };
}

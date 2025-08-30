# Running IHP app + a local Postgres connected to it
{ config, nixpkgs, pkgs, modulesPath, lib, ihp, ... }:
let cfg = config.services.ihp;
in
{
    imports = [
        ihp.nixosModules.options
        ihp.nixosModules.binaryCache
        ihp.nixosModules.services_app
        ihp.nixosModules.services_worker
        ihp.nixosModules.services_migrate
    ];

    # Pin the nixpkgs to the IHP nixpkgs
    nix.registry.nixpkgs.flake = nixpkgs;
    
    # Add swap to avoid running out of memory during builds
    swapDevices = [ { device = "/swapfile"; size = 8192; } ];

    # Vim and psql commands are helpful when accessing the server
    environment.systemPackages = with pkgs; [ vim postgresql ];
    programs.vim.defaultEditor = true;

    # Allow public access
    networking.firewall.enable = true;
    networking.firewall.allowedTCPPorts = [ 80 22 ];

    # Enable Letsencrypt
    # TODO security.acme.defaults.email = email;
    security.acme.acceptTerms = true;

    # Add a loadbalancer
    services.nginx = {
        enable = true;
        enableReload = true;
        recommendedProxySettings = true;
        recommendedGzipSettings = true;
        recommendedOptimisation = true;
        recommendedTlsSettings = true;
    };

    # Setup the domain
    services.nginx.virtualHosts = {
        "${cfg.domain}" = {
            serverAliases = [ ];
            enableACME = cfg.httpsEnabled;
            forceSSL = cfg.httpsEnabled;
            locations = {
                "/" = {
                    proxyPass = "http://localhost:8000";
                    proxyWebsockets = true;
                    extraConfig =
                    # required when the target is also TLS server with multiple hosts
                    "proxy_ssl_server_name on;" +
                    # required when the server wants to use HTTP Authentication
                    "proxy_pass_header Authorization;";
                };
            };
        };
    };

    # Postgres
    services.postgresql = {
        enable = true;
        initialScript = pkgs.writeText "ihp-initScript" ''
            CREATE USER ${cfg.databaseUser};
            CREATE DATABASE ${cfg.databaseName} OWNER ${cfg.databaseUser};
            GRANT ALL PRIVILEGES ON DATABASE ${cfg.databaseName} TO "${cfg.databaseUser}";
            \connect ${cfg.databaseName}
            SET ROLE '${cfg.databaseUser}';
            CREATE TABLE IF NOT EXISTS schema_migrations (revision BIGINT NOT NULL UNIQUE);
            \i ${ihp}/IHPSchema.sql
            \i ${cfg.schema}
            \i ${cfg.fixtures}
        '';
    };

    services.ihp.databaseUser = "root";
    services.ihp.databaseUrl = "postgresql://${cfg.databaseUser}@/${cfg.databaseName}";

    # Enable automatic GC to avoid the disk from filling up
    #
    # https://github.com/digitallyinduced/ihp/pull/1792#pullrequestreview-1570755863
    #
    # " It's was a recurring problem on Shipnix that people ran out of disk space and the database service crashed without this"
    nix.gc = {
        automatic = true;
        dates = "weekly";
        options = "--delete-older-than 30d";
    };

    # Saves disk space by detecting and handling identical contents in the Nix Store
    nix.settings.auto-optimise-store = true;

    environment.variables = {
        PGUSER = cfg.databaseUser;
        PGDATABASE = cfg.databaseName;
    };
}


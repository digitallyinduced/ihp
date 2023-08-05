# Running IHP app + a local Postgres connected to it
{ config, pkgs, modulesPath, lib, ihp, ... }:
let cfg = config.services.ihp;
in
{
    imports = [
        ihp.nixosModules.options
        ihp.nixosModules.services_app
        ihp.nixosModules.services_worker
        ihp.nixosModules.services_migrate
    ];

    # Speed up builds with the IHP binary cache
    nix.settings.substituters = [ "https://digitallyinduced.cachix.org" ];
    nix.settings.trusted-public-keys = [ "digitallyinduced.cachix.org-1:y+wQvrnxQ+PdEsCt91rmvv39qRCYzEgGQaldK26hCKE=" ];
    
    # Add swap to avoid running out of memory during builds
    swapDevices = [ { device = "/swapfile"; size = 8192; } ];

    # Vim and psql commands are helpful when accessing the server
    environment.systemPackages = with pkgs; [ vim postgresql ];
    programs.vim.defaultEditor = true;

    system.stateVersion = "23.05";

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
        ensureDatabases = [ cfg.databaseName ];
        ensureUsers = [
            {
                name = cfg.databaseUser;
                ensurePermissions = {
                    "DATABASE ${cfg.databaseName}" = "ALL PRIVILEGES";
                };
            }
        ];
        initialScript = pkgs.writeText "ihp-initScript" ''
            CREATE TABLE IF NOT EXISTS schema_migrations (revision BIGINT NOT NULL UNIQUE);
            \i ${ihp}/lib/IHP/IHPSchema.sql
            \i ${cfg.schema}
            \i ${cfg.fixtures}
        '';
    };

    services.ihp.databaseUrl = ""; # TODO: Set this to some real value
}


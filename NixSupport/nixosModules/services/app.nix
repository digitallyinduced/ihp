{ config, pkgs, modulesPath, lib, self, ... }:
let
    cfg = config.services.ihp;
    ihpApp = self.packages.x86_64-linux.default;
in
{
    systemd.services.app = {
        description = "IHP App";
        enable = true;
        after = [ "network.target" ];
        wantedBy = [ "multi-user.target" ];
        serviceConfig = {
            Type = "simple";
            Restart = "always";
            WorkingDirectory = "${ihpApp}/lib";
            ExecStart = "${ihpApp}/bin/RunProdServer";
        };
        environment =
            let
                defaultEnv = {
                    PORT = "${toString cfg.appPort}";
                    IHP_ENV = cfg.ihpEnv;
                    IHP_BASEURL = cfg.baseUrl;
                    IHP_REQUEST_LOGGER_IP_ADDR_SOURCE = cfg.requestLoggerIPAddrSource;
                    DATABASE_URL = cfg.databaseUrl;
                    IHP_SESSION_SECRET = cfg.sessionSecret;
                };
            in
                defaultEnv // cfg.additionalEnvVars;
    };
}
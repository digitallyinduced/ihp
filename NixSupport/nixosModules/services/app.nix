{ config, pkgs, modulesPath, lib, self, ... }:
let
    cfg = config.services.ihp;
in
{
    systemd.services.app = {
        description = "IHP App";
        enable = true;
        after = [ "network.target" "app.socket" "app-keygen.service" ];
        wantedBy = [ "multi-user.target" ];
        serviceConfig = {
            Type = "notify";
            Restart = "always";
            ExecStart = "${cfg.package}/bin/RunProdServer";
            KillSignal = "SIGINT";
            WatchdogSec = "60";
            Sockets = "app.socket";
        };
        environment =
            let
                defaultEnv = {
                    PORT = "${toString cfg.appPort}";
                    IHP_ENV = cfg.ihpEnv;
                    IHP_BASEURL = cfg.baseUrl;
                    IHP_REQUEST_LOGGER_IP_ADDR_SOURCE = cfg.requestLoggerIPAddrSource;
                    DATABASE_URL = cfg.databaseUrl;
                    IHP_SESSION_SECRET_FILE = cfg.sessionSecretFile;
                    GHCRTS = cfg.rtsFlags;
                    IHP_SYSTEMD = "1";
                };
            in
                defaultEnv // cfg.additionalEnvVars;
    };
    systemd.sockets.app = {
        wantedBy = [ "sockets.target" ];
        socketConfig = {
            ListenStream = "${toString cfg.appPort}";
            Accept = "no";
        };
    };
}

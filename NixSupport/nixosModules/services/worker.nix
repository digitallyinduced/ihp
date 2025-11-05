{ config, pkgs, self, lib, ... }:
let
    cfg = config.services.ihp;
in
{
    systemd.services.worker = {
        enable = true;
        after = [ "network.target" "app-keygen.service" ];
        wantedBy = [ "multi-user.target" ];
        serviceConfig = {
            Type = "simple";
            Restart = "always";
            WorkingDirectory = "${cfg.package}/lib";
            ExecStart = "${cfg.package}/bin/RunJobs";
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
                };
            in
                defaultEnv // cfg.additionalEnvVars;
    };
}
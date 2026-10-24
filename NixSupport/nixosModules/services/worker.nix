{ config, pkgs, self, lib, ... }:
let
    cfg = config.services.ihp;
in
{
    systemd.services.worker = {
        # Apps without jobs get no RunJobs binary. A package built outside
        # IHP's builder has no runJobsBinary attribute, so keep the worker there.
        enable = !(cfg.package ? runJobsBinary) || cfg.package.runJobsBinary != null;
        after = [ "network.target" "app-keygen.service" ];
        wantedBy = [ "multi-user.target" ];
        serviceConfig = {
            Type = "simple";
            Restart = "always";
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
{ config, pkgs, modulesPath, lib, self, ... }:
let
    cfg = config.services.ihp;
    openssl = "${pkgs.openssl}/bin/openssl";
    base64 = "${pkgs.coreutils}/bin/base64";
in
{
    systemd.services.app-keygen = {
        description = "App Session Key Generation";
        wantedBy = [ "multi-user.target" ];
        before = [ "app.service" ];
        script = ''
            mkdir -p "$(dirname "${cfg.sessionSecretFile}")"
            
            if [ -n "${cfg.sessionSecret or ""}" ]; then
                # If sessionSecret is set, decode and write it to the file
                echo "${cfg.sessionSecret}" | ${base64} -d > "${cfg.sessionSecretFile}"
            elif [ ! -f "${cfg.sessionSecretFile}" ]; then
                # If sessionSecret is not set, generate a new secret
                ${openssl} rand 96 > "${cfg.sessionSecretFile}"
            fi
            
            chmod 600 "${cfg.sessionSecretFile}"
        '';
        serviceConfig.Type = "oneshot";
    };
}
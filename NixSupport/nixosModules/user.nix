# The system user the IHP services run as
{ config, options, lib, ... }:
let
    cfg = config.services.ihp;
    # A session secret somewhere else is provided by the operator, e.g. rendered
    # by a secret manager, so only the default directory is created here.
    ownsSessionSecretDir = cfg.sessionSecretFile == options.services.ihp.sessionSecretFile.default;
in
{
    users.users = lib.mkIf (cfg.user != "root") {
        "${cfg.user}" = {
            isSystemUser = true;
            group = cfg.group;
            description = "IHP app";
        };
    };

    users.groups = lib.mkIf (cfg.group != "root") {
        "${cfg.group}" = {};
    };

    # The services run unprivileged, so they can neither create the directory nor
    # read a secret that belongs to root.
    systemd.tmpfiles.rules =
        lib.optional ownsSessionSecretDir "d ${builtins.dirOf cfg.sessionSecretFile} 0750 ${cfg.user} ${cfg.group} -"
        ++ [ "z ${cfg.sessionSecretFile} 0600 ${cfg.user} ${cfg.group} -" ];
}

{ config, lib, ... }:
let
    cfg = config.services.ihp.ghcBuildCache;
in
{
    options.services.ihp.ghcBuildCache = {
        enable = lib.mkEnableOption ''
            the unsafe host-local GHC intermediates cache for IHP production builds
        '';

        cacheDirectory = lib.mkOption {
            type = lib.types.addCheck lib.types.str (path: lib.hasPrefix "/" path);
            default = "/var/cache/ihp-ghc";
            description = ''
                Absolute path to the mutable cache directory on the build host.
                This must match ihp.incrementalProductionBuild.cacheDirectory.
            '';
        };

        owner = lib.mkOption {
            type = lib.types.str;
            default = "root";
            description = "Owner of the GHC intermediates cache directory.";
        };

        group = lib.mkOption {
            type = lib.types.str;
            default = "nixbld";
            description = "Group allowed to read and write the GHC intermediates cache.";
        };
    };

    config = lib.mkIf cfg.enable {
        systemd.tmpfiles.rules = [
            "d ${cfg.cacheDirectory} 2770 ${cfg.owner} ${cfg.group} -"
        ];

        # sandbox-paths are read-only, so the cached app-library derivation
        # carries __noChroot. In relaxed mode all other ordinary derivations
        # remain sandboxed.
        nix.settings.sandbox = "relaxed";
        nix.settings.system-features = lib.mkAfter [ "ihp-ghc-cache" ];
    };
}

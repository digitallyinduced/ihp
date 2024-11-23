# Running an IHP web server + Worker
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
        ihp.nixosModules.services_appKeygen
    ];

    # Pin the nixpkgs to the IHP nixpkgs
    nix.registry.nixpkgs.flake = nixpkgs;
}


# Running an IHP web server + Worker
{ config, pkgs, modulesPath, lib, ihp, ihpApp, ... }:
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

    # Pin the nixpkgs to the IHP nixpkgs
    nix.registry.nixpkgs.flake = nixpkgs;

    system.stateVersion = "23.05";
}


# Configures the IHP binary cache
{ config, nixpkgs, pkgs, modulesPath, lib, ihp, ... }:
{
    # Speed up builds with the IHP binary cache
    nix.settings.substituters = lib.mkAfter [ "https://digitallyinduced.cachix.org" ];
    nix.settings.trusted-public-keys = lib.mkAfter[ "digitallyinduced.cachix.org-1:y+wQvrnxQ+PdEsCt91rmvv39qRCYzEgGQaldK26hCKE=" ];
}


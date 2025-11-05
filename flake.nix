{
    description = "IHP is a modern batteries-included haskell web framework, built on top of Haskell and Nix.";

    inputs = {
        # "github:NixOS/nixpkgs/nixos-unstable"
        nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";

        # pre-defined set of default target systems
        systems.url = "github:nix-systems/default";

        # a module framework for flakes
        flake-parts.url = "github:hercules-ci/flake-parts";
        flake-parts.inputs.nixpkgs-lib.follows = "nixpkgs";

        # used for setting up development environments
        devenv.url = "github:cachix/devenv/v1.10";
        devenv.inputs.nixpkgs.follows = "nixpkgs";

        # TODO use a corresponding release branch
        # import ihp-boilerplate for the templates
        ihp-boilerplate.url = "github:digitallyinduced/ihp-boilerplate";

        nix-filter.url = "github:numtide/nix-filter";

        devenv-root = {
            url = "file+file:///dev/null";
            flake = false;
        };
    };

    outputs = inputs: inputs.flake-parts.lib.mkFlake { inherit inputs; } (
        { self, flake-parts-lib, withSystem, ... } : {
            systems = import inputs.systems;
            imports = [
                inputs.devenv.flakeModule
                (flake-parts-lib.importApply ./devenv-module.nix { inherit inputs self; })
            ];

            flake = {
                overlays.default = import ./NixSupport/overlay.nix { inherit inputs self; };
                flakeModules.default = flake-parts-lib.importApply ./flake-module.nix { inherit inputs; };
                templates.default = {
                    path = inputs.ihp-boilerplate;
                    description = "Template for an IHP project";
                    welcomeText = ''
                        TODO this is shown when running nix init, could contain instruction to get started
                    '';
                };
                nixosModules = {
                    app = ./NixSupport/nixosModules/app.nix;
                    appWithPostgres = ./NixSupport/nixosModules/appWithPostgres.nix;

                    services_app = ./NixSupport/nixosModules/services/app.nix;
                    services_worker = ./NixSupport/nixosModules/services/worker.nix;
                    services_migrate = ./NixSupport/nixosModules/services/migrate.nix;
                    services_loadSchema = ./NixSupport/nixosModules/services/loadSchema.nix;
                    services_appKeygen = ./NixSupport/nixosModules/services/app-keygen.nix;
                    options = ./NixSupport/nixosModules/options.nix;
                    binaryCache = ./NixSupport/nixosModules/binaryCache.nix;
                };
                nix-ci.impure = true;
            };
        }
    );
    
    nixConfig = {
        extra-trusted-public-keys = "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw= cachix.cachix.org-1:eWNHQldwUO7G2VkjpnjDbWwy4KQ/HNxht7H4SSoMckM= digitallyinduced.cachix.org-1:y+wQvrnxQ+PdEsCt91rmvv39qRCYzEgGQaldK26hCKE=";
        extra-substituters = "https://devenv.cachix.org https://cachix.cachix.org https://digitallyinduced.cachix.org";
    };

}

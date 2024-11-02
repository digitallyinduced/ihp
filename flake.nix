{
    description = "IHP is a modern batteries-included haskell web framework, built on top of Haskell and Nix.";

    inputs = {
        nixpkgs.url = "github:NixOS/nixpkgs/haskell-updates";

        # pre-defined set of default target systems
        systems.url = "github:nix-systems/default";

        # a module framework for flakes
        flake-parts.url = "github:hercules-ci/flake-parts";
        flake-parts.inputs.nixpkgs-lib.follows = "nixpkgs";

        # used for setting up development environments
        devenv.url = "github:cachix/devenv?rev=6e318854efa95c5e67a1152547f838754e8f0306";
        devenv.inputs.nixpkgs.follows = "nixpkgs";

        # TODO use a corresponding release branch
        # import ihp-boilerplate for the templates
        ihp-boilerplate.url = "github:digitallyinduced/ihp-boilerplate";

        nix-filter.url = "github:numtide/nix-filter";
    };

    outputs = inputs: inputs.flake-parts.lib.mkFlake { inherit inputs; } (
        { flake-parts-lib, withSystem, ... } : {
            systems = import inputs.systems;
            imports = [
                inputs.devenv.flakeModule
                (flake-parts-lib.importApply ./devenv-module.nix { inherit inputs; })
            ];

            flake = {
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
                    options = ./NixSupport/nixosModules/options.nix;
                    binaryCache = ./NixSupport/nixosModules/binaryCache.nix;
                };
                nix-ci.impure = true;
            };
        }
    );


}

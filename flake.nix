{
    description = "IHP is a modern batteries-included haskell web framework, built on top of Haskell and Nix.";

    inputs = {
        # TODO use nixpkgs-unstable and just .lock a version?
        # Waiting for https://github.com/NixOS/nixpkgs/pull/296909
        nixpkgs.url = "github:mpscholten/nixpkgs?rev=f64c488137efa6aab7ad07921bd7c7483c2a35c8";

        # pre-defined set of default target systems
        systems.url = "github:nix-systems/default";

        # a module framework for flakes
        flake-parts.url = "github:hercules-ci/flake-parts";

        # used for setting up development environments
        devenv.url = "github:cachix/devenv";
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
            };
        }
    );

}

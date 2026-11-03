{
    description = "IHP is a modern batteries-included haskell web framework, built on top of Haskell and Nix.";

    inputs = {
        nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";             # for Haskell packages
        nixpkgs-nixos.url = "github:NixOS/nixpkgs/nixos-26.05";     # for NixOS deployments (pin independently)

        hasql-source = {
            url = "github:nikita-volkov/hasql/ef3e361ced8ad62652f1a8e7a77b3d0d0e2b9065";
            flake = false;
        };
        hasql-pool-source = {
            url = "github:nikita-volkov/hasql-pool/50b49864afc796fd7df6e08706d3a628c656dcf0";
            flake = false;
        };
        hasql-transaction-source = {
            url = "github:nikita-volkov/hasql-transaction/81adf1ad3d8cc60d64a1354eda58d4c46b705532";
            flake = false;
        };
        hasql-dynamic-statements-source = {
            url = "github:nikita-volkov/hasql-dynamic-statements/bfcf8159ab2f21080a672336d1c14cf83db51856";
            flake = false;
        };
        hasql-implicits-source = {
            url = "github:nikita-volkov/hasql-implicits/cb6e0c7f99dbd85b64409e1ea267b68d49eeb5b2";
            flake = false;
        };
        hasql-notifications-source = {
            url = "github:nikita-volkov/hasql-notifications/e57de3cc9f9efe7d3ad66e6c5d386c953ab9a3bc";
            flake = false;
        };
        pqi-source = {
            url = "github:nikita-volkov/pqi/e8484ed4a442fec9b5274d116f6816f89db7c0eb";
            flake = false;
        };
        pqi-native-source = {
            url = "github:nikita-volkov/pqi-native/f887ca3776ec5910a4ceb8825e71e7a57c379708";
            flake = false;
        };
        pqi-ffi-source = {
            url = "github:nikita-volkov/pqi-ffi/1ca01c1487f94c68e4cb2793b2193bb59f13727e";
            flake = false;
        };

        # pre-defined set of default target systems
        systems.url = "github:nix-systems/default";

        # a module framework for flakes
        flake-parts.url = "github:hercules-ci/flake-parts";
        flake-parts.inputs.nixpkgs-lib.follows = "nixpkgs";

        # used for setting up development environments
        devenv.url = "github:cachix/devenv";
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
                    path = /. + builtins.unsafeDiscardStringContext (toString inputs.ihp-boilerplate);
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
            };
        }
    );
    
    nixConfig = {
        extra-trusted-public-keys = "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw= cachix.cachix.org-1:eWNHQldwUO7G2VkjpnjDbWwy4KQ/HNxht7H4SSoMckM= digitallyinduced.cachix.org-1:y+wQvrnxQ+PdEsCt91rmvv39qRCYzEgGQaldK26hCKE=";
        extra-substituters = "https://devenv.cachix.org https://cachix.cachix.org https://digitallyinduced.cachix.org";
    };

}

{
    description = "IHP is a modern batteries-included haskell web framework, built on top of Haskell and Nix.";

    inputs = {
        # TEMPORARY PIN — not the nixpkgs-unstable channel branch.
        #
        # Pinned to a staging-next commit that contains NixOS/nixpkgs#519795
        # ("haskellPackages.ihp: pin to hasql 1.10"), which adds the upstream
        # IHP hasql-1.10 scope + dontCheck for the versioned hasql attrs.
        # Hydra has already built haskellPackages.ihp green at this rev, so the
        # entire hasql 1.10 closure (+ GHC + the Haskell dep tree) is prebuilt
        # in cache.nixos.org for aarch64-darwin / x86_64-linux / aarch64-linux.
        # That lets NixSupport/overlay.nix drop its local dontCheck workarounds
        # and get binary-cache hits instead of rebuilding the stack from source.
        #
        # Revert to "github:NixOS/nixpkgs/nixpkgs-unstable" once #519795 has
        # propagated through staging-next -> master -> the nixpkgs-unstable
        # channel (tracked by NixOS/nixpkgs#517946).
        nixpkgs.url = "github:NixOS/nixpkgs/23efd5e6fcd2ebf02486146f141d5348449694ab";      # for Haskell packages
        nixpkgs-nixos.url = "github:NixOS/nixpkgs/nixos-25.11";     # for NixOS deployments (pin independently)

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

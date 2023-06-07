{
    description = "IHP is a modern batteries-included haskell web framework, built on top of Haskell and Nix.";

    inputs = {
        # TODO use nixpkgs-unstable and just .lock a version
        nixpkgs.url = "github:NixOS/nixpkgs?rev=a95ed9fe764c3ba2bf2d2fa223012c379cd6b32e";

        # pre-defined set of default target systems
        systems.url = "github:nix-systems/default";

        # a module framework for flakes
        flake-parts.url = "github:hercules-ci/flake-parts";

        # used for setting up development environments
        devenv.url = "github:cachix/devenv";
        devenv.inputs.nixpkgs.follows = "nixpkgs";

        # import ihp-boilerplate for the templates
        ihp-boilerplate.url = "github:digitallyinduced/ihp-boilerplate/nicolas/flake";  # TODO branch
        ihp-boilerplate.flake = false;

        # required for devenv containers, which we currently manually disable
        # nix2container.url = "github:nlewo/nix2container";
        # nix2container.inputs.nixpkgs.follows = "nixpkgs";
        # mk-shell-bin.url = "github:rrbutani/nix-mk-shell-bin";

        /*
        TODO
        it might be interesting to use haskell-flake or something similar to build
        packages directly from the cabal file so there's a single source of truth and we don't
        have to parallely maintain a .nix and a .cabal file
        */
        # haskell-flake.url = "github:srid/haskell-flake";
    };

  outputs = inputs: inputs.flake-parts.lib.mkFlake { inherit inputs; } (
    { flake-parts-lib, withSystem, ... } : {
      systems = import inputs.systems;
      imports = [
        inputs.devenv.flakeModule
        ./devenv-module.nix
      ];

      flake = {
        flakeModules.default = flake-parts-lib.importApply ./flake-module.nix { inherit withSystem inputs; };
        templates.default = {
          path = inputs.ihp-boilerplate;
          description = "Template for an IHP project";
          welcomeText = ''
            TODO this is shown when running nix init, could contain instruction to get started
          '';
        };
      };
    }
  );

}

{
    inputs = {
        nixpkgs.url = "github:NixOS/nixpkgs";
        devenv.url = "github:cachix/devenv";
    };

    outputs = inputs@{ flake-parts, nixpkgs, ... }:
        flake-parts.lib.mkFlake { inherit inputs; } {
            imports = [ inputs.devenv.flakeModule ];
            systems = nixpkgs.lib.systems.flakeExposed;

            perSystem = { config, self', inputs', pkgs, system, ... }: {
                packages.default = pkgs.stdenv.mkDerivation {
                    name = "ihp-guide";
                    src = ./.;
                    nativeBuildInputs = with pkgs; [ haskellPackages.mmark-cli nodejs-18_x curl cacert ];
                    buildPhase = ''
                        # See https://github.com/svanderburg/node2nix/issues/217#issuecomment-751311272
                        export HOME=/tmp

                        LANG=en_US.UTF-8 make guide.tar.gz
                    '';
                    installPhase = ''
                        mv guide.tar.gz $out
                    '';
                };

                devenv.shells.default = {
                    process.implementation = "overmind";
                    packages = with pkgs; [
                        haskellPackages.wai-app-static
                        entr
                        haskellPackages.mmark-cli
                        nodejs-18_x
                    ];

                    enterShell = ''echo "Server is starting at: http://localhost:3000/index.html"'';

                    processes.webServer.exec = "warp";
                    processes.fileWatcher.exec = "ls *.markdown layout.html | entr make";
                };
            };
        };
}
{
    inputs = {
        nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
        devenv.url = "github:cachix/devenv/v1.8.2";
    };

    outputs = inputs@{ flake-parts, nixpkgs, ... }:
        flake-parts.lib.mkFlake { inherit inputs; } {
            imports = [ inputs.devenv.flakeModule ];
            systems = nixpkgs.lib.systems.flakeExposed;

            perSystem = { config, self', inputs', pkgs, system, ... }: {
                packages.default = pkgs.stdenv.mkDerivation {
                    name = "ihp-guide";
                    src = ./.;
                    nativeBuildInputs = with pkgs; [ haskellPackages.mmark-cli nodejs curl cacert ];
                    buildPhase = ''
                        # See https://github.com/svanderburg/node2nix/issues/217#issuecomment-751311272
                        export HOME=/tmp

                        make guide.tar.gz
                    '';
                    installPhase = ''
                        mv guide.tar.gz $out
                    '';#
                    LOCALE_ARCHIVE = pkgs.lib.optionalString pkgs.stdenv.isLinux "${pkgs.glibcLocales}/lib/locale/locale-archive";
                    LANG = "en_US.UTF-8";
                };

                devenv.shells.default = {
                    process.manager.implementation = "overmind";
                    packages = with pkgs; [
                        haskellPackages.wai-app-static
                        entr
                        haskellPackages.mmark-cli
                        nodejs
                    ];

                    enterShell = ''echo "Server is starting at: http://localhost:3000/index.html"'';

                    processes.webServer.exec = "warp";
                    processes.fileWatcher.exec = "ls *.markdown layout.html | entr make";
                };
            };
        };
}